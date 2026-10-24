//! microWakeWord: TFLite micro frontend features -> streaming TFLite model.

use std::collections::VecDeque;
use std::ffi::{CString, c_char, c_void};
use std::mem;
use std::path::Path;
use std::ptr;
use std::ptr::NonNull;

const STEP_SAMPLES: usize = 160; // 10 ms at 16 kHz
const FEATURES: usize = 40;
const FEATURE_SCALE: f32 = 0.039_062_5;
const TFLITE_OK: i32 = 0;
const TFLITE_UINT8: i32 = 3;
const TFLITE_INT8: i32 = 9;

#[repr(C)]
#[derive(Clone, Copy)]
struct QuantParams {
    scale: f32,
    zero_point: i32,
}

// Subset of the stable TFLite C API (the vendored library ships no headers)
// and of csrc/frontend_shim.cc.
unsafe extern "C" {
    fn TfLiteModelCreateFromFile(path: *const c_char) -> *mut c_void;
    fn TfLiteModelDelete(model: *mut c_void);
    fn TfLiteInterpreterCreate(model: *const c_void, opts: *const c_void) -> *mut c_void;
    fn TfLiteInterpreterDelete(interp: *mut c_void);
    fn TfLiteInterpreterAllocateTensors(interp: *mut c_void) -> i32;
    fn TfLiteInterpreterInvoke(interp: *mut c_void) -> i32;
    fn TfLiteInterpreterGetInputTensor(interp: *const c_void, idx: i32) -> *mut c_void;
    fn TfLiteInterpreterGetOutputTensor(interp: *const c_void, idx: i32) -> *const c_void;
    fn TfLiteTensorDim(t: *const c_void, dim: i32) -> i32;
    fn TfLiteTensorByteSize(t: *const c_void) -> usize;
    fn TfLiteTensorType(t: *const c_void) -> i32;
    fn TfLiteTensorQuantizationParams(t: *const c_void) -> QuantParams;
    fn TfLiteTensorCopyFromBuffer(t: *mut c_void, data: *const c_void, len: usize) -> i32;
    fn TfLiteTensorCopyToBuffer(t: *const c_void, data: *mut c_void, len: usize) -> i32;

    fn mf_create() -> *mut c_void;
    fn mf_process(
        h: *mut c_void,
        samples: *const i16,
        n: usize,
        read: *mut usize,
        out: *mut u16,
        out_len: usize,
    ) -> usize;
    fn mf_free(h: *mut c_void);
}

/// Owned TFLite model; interpreters borrow it.
struct Model(NonNull<c_void>);

impl Model {
    fn load(path: &Path) -> Result<Self, String> {
        let c = CString::new(path.as_os_str().as_encoded_bytes()).map_err(|e| e.to_string())?;
        // SAFETY: `c` is a valid NUL-terminated path for the duration of the call.
        let raw = unsafe { TfLiteModelCreateFromFile(c.as_ptr()) };
        NonNull::new(raw)
            .map(Model)
            .ok_or_else(|| format!("cannot load wake word model {}", path.display()))
    }
}

impl Drop for Model {
    fn drop(&mut self) {
        // SAFETY: created by TfLiteModelCreateFromFile, deleted exactly once; every
        // interpreter using it is dropped first (field order in `WakeWord`).
        unsafe { TfLiteModelDelete(self.0.as_ptr()) };
    }
}

/// Element type of a quantized tensor.
#[derive(Clone, Copy)]
enum QuantType {
    Int8,
    UInt8,
}

impl QuantType {
    fn from_tflite(t: i32) -> Result<Self, String> {
        match t {
            TFLITE_INT8 => Ok(QuantType::Int8),
            TFLITE_UINT8 => Ok(QuantType::UInt8),
            other => Err(format!(
                "unsupported tensor type {other}; expected int8 or uint8"
            )),
        }
    }
}

/// Owned raw interpreter handle.
struct RawInterp(NonNull<c_void>);

impl Drop for RawInterp {
    fn drop(&mut self) {
        // SAFETY: created by TfLiteInterpreterCreate and deleted exactly once.
        unsafe { TfLiteInterpreterDelete(self.0.as_ptr()) };
    }
}

/// Interpreter with validated tensor geometry. The streaming model keeps
/// internal state, so a fresh interpreter is the only way to forget audio.
struct Interpreter {
    // Tensor pointers are owned by `raw` and must not outlive it.
    input: NonNull<c_void>,
    output: NonNull<c_void>,
    raw: RawInterp,
    in_q: QuantParams,
    out_q: QuantParams,
    in_type: QuantType,
    out_type: QuantType,
    /// Feature frames per inference.
    stride: usize,
}

impl Interpreter {
    fn new(model: &Model) -> Result<Self, String> {
        // SAFETY: the model pointer is valid and outlives the interpreter.
        let raw = unsafe { TfLiteInterpreterCreate(model.0.as_ptr(), ptr::null()) };
        let raw = RawInterp(NonNull::new(raw).ok_or("cannot create TFLite interpreter")?);
        let r = raw.0.as_ptr();
        // SAFETY: `r` is a live interpreter; the tensor pointers it returns stay
        // valid until it is deleted, and `raw` frees it on every error path.
        let (input, output, in_type, out_type, in_q, out_q, stride, in_bytes, out_bytes) = unsafe {
            if TfLiteInterpreterAllocateTensors(r) != TFLITE_OK {
                return Err("cannot allocate TFLite tensors".into());
            }
            let input = NonNull::new(TfLiteInterpreterGetInputTensor(r, 0))
                .ok_or("model has no input tensor")?;
            let output = NonNull::new(TfLiteInterpreterGetOutputTensor(r, 0).cast_mut())
                .ok_or("model has no output tensor")?;
            let (i, o) = (input.as_ptr(), output.as_ptr());
            (
                input,
                output,
                QuantType::from_tflite(TfLiteTensorType(i))?,
                QuantType::from_tflite(TfLiteTensorType(o))?,
                TfLiteTensorQuantizationParams(i),
                TfLiteTensorQuantizationParams(o),
                usize::try_from(TfLiteTensorDim(i, 1)).unwrap_or(0),
                TfLiteTensorByteSize(i),
                TfLiteTensorByteSize(o),
            )
        };
        if stride == 0 || in_bytes != stride * FEATURES || out_bytes != 1 {
            return Err(format!(
                "unexpected tensor shapes: stride {stride}, input {in_bytes} bytes, output {out_bytes} bytes"
            ));
        }
        if !(in_q.scale.is_finite() && in_q.scale > 0.0 && out_q.scale.is_finite()) {
            return Err("invalid quantization parameters".into());
        }
        Ok(Interpreter {
            input,
            output,
            raw,
            in_q,
            out_q,
            in_type,
            out_type,
            stride,
        })
    }

    fn quantize(&self, feature: u16) -> u8 {
        let q = (f32::from(feature) * FEATURE_SCALE / self.in_q.scale
            + self.in_q.zero_point as f32)
            .round();
        match self.in_type {
            // Clamped to the target range, so the casts cannot truncate.
            #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
            QuantType::Int8 => (q.clamp(-128.0, 127.0) as i8).cast_unsigned(),
            #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
            QuantType::UInt8 => q.clamp(0.0, 255.0) as u8,
        }
    }

    /// Run one inference over `input` (exactly stride * FEATURES bytes).
    fn invoke(&mut self, input: &[u8]) -> Result<f32, String> {
        let mut raw = 0u8;
        // SAFETY: tensors belong to this live interpreter; sizes were validated in
        // `new` and `input.len()` is checked against them by TFLite.
        let ok = unsafe {
            TfLiteTensorCopyFromBuffer(self.input.as_ptr(), input.as_ptr().cast(), input.len())
                == TFLITE_OK
                && TfLiteInterpreterInvoke(self.raw.0.as_ptr()) == TFLITE_OK
                && TfLiteTensorCopyToBuffer(self.output.as_ptr(), (&raw mut raw).cast(), 1)
                    == TFLITE_OK
        };
        if !ok {
            return Err("wake word inference failed".into());
        }
        let q = match self.out_type {
            QuantType::Int8 => i32::from(raw.cast_signed()),
            QuantType::UInt8 => i32::from(raw),
        };
        Ok((q - self.out_q.zero_point) as f32 * self.out_q.scale)
    }
}

/// Owned audio feature frontend (csrc/frontend_shim.cc).
struct Frontend(NonNull<c_void>);

impl Frontend {
    fn new() -> Result<Self, String> {
        // SAFETY: mf_create has no preconditions.
        NonNull::new(unsafe { mf_create() })
            .map(Frontend)
            .ok_or_else(|| "cannot create audio frontend".into())
    }

    /// Process one step; returns consumed samples and whether `out` was filled.
    fn process(&mut self, samples: &[i16], out: &mut [u16; FEATURES]) -> (usize, bool) {
        let n = samples.len().min(STEP_SAMPLES);
        let mut read = 0;
        // SAFETY: `samples` holds at least `n` samples, `out` has FEATURES slots and
        // the shim never writes more than `out_len`.
        let written = unsafe {
            mf_process(
                self.0.as_ptr(),
                samples.as_ptr(),
                n,
                &raw mut read,
                out.as_mut_ptr(),
                FEATURES,
            )
        };
        (read.min(n), written == FEATURES)
    }
}

impl Drop for Frontend {
    fn drop(&mut self) {
        // SAFETY: created by mf_create and freed exactly once.
        unsafe { mf_free(self.0.as_ptr()) };
    }
}

pub struct WakeWord {
    // Drop order matters: the interpreter must go before the model.
    interp: Interpreter,
    frontend: Frontend,
    model: Model,
    quant: Vec<u8>,
    frames: usize,
    pending: Vec<i16>,
    probs: VecDeque<f32>,
    window: usize,
    threshold: f32,
    /// Highest averaged probability since `take_peak`.
    peak: f32,
}

// SAFETY: the TFLite and frontend handles are only used through `&mut self`
// and have no thread affinity; the detector moves to the worker thread once.
unsafe impl Send for WakeWord {}

impl WakeWord {
    pub fn new(model: &Path, threshold: f32, window: usize) -> Result<Self, String> {
        let model = Model::load(model)?;
        let interp = Interpreter::new(&model)?;
        Ok(WakeWord {
            quant: vec![0; interp.stride * FEATURES],
            interp,
            frontend: Frontend::new()?,
            model,
            frames: 0,
            pending: Vec::new(),
            probs: VecDeque::new(),
            window: window.max(1),
            threshold,
            peak: 0.0,
        })
    }

    /// Feed 16 kHz mono samples; returns the averaged probability if it
    /// crossed the threshold.
    pub fn feed(&mut self, samples: &[i16]) -> Option<f32> {
        self.pending.extend_from_slice(samples);
        let mut off = 0;
        let mut hit = None;
        let mut feat = [0u16; FEATURES];
        while self.pending.len() - off >= STEP_SAMPLES {
            let (read, have) = self.frontend.process(&self.pending[off..], &mut feat);
            // Always advance, or a misbehaving frontend would spin forever.
            off += read.max(1);
            if !have {
                continue;
            }
            let base = self.frames * FEATURES;
            for (dst, &v) in self.quant[base..base + FEATURES].iter_mut().zip(&feat) {
                *dst = self.interp.quantize(v);
            }
            self.frames += 1;
            if self.frames < self.interp.stride {
                continue;
            }
            self.frames = 0;
            match self.interp.invoke(&self.quant) {
                Ok(p) => {
                    if let Some(mean) = self.push_prob(p) {
                        self.peak = self.peak.max(mean);
                        if mean > self.threshold {
                            hit = Some(mean);
                        }
                    }
                }
                Err(e) => eprintln!("voice-assistant: {e}"),
            }
        }
        self.pending.drain(..off.min(self.pending.len()));
        hit
    }

    /// The highest probability seen since the last call, below the
    /// threshold too, for logging near misses.
    pub fn take_peak(&mut self) -> f32 {
        mem::take(&mut self.peak)
    }

    fn push_prob(&mut self, p: f32) -> Option<f32> {
        self.probs.push_back(p);
        while self.probs.len() > self.window {
            self.probs.pop_front();
        }
        (self.probs.len() == self.window)
            .then(|| self.probs.iter().sum::<f32>() / self.probs.len() as f32)
    }

    /// Forget all audio heard so far, including the model's internal state,
    /// so a finished command cannot re-trigger. On failure the old state is
    /// kept and the error returned.
    pub fn reset(&mut self) -> Result<(), String> {
        let interp = Interpreter::new(&self.model)?;
        let frontend = Frontend::new()?;
        self.interp = interp;
        self.frontend = frontend;
        self.probs.clear();
        self.pending.clear();
        self.frames = 0;
        Ok(())
    }
}
