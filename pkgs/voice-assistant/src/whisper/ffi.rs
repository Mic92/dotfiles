//! Safe wrapper over whisper.cpp; every `unsafe` of the crate's speech
//! recognition lives here.

#[allow(
    non_upper_case_globals,
    non_camel_case_types,
    non_snake_case,
    dead_code,
    unsafe_op_in_unsafe_fn,
    clippy::all,
    clippy::pedantic,
    clippy::undocumented_unsafe_blocks
)]
mod sys {
    include!(concat!(env!("OUT_DIR"), "/whisper_sys.rs"));
}

use std::ffi::{CStr, CString, c_char, c_void};
use std::path::Path;
use std::ptr;
use std::ptr::NonNull;
use std::slice;

pub type Token = i32;

/// Silero VAD consumes 32 ms frames at 16 kHz.
pub const VAD_FRAME: usize = 512;
const VAD_FRAME_C: i32 = 512;

unsafe extern "C" fn quiet_log(
    _level: sys::ggml_log_level,
    _text: *const c_char,
    _user: *mut c_void,
) {
}

unsafe extern "C" {
    // ggml loads its CPU/GPU backends as plugins; nothing works before this.
    fn ggml_backend_load_all();
}

pub fn init() {
    // SAFETY: called once at startup before any other whisper/ggml call;
    // `quiet_log` matches ggml_log_callback and ignores its arguments.
    unsafe {
        sys::whisper_log_set(Some(quiet_log), ptr::null_mut());
        ggml_backend_load_all();
    }
}

fn cpath(p: &Path) -> Result<CString, String> {
    CString::new(p.as_os_str().as_encoded_bytes()).map_err(|e| e.to_string())
}

pub fn c_int(n: usize) -> Result<i32, String> {
    i32::try_from(n).map_err(|_| format!("{n} is too large for whisper"))
}

pub struct Vad {
    ctx: NonNull<sys::whisper_vad_context>,
    pending: Vec<f32>,
}

// SAFETY: the context is only used through `&mut self` and has no thread
// affinity; it moves to the worker thread once.
unsafe impl Send for Vad {}

impl Vad {
    pub fn new(model: &Path) -> Result<Self, String> {
        let path = cpath(model)?;
        // SAFETY: `path` is a valid C string for the duration of the call; the
        // params struct is fully initialised by its default constructor.
        let ctx = unsafe {
            let mut params = sys::whisper_vad_default_context_params();
            params.n_threads = 1;
            params.use_gpu = false;
            sys::whisper_vad_init_from_file_with_params(path.as_ptr(), params)
        };
        let ctx = NonNull::new(ctx)
            .ok_or_else(|| format!("cannot load VAD model {}", model.display()))?;
        Ok(Vad {
            ctx,
            pending: Vec::new(),
        })
    }

    pub fn reset(&mut self) {
        self.pending.clear();
        // SAFETY: `ctx` is a live VAD context.
        unsafe { sys::whisper_vad_reset_state(self.ctx.as_ptr()) };
    }

    /// Feed samples; returns one speech probability per completed frame.
    pub fn feed(&mut self, samples: &[f32]) -> Vec<f32> {
        self.pending.extend_from_slice(samples);
        let mut out = Vec::new();
        let frames = self.pending.len() / VAD_FRAME;
        for frame in self.pending.as_chunks::<VAD_FRAME>().0 {
            // SAFETY: `frame` holds exactly VAD_FRAME samples (fits in i32); the
            // probability buffer is owned by the context and has `n` entries.
            unsafe {
                let ctx = self.ctx.as_ptr();
                if !sys::whisper_vad_detect_speech_no_reset(ctx, frame.as_ptr(), VAD_FRAME_C) {
                    continue;
                }
                let n = usize::try_from(sys::whisper_vad_n_probs(ctx)).unwrap_or(0);
                let probs = sys::whisper_vad_probs(ctx);
                if n > 0 && !probs.is_null() {
                    out.push(*probs.add(n - 1));
                }
            }
        }
        self.pending.drain(..frames * VAD_FRAME);
        out
    }
}

impl Drop for Vad {
    fn drop(&mut self) {
        // SAFETY: created by whisper_vad_init_from_file_with_params, freed once.
        unsafe { sys::whisper_vad_free(self.ctx.as_ptr()) };
    }
}

/// What to transcribe with; greedy decoding, no timestamps, one segment.
pub struct FullParams<'a> {
    pub threads: i32,
    pub language: &'a CStr,
    pub prompt: &'a CStr,
    /// Encoder frames to use (50 per second of audio).
    pub audio_ctx: i32,
}

pub struct TokenData {
    pub id: Token,
    pub p: f32,
    pub text: String,
}

pub struct Segment {
    pub no_speech: f32,
    pub tokens: Vec<TokenData>,
}

/// Tokens framing every transcription.
pub struct Special {
    pub prev: Token,
    pub sot: Token,
    pub eot: Token,
    pub transcribe: Token,
    pub no_timestamps: Token,
}

/// Owned whisper context with its default state.
pub struct Context(NonNull<sys::whisper_context>);

// SAFETY: the context is only used through `&self`/`&mut self` and has no
// thread affinity; it moves to the worker thread once.
unsafe impl Send for Context {}

impl Drop for Context {
    fn drop(&mut self) {
        // SAFETY: created by whisper_init_from_file_with_params, freed once.
        unsafe { sys::whisper_free(self.0.as_ptr()) };
    }
}

impl Context {
    pub fn load(model: &Path) -> Result<Self, String> {
        let path = cpath(model)?;
        // SAFETY: `path` is a valid C string; default params are fully initialised.
        let raw = unsafe {
            let mut cparams = sys::whisper_context_default_params();
            cparams.use_gpu = false;
            sys::whisper_init_from_file_with_params(path.as_ptr(), cparams)
        };
        NonNull::new(raw)
            .map(Context)
            .ok_or_else(|| format!("cannot load whisper model {}", model.display()))
    }

    fn raw(&self) -> *mut sys::whisper_context {
        self.0.as_ptr()
    }

    /// Encode and decode `samples`; results via `segments`, encoder output
    /// stays for `decode`.
    pub fn full(&mut self, params: &FullParams, samples: &[f32]) -> Result<(), String> {
        let n = c_int(samples.len())?;
        // SAFETY: the context is live and exclusively borrowed; the CStrs
        // outlive the call; `samples` has `n` entries.
        let rc = unsafe {
            let mut p = sys::whisper_full_default_params(
                sys::whisper_sampling_strategy_WHISPER_SAMPLING_GREEDY,
            );
            p.n_threads = params.threads;
            p.language = params.language.as_ptr();
            p.initial_prompt = params.prompt.as_ptr();
            p.no_timestamps = true;
            p.single_segment = true;
            p.print_progress = false;
            p.print_realtime = false;
            p.print_timestamps = false;
            p.print_special = false;
            p.audio_ctx = params.audio_ctx;
            sys::whisper_full(self.raw(), p, samples.as_ptr(), n)
        };
        if rc == 0 {
            Ok(())
        } else {
            Err(format!("whisper_full failed ({rc})"))
        }
    }

    /// The segments of the last `full`, text copied out.
    pub fn segments(&self) -> Vec<Segment> {
        let ctx = self.raw();
        // SAFETY: indices come from the context's own counts; text pointers
        // are owned by the context and copied before returning.
        unsafe {
            (0..sys::whisper_full_n_segments(ctx))
                .map(|i| Segment {
                    no_speech: sys::whisper_full_get_segment_no_speech_prob(ctx, i),
                    tokens: (0..sys::whisper_full_n_tokens(ctx, i))
                        .map(|t| {
                            let data = sys::whisper_full_get_token_data(ctx, i, t);
                            let s = sys::whisper_full_get_token_text(ctx, i, t);
                            TokenData {
                                id: data.id,
                                p: data.p,
                                text: if s.is_null() {
                                    String::new()
                                } else {
                                    CStr::from_ptr(s).to_string_lossy().into_owned()
                                },
                            }
                        })
                        .collect(),
                })
                .collect()
        }
    }

    pub fn tokenize(&self, text: &str) -> Result<Vec<Token>, String> {
        let c = CString::new(text).map_err(|e| e.to_string())?;
        // SAFETY: a null buffer of size 0 makes whisper return the needed size
        // negated; the second call writes at most `want` tokens into `buf`.
        unsafe {
            let want =
                sys::whisper_tokenize(self.raw(), c.as_ptr(), ptr::null_mut(), 0).unsigned_abs();
            let mut buf = vec![0; usize::try_from(want).map_err(|e| e.to_string())?];
            let got =
                sys::whisper_tokenize(self.raw(), c.as_ptr(), buf.as_mut_ptr(), c_int(buf.len())?);
            buf.truncate(usize::try_from(got).map_err(|_| format!("cannot tokenize {text:?}"))?);
            Ok(buf)
        }
    }

    pub fn special(&self) -> Special {
        let ctx = self.raw();
        // SAFETY: plain getters on a live context.
        unsafe {
            Special {
                prev: sys::whisper_token_prev(ctx),
                sot: sys::whisper_token_sot(ctx),
                eot: sys::whisper_token_eot(ctx),
                transcribe: sys::whisper_token_transcribe(ctx),
                no_timestamps: sys::whisper_token_not(ctx),
            }
        }
    }

    pub fn language_token(&self, language: &CStr) -> Option<Token> {
        // SAFETY: `language` is a valid C string; getters on a live context.
        unsafe {
            let id = sys::whisper_lang_id(language.as_ptr());
            (id >= 0).then(|| sys::whisper_token_lang(self.raw(), id))
        }
    }

    /// Longest prompt whisper conditions on.
    pub fn max_prompt(&self) -> usize {
        // SAFETY: getter on a live context.
        usize::try_from(unsafe { sys::whisper_n_text_ctx(self.raw()) } / 2).unwrap_or(0)
    }

    /// Run the decoder over `tokens` placed after the first `n_past` ones
    /// (which are kept from earlier calls) and the encoder output of the
    /// last `full`; returns the logits for the token after the last one.
    pub fn decode(
        &mut self,
        tokens: &[Token],
        n_past: usize,
        threads: i32,
    ) -> Result<&[f32], String> {
        if tokens.is_empty() {
            return Err("nothing to decode".into());
        }
        let ctx = self.raw();
        // SAFETY: the context is exclusively borrowed; `tokens` holds the
        // given count; the logits row has n_vocab floats and lives until the
        // next decode, which needs `&mut self` and so ends this borrow.
        unsafe {
            let rc = sys::whisper_decode(
                ctx,
                tokens.as_ptr(),
                c_int(tokens.len())?,
                c_int(n_past)?,
                threads,
            );
            if rc != 0 {
                return Err(format!("whisper_decode failed ({rc})"));
            }
            let n_vocab = usize::try_from(sys::whisper_n_vocab(ctx)).map_err(|e| e.to_string())?;
            let logits = sys::whisper_get_logits(ctx);
            if logits.is_null() {
                return Err("whisper returned no logits".into());
            }
            // whisper_get_logits points at the first row; with one row of
            // logits per batch entry the last is ours.
            Ok(slice::from_raw_parts(
                logits.add(n_vocab * (tokens.len() - 1)),
                n_vocab,
            ))
        }
    }
}
