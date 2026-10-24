//! onnxruntime via `ort`, loaded at runtime from the same library
//! sherpa-onnx links, so the process carries one copy of it.

use std::path::Path;
use std::sync::OnceLock;

use ort::environment::Environment;
use ort::session::Session;
use ort::value::Tensor;

pub use ort::session::SessionOutputs as Outputs;

fn environment() -> Result<&'static Environment, String> {
    static ENV: OnceLock<Result<Environment, String>> = OnceLock::new();
    ENV.get_or_init(|| {
        let lib = option_env!("VA_ONNXRUNTIME").unwrap_or("libonnxruntime.so");
        let err = |e: &dyn std::fmt::Display| format!("onnxruntime {lib}: {e}");
        ort::init_from(lib)
            .map_err(|e| err(&e))?
            .build()
            .map_err(|e| err(&e))
    })
    .as_ref()
    .map_err(Clone::clone)
}

pub fn session(model: &Path, threads: usize) -> Result<Session, String> {
    let err = |e: &dyn std::fmt::Display| format!("{}: {e}", model.display());
    Session::builder(environment()?)
        .map_err(|e| err(&e))?
        .with_intra_threads(threads)
        .map_err(|e| err(&e))?
        .commit_from_file(model)
        .map_err(|e| err(&e))
}

pub fn tensor<T: ort::value::PrimitiveTensorElementType + std::fmt::Debug + Clone + 'static>(
    shape: &[usize],
    data: Vec<T>,
) -> Result<Tensor<T>, String> {
    Tensor::from_array((shape.to_vec(), data)).map_err(|e| e.to_string())
}

/// The named output as a flat `f32` slice with its shape.
pub fn output<'a>(out: &'a Outputs, name: &str) -> Result<(Vec<usize>, &'a [f32]), String> {
    let (shape, data) = out[name]
        .try_extract_tensor::<f32>()
        .map_err(|e| format!("output {name}: {e}"))?;
    let shape = shape
        .iter()
        .map(|&d| usize::try_from(d).map_err(|_| format!("output {name}: dimension {d}")))
        .collect::<Result<_, _>>()?;
    Ok((shape, data))
}
