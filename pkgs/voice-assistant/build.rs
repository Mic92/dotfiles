// Panicking is how build scripts report errors.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use std::env;
use std::path::PathBuf;

fn var(name: &str) -> String {
    println!("cargo:rerun-if-env-changed={name}");
    env::var(name).unwrap_or_else(|_| panic!("{name} must be set (see default.nix)"))
}

fn main() {
    // microfrontend + kissfft sources from pymicro-features
    let features = PathBuf::from(var("MICRO_FEATURES_SRC"));
    let fe = features.join("tensorflow/lite/experimental/microfrontend/lib");
    let mut build = cc::Build::new();
    build
        .cpp(true)
        .std("c++17")
        .opt_level(2)
        .define("FIXED_POINT", "16")
        .include(&features)
        .include(features.join("kissfft"))
        .file("csrc/frontend_shim.cc")
        .file(features.join("kissfft/kiss_fft.cc"))
        .file(features.join("kissfft/tools/kiss_fftr.cc"))
        .warnings(false);
    for f in [
        "kiss_fft_int16",
        "fft",
        "fft_util",
        "filterbank",
        "filterbank_util",
        "frontend",
        "frontend_util",
        "log_lut",
        "log_scale",
        "log_scale_util",
        "noise_reduction",
        "noise_reduction_util",
        "pcan_gain_control",
        "pcan_gain_control_util",
        "window",
        "window_util",
    ] {
        build.file(fe.join(format!("{f}.cc")));
    }
    build.compile("microfrontend");
    println!("cargo:rerun-if-changed=csrc/frontend_shim.cc");

    println!("cargo:rustc-link-search=native={}", var("TFLITE_LIB_DIR"));
    println!("cargo:rustc-link-lib=dylib=tensorflowlite_c");
    let out = PathBuf::from(env::var("OUT_DIR").unwrap());

    println!("cargo:rustc-link-lib=dylib=sherpa-onnx-c-api");
    bindgen::Builder::default()
        .header_contents("wrapper.h", "#include <sherpa-onnx/c-api/c-api.h>")
        .clang_args(var("SHERPA_CFLAGS").split_whitespace())
        .allowlist_function("SherpaOnnx(CreateOfflineTts|DestroyOfflineTts|OfflineTtsGenerateWithConfig|OfflineTtsSampleRate|DestroyOfflineTtsGeneratedAudio)")
        .allowlist_type("SherpaOnnx(OfflineTtsConfig|GenerationConfig|GeneratedAudio)")
        .generate()
        .expect("bindgen sherpa-onnx c-api.h")
        .write_to_file(out.join("sherpa_sys.rs"))
        .expect("write bindings");
}
