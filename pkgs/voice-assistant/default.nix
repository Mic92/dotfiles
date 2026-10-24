{
  lib,
  stdenv,
  rustPlatform,
  fetchFromGitHub,
  fetchurl,
  fetchzip,
  linkFarm,
  autoPatchelfHook,
  makeWrapper,
  pkg-config,
  pipewire,
  sherpa-onnx,
  onnxruntime,
  libnotify,
  playerctl,
  mpv,
  curl,
  # one of uisfx's sets: arcade cinematic dreamy glass mechanical minimal
  # organic rubber scifi soft studio zen
  soundTheme ? "mechanical",
}:
let
  # TFLite micro frontend + kissfft, as used by microWakeWord
  microFeatures = fetchFromGitHub {
    owner = "rhasspy";
    repo = "pymicro-features";
    tag = "v2.0.2";
    hash = "sha256-bCvrsvifUxM7jfqBePmcYviZ5pIy1ZApq48Pu/5b7AM=";
  };
  # prebuilt libtensorflowlite_c and the official wake word models
  microWakeWord = fetchFromGitHub {
    owner = "OHF-Voice";
    repo = "pymicro-wakeword";
    tag = "v2.5.0";
    hash = "sha256-hyCH0yMUyQbAER+kvIgdlMDSbH4RVq7tFaxSprqCdRw=";
  };
  tfliteLibDir = "${microWakeWord}/lib/${
    {
      x86_64-linux = "linux_amd64";
      aarch64-linux = "linux_arm64";
    }
    .${stdenv.hostPlatform.system}
  }";
  vadModel = fetchurl {
    url = "https://raw.githubusercontent.com/snakers4/silero-vad/v5.1.2/src/silero_vad/data/silero_vad.onnx";
    hash = "sha256-JiOilT9v89LB5hdAxs23FoEzR5smff7xFKSjzFvdeI8=";
  };
  # NVIDIA parakeet-unified-en-0.6b (NVIDIA Open Model License), int8 RNNT
  asrModel = fetchzip {
    url = "https://github.com/k2-fsa/sherpa-onnx/releases/download/asr-models/sherpa-onnx-nemo-parakeet-unified-en-0.6b-int8-non-streaming.tar.bz2";
    hash = "sha256-2cG9a95labt6PBn6utz/mtHUlfpp18nJ3Bv95g/lwP4=";
  };
  intentModel =
    let
      hf =
        f: "https://huggingface.co/Xenova/gte-small/resolve/5927d1727bb12db490052a1b33265ad78058de08/${f}";
    in
    linkFarm "gte-small" {
      "model.onnx" = fetchurl {
        url = hf "onnx/model_quantized.onnx";
        hash = "sha256-GN7BBRCbYAQ2l5nKR2H7j7QTxkFywCFHvPrBhrXF9ss=";
      };
      "vocab.txt" = fetchurl {
        url = hf "vocab.txt";
        hash = "sha256-B+ztN1zsFE0nyQAkHz4zlHjeyVj5L928VR8pXJkgOKM=";
      };
    };
  ttsModel = fetchzip {
    # micro: clearer than nano-int8 and no slower on CPU
    url = "https://github.com/k2-fsa/sherpa-onnx/releases/download/tts-models/kitten-micro-en-v0_8.tar.bz2";
    hash = "sha256-Pp7XyLa6BpH36tLOYvLtVHmuqDx3Prof9cSzxLbfEsw=";
  };
  # CC0 UI sound sets
  uisfx = fetchFromGitHub {
    owner = "romainsimon";
    repo = "uisfx";
    rev = "9950fe66f993a6660dab9c2651dcbcd899ffd83b";
    hash = "sha256-8nAVfndZDcR2P+LHniH/IYw14eS2RHcObBdbmOWcFh8=";
  };
in
rustPlatform.buildRustPackage {
  pname = "voice-assistant";
  version = "0.1.0";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./Cargo.toml
      ./Cargo.lock
      ./build.rs
      ./src
      ./csrc
    ];
  };
  cargoLock.lockFile = ./Cargo.lock;

  nativeBuildInputs = [
    autoPatchelfHook
    makeWrapper
    pkg-config
    rustPlatform.bindgenHook
  ];
  buildInputs = [
    pipewire
    sherpa-onnx
    stdenv.cc.cc.lib
  ];

  env = {
    MICRO_FEATURES_SRC = microFeatures;
    TFLITE_LIB_DIR = tfliteLibDir;
    VA_WAKE_MODEL = "${placeholder "out"}/share/voice-assistant/models/alexa.tflite";
    VA_VAD_MODEL = vadModel;
    VA_ASR_MODEL = asrModel;
    VA_INTENT_MODEL = intentModel;
    # The same library sherpa-onnx links. ort loads it at runtime.
    VA_ONNXRUNTIME = "${lib.getLib onnxruntime}/lib/libonnxruntime.so";
    VA_TTS_MODEL = ttsModel;
    VA_SOUNDS = "${uisfx}/packages/uisfx/sounds/${soundTheme}";
    SHERPA_CFLAGS = "-I${lib.getDev sherpa-onnx}/include";
  };

  postInstall = ''
    install -Dm644 -t $out/lib ${tfliteLibDir}/libtensorflowlite_c.so
    install -Dm644 -t $out/share/voice-assistant/models ${microWakeWord}/pymicro_wakeword/models/*
    wrapProgram $out/bin/voice-assistant \
      --prefix PATH : ${
        lib.makeBinPath [
          pipewire
          libnotify
          # pausing music, now playing, the radio, web lookups
          playerctl
          mpv
          curl
        ]
      }
  '';

  meta = {
    description = "Wake word voice assistant that drives the noctalia timer";
    license = lib.licenses.mit;
    platforms = [ "x86_64-linux" ];
    sourceProvenance = with lib.sourceTypes; [
      fromSource
      binaryNativeCode
    ];
    mainProgram = "voice-assistant";
  };
}
