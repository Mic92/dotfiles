{
  lib,
  stdenv,
  rustPlatform,
  fetchFromGitHub,
  fetchurl,
  fetchzip,
  autoPatchelfHook,
  makeWrapper,
  pkg-config,
  pipewire,
  whisper-cpp,
  sherpa-onnx,
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
    url = "https://huggingface.co/ggml-org/whisper-vad/resolve/main/ggml-silero-v5.1.2.bin";
    hash = "sha256-KZQNmNQrkfvQXOSJ8+z3xy8KQvAn5IdZGaKPtMBOos8=";
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
  whisperModel = fetchurl {
    url = "https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-small.bin";
    hash = "sha256-G+OpsgY4Z7k35k4ux0gzZKeZF+FX+pjF2UtcH//qmHs=";
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
    whisper-cpp
    sherpa-onnx
    stdenv.cc.cc.lib
  ];

  env = {
    MICRO_FEATURES_SRC = microFeatures;
    TFLITE_LIB_DIR = tfliteLibDir;
    VA_WAKE_MODEL = "${placeholder "out"}/share/voice-assistant/models/alexa.tflite";
    VA_VAD_MODEL = vadModel;
    VA_WHISPER_MODEL = whisperModel;
    VA_TTS_MODEL = ttsModel;
    VA_SOUNDS = "${uisfx}/packages/uisfx/sounds/${soundTheme}";
    SHERPA_CFLAGS = "-I${lib.getDev sherpa-onnx}/include";
  };

  # bindgen needs whisper.h and the ggml headers it includes
  preBuild = ''
    export WHISPER_CFLAGS="$(pkg-config --cflags whisper)"
  '';

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
