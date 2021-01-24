#!/usr/bin/env python3

import sys
import argparse
import os
import torch
import time
import subprocess
import numpy as np
from typing import Any, Tuple
from http.server import HTTPServer, BaseHTTPRequestHandler
from flask import Flask, request, send_file
from io import BytesIO

from TTS.utils.io import load_config
from TTS.utils.audio import AudioProcessor
from TTS.tts.utils.generic_utils import setup_model
from TTS.tts.utils.text.symbols import symbols, phonemes
from TTS.tts.utils.synthesis import synthesis
from TTS.tts.utils.io import load_checkpoint


def interpolate_vocoder_input(scale_factor, spec):
    """Interpolation to tolarate the sampling rate difference
    btw tts model and vocoder"""
    print(" > before interpolation :", spec.shape)
    spec = torch.tensor(spec).unsqueeze(0).unsqueeze(0)
    spec = torch.nn.functional.interpolate(
        spec, scale_factor=scale_factor, mode="bilinear"
    ).squeeze(0)
    print(" > after interpolation :", spec.shape)
    return spec


class Synthesizer:
    def __init__(
        self,
        tts_config: Any,
        tts_model: Any,
        ap: AudioProcessor,
        vocoder_config: Any,
        vocoder_model: Any,
        ap_vocoder: AudioProcessor,
    ) -> None:
        self.tts_config = tts_config
        self.tts_model = tts_model
        self.ap = ap
        self.vocoder_config = vocoder_config
        self.vocoder_model = vocoder_model
        self.ap_vocoder = ap_vocoder
        # scale factor for sampling rate difference
        self.scale_factor = [1, vocoder_config["audio"]["sample_rate"] / ap.sample_rate]
        print(f"scale_factor: {self.scale_factor}")

    def render_wav(self, data) -> BytesIO:
        wav = np.array(data)

        f = BytesIO()
        self.ap.save_wav(data, f)
        return f

    def tts(self, text, use_cuda: bool = False, use_gl: bool = False) -> BytesIO:
        t_1 = time.time()
        # run tts
        target_sr = self.tts_config.audio["sample_rate"]
        (
            waveform,
            alignment,
            mel_spec,
            mel_postnet_spec,
            stop_tokens,
            inputs,
        ) = synthesis(
            self.tts_model,
            text,
            self.tts_config,
            use_cuda,
            self.ap,
            None,
            None,
            False,
            self.tts_config.enable_eos_bos_chars,
            use_gl,
        )
        # run vocoder
        mel_postnet_spec = self.ap._denormalize(mel_postnet_spec.T).T
        if not use_gl:
            target_sr = self.vocoder_config.audio["sample_rate"]
            vocoder_input = self.ap_vocoder._normalize(mel_postnet_spec.T)
            if self.scale_factor[1] != 1:
                vocoder_input = interpolate_vocoder_input(
                    self.scale_factor, vocoder_input
                )
            else:
                vocoder_input = torch.tensor(vocoder_input).unsqueeze(0)
            waveform = self.vocoder_model.inference(vocoder_input)
        # format output
        if use_cuda and not use_gl:
            waveform = waveform.cpu()
        if not use_gl:
            waveform = waveform.numpy()
        waveform = waveform.squeeze()
        # compute run-time performance
        rtf = (time.time() - t_1) / (len(waveform) / self.ap.sample_rate)
        tps = (time.time() - t_1) / len(waveform)
        print(waveform.shape)
        print(" > Run-time: {}".format(time.time() - t_1))
        print(" > Real-time factor: {}".format(rtf))
        print(" > Time per step: {}".format(tps))
        # display audio
        return self.render_wav(waveform)


def setup_synthesizer(
    tts_config_file: str,
    tts_model_file: str,
    vocoder_config_file: str,
    vocoder_model_file: str,
) -> Synthesizer:
    use_cuda = False
    # load configs
    tts_config = load_config(tts_config_file)
    vocoder_config = load_config(vocoder_config_file)

    ap = AudioProcessor(**tts_config.audio)

    # load the model
    num_chars = len(phonemes) if tts_config.use_phonemes else len(symbols)
    model = setup_model(num_chars, 0, tts_config)

    # load model state
    model, _ = load_checkpoint(model, tts_model_file, use_cuda=use_cuda)
    model.eval()
    model.store_inverse()

    from TTS.vocoder.utils.generic_utils import setup_generator

    # LOAD VOCODER MODEL
    vocoder_model = setup_generator(vocoder_config)
    vocoder_model.load_state_dict(
        torch.load(vocoder_model_file, map_location="cpu")["model"]
    )
    vocoder_model.remove_weight_norm()
    vocoder_model.inference_padding = 0

    ap_vocoder = AudioProcessor(**vocoder_config["audio"])
    if use_cuda:
        vocoder_model.cuda()
    vocoder_model.eval()

    model.length_scale = 1.0  # set speed of the speech.
    model.noise_scale = 0.33  # set speech variation
    return Synthesizer(tts_config, model, ap, vocoder_config, vocoder_model, ap_vocoder)


class Server:
    def __init__(self, synthesizer: Synthesizer) -> None:
        self.synthesizer = synthesizer
        self.server = Flask(__name__)
        self.server.add_url_rule("/api/tts", "tts", self.tts, methods=["GET"])

    def run(self, host: str = "::", port: int = 5002):
        self.server.run(host=host, port=port)

    def tts(self):
        text = request.args.get("text")
        print(" > Model input: {}".format(text))
        data = self.synthesizer.tts(text)
        return send_file(data, mimetype="audio/wav")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--tts-checkpoint", type=str, help="path to TTS checkpoint file"
    )
    parser.add_argument("--tts-config", type=str, help="path to TTS config.json file")
    parser.add_argument(
        "--vocoder-config", type=str, help="path to TTS.vocoder config file."
    )
    parser.add_argument(
        "--vocoder-checkpoint", type=str, help="path to TTS.vocoder checkpoint file."
    )
    parser.add_argument("--port", type=int, default=5002, help="port to listen on.")
    parser.add_argument("--host", type=str, default="::", help="host to listen on.")
    return parser.parse_args()


def main():
    args = parse_args()
    synthesizer = setup_synthesizer(
        args.tts_config,
        args.tts_checkpoint,
        args.vocoder_config,
        args.vocoder_checkpoint,
    )
    s = Server(synthesizer)
    s.run(args.host, args.port)


if __name__ == "__main__":
    main()
