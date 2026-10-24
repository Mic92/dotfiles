// Opaque wrapper around the TFLite micro audio frontend, configured exactly
// like pymicro-features so the official microWakeWord models match.
#include <cstdint>
#include <cstdlib>

extern "C" {
#include "tensorflow/lite/experimental/microfrontend/lib/frontend.h"
#include "tensorflow/lite/experimental/microfrontend/lib/frontend_util.h"
}

namespace {
struct Handle {
  FrontendConfig cfg;
  FrontendState state;
};
} // namespace

extern "C" {

void *mf_create(void) {
  auto *h = static_cast<Handle *>(calloc(1, sizeof(Handle)));
  if (h == nullptr) {
    return nullptr;
  }
  h->cfg.window.size_ms = 30;
  h->cfg.window.step_size_ms = 10;
  h->cfg.filterbank.num_channels = 40;
  h->cfg.filterbank.lower_band_limit = 125.0F;
  h->cfg.filterbank.upper_band_limit = 7500.0F;
  h->cfg.noise_reduction.smoothing_bits = 10;
  h->cfg.noise_reduction.even_smoothing = 0.025F;
  h->cfg.noise_reduction.odd_smoothing = 0.06F;
  h->cfg.noise_reduction.min_signal_remaining = 0.05F;
  h->cfg.pcan_gain_control.enable_pcan = 1;
  h->cfg.pcan_gain_control.strength = 0.95F;
  h->cfg.pcan_gain_control.offset = 80.0F;
  h->cfg.pcan_gain_control.gain_bits = 21;
  h->cfg.log_scale.enable_log = 1;
  h->cfg.log_scale.scale_shift = 6;
  if (FrontendPopulateState(&h->cfg, &h->state, 16000) == 0) {
    free(h);
    return nullptr;
  }
  return h;
}

// Consumes up to n samples; returns the number of features written to out
// (0 or 40) and stores the consumed sample count in *read.
size_t mf_process(void *handle, const int16_t *samples, size_t n, size_t *read,
                  uint16_t *out, size_t out_len) {
  auto *h = static_cast<Handle *>(handle);
  FrontendOutput fo = FrontendProcessSamples(&h->state, samples, n, read);
  size_t count = fo.size < out_len ? fo.size : out_len;
  for (size_t i = 0; i < count; ++i) {
    out[i] = fo.values[i];
  }
  return count;
}

void mf_free(void *handle) {
  auto *h = static_cast<Handle *>(handle);
  FrontendFreeStateContents(&h->state);
  free(h);
}
}
