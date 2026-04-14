// SPDX-License-Identifier: MIT
// Minimal loader: opens the skeleton, pins the target_uids map under
// /sys/fs/bpf/hide-nix-store/, seeds it with uids passed on argv,
// attaches all programs and sleeps forever (links live as long as we do).

#include <bpf/bpf.h>
#include <bpf/libbpf.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>

#include "hide_nix_store.skel.h"

#define PIN_DIR "/sys/fs/bpf/hide-nix-store"

static volatile sig_atomic_t stop;
static void on_sig(int sig) {
  (void)sig;
  stop = 1;
}

int main(int argc, char **argv) {
  struct hide_nix_store_bpf *skel;
  int err;

  libbpf_set_print(NULL);

  if (mkdir(PIN_DIR, 0700) && errno != EEXIST) {
    perror("mkdir " PIN_DIR);
    return 1;
  }

  skel = hide_nix_store_bpf__open();
  if (!skel) {
    fprintf(stderr, "open skel failed\n");
    return 1;
  }

  bpf_map__set_pin_path(skel->maps.target_uids, PIN_DIR "/target_uids");

  err = hide_nix_store_bpf__load(skel);
  if (err) {
    fprintf(stderr, "load failed: %d\n", err);
    return 1;
  }

  int map_fd = bpf_map__fd(skel->maps.target_uids);
  for (int i = 1; i < argc; i++) {
    __u32 uid = (unsigned)atoi(argv[i]);
    __u8 one = 1;
    if (bpf_map_update_elem(map_fd, &uid, &one, BPF_ANY))
      fprintf(stderr, "warn: failed to add uid %u\n", uid);
  }

  err = hide_nix_store_bpf__attach(skel);
  if (err) {
    fprintf(stderr, "attach failed: %d\n", err);
    return 1;
  }

  fprintf(stderr, "hide-nix-store: attached, %d uid(s) targeted\n", argc - 1);

  signal(SIGINT, on_sig);
  signal(SIGTERM, on_sig);
  while (!stop)
    pause();

  hide_nix_store_bpf__destroy(skel);
  return 0;
}
