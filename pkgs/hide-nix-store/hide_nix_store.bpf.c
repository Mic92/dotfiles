// SPDX-License-Identifier: MIT
// Hide /nix/store from selected uids:
//  - LSM file_open: deny open() of anything under /nix/store/
//  - fmod_ret on getdents64: return 0 ("end of directory") when the fd
//    points at /nix/store, so `ls /nix/store` shows an empty dir.
//
// Target uids are looked up in the `target_uids` map so userspace can
// add/remove them at runtime via the pinned map.

#include "vmlinux.h"
#include <bpf/bpf_core_read.h>
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_tracing.h>

char LICENSE[] SEC("license") = "GPL";

#define PATH_PREFIX "/nix/store"
#define PATH_PREFIX_LEN (sizeof(PATH_PREFIX) - 1)
#define EPERM 1

struct {
  __uint(type, BPF_MAP_TYPE_HASH);
  __uint(max_entries, 64);
  __type(key, u32);  // uid
  __type(value, u8); // unused
  __uint(pinning, LIBBPF_PIN_BY_NAME);
} target_uids SEC(".maps");

static __always_inline bool is_target_uid(void) {
  u32 uid = bpf_get_current_uid_gid() & 0xffffffff;
  return bpf_map_lookup_elem(&target_uids, &uid) != NULL;
}

static __always_inline bool path_is_nix_store(struct path *path) {
  char buf[64];
  long len = bpf_d_path(path, buf, sizeof(buf));
  if (len < 0)
    return false;
  // bpf_d_path returns length incl. NUL
  if (len < (long)(PATH_PREFIX_LEN + 1))
    return false;
  for (int i = 0; i < PATH_PREFIX_LEN; i++)
    if (buf[i] != PATH_PREFIX[i])
      return false;
  // either exactly "/nix/store" or "/nix/store/..."
  return buf[PATH_PREFIX_LEN] == '\0' || buf[PATH_PREFIX_LEN] == '/';
}

// Deny opening anything under /nix/store for target uids.
SEC("lsm/file_open")
int BPF_PROG(deny_nix_store_open, struct file *file) {
  if (!is_target_uid())
    return 0;
  if (path_is_nix_store(&file->f_path))
    return -EPERM;
  return 0;
}

// Resolve an fd of the current task to its struct file*.
static __always_inline struct file *fd_to_file(int fd) {
  struct task_struct *task = (struct task_struct *)bpf_get_current_task_btf();
  struct files_struct *files = BPF_CORE_READ(task, files);
  struct fdtable *fdt = BPF_CORE_READ(files, fdt);
  unsigned int max_fds = BPF_CORE_READ(fdt, max_fds);
  if (fd < 0 || (unsigned int)fd >= max_fds)
    return NULL;
  struct file **farr = BPF_CORE_READ(fdt, fd);
  struct file *f = NULL;
  bpf_core_read(&f, sizeof(f), farr + fd);
  return f;
}

// Make readdir on /nix/store return "no more entries".
// We hook the security_* variant because fmod_ret is only allowed there
// and on error-injectable functions. security_file_permission is called
// from iterate_dir() before entries are produced; returning -EPERM makes
// getdents64 fail. To get a *clean empty* listing instead of EPERM we'd
// need bpf_override_return on getdents64 (CONFIG_BPF_KPROBE_OVERRIDE),
// which is the second program below.
SEC("lsm/file_permission")
int BPF_PROG(deny_nix_store_readdir, struct file *file, int mask) {
  if (!is_target_uid())
    return 0;
  if (path_is_nix_store(&file->f_path))
    return -EPERM;
  return 0;
}

#ifdef HAVE_OVERRIDE_RETURN
// Optional: truly empty directory instead of EPERM.
// Requires CONFIG_BPF_KPROBE_OVERRIDE=y and getdents64 in the
// error-injection list. Disabled by default; enable with
// -DHAVE_OVERRIDE_RETURN if your kernel supports it.
SEC("kprobe/__x64_sys_getdents64")
int BPF_KPROBE(empty_nix_store_getdents, struct pt_regs *regs) {
  if (!is_target_uid())
    return 0;
  int fd = (int)PT_REGS_PARM1_CORE(regs);
  struct file *f = fd_to_file(fd);
  if (!f)
    return 0;
  if (path_is_nix_store(&f->f_path))
    bpf_override_return((void *)ctx, 0);
  return 0;
}
#endif
