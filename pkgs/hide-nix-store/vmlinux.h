/* Minimal hand-written vmlinux.h for CO-RE.
 * Only the types referenced by hide_nix_store.bpf.c. Field offsets are
 * relocated at load time via BTF, so layout here is irrelevant — only
 * names and rough types matter.
 */
#pragma once

typedef signed char __s8;
typedef unsigned char __u8;
typedef short __s16;
typedef unsigned short __u16;
typedef int __s32;
typedef unsigned int __u32;
typedef long long __s64;
typedef unsigned long long __u64;

typedef __u16 __le16;
typedef __u16 __be16;
typedef __u32 __le32;
typedef __u32 __be32;
typedef __u64 __le64;
typedef __u64 __be64;
typedef __u32 __wsum;
typedef __u16 __sum16;

typedef __s8 s8;
typedef __u8 u8;
typedef __s16 s16;
typedef __u16 u16;
typedef __s32 s32;
typedef __u32 u32;
typedef __s64 s64;
typedef __u64 u64;

typedef _Bool bool;
enum { false = 0, true = 1 };

enum bpf_map_type {
  BPF_MAP_TYPE_UNSPEC = 0,
  BPF_MAP_TYPE_HASH = 1,
  BPF_MAP_TYPE_ARRAY = 2,
};

enum {
  BPF_ANY = 0,
  BPF_NOEXIST = 1,
  BPF_EXIST = 2,
};

struct vfsmount;
struct dentry;

struct path {
  struct vfsmount *mnt;
  struct dentry *dentry;
};

struct file {
  struct path f_path;
};

struct fdtable {
  unsigned int max_fds;
  struct file **fd;
};

struct files_struct {
  struct fdtable *fdt;
};

struct task_struct {
  struct files_struct *files;
};

struct pt_regs {
  unsigned long di;
  unsigned long si;
  unsigned long dx;
  unsigned long cx;
  unsigned long r8;
  unsigned long r9;
  unsigned long sp;
  unsigned long ip;
};
