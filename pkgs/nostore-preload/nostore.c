/*
 * nostore — LD_PRELOAD / DYLD_INSERT_LIBRARIES guard rail that makes
 * readdir() on /nix/store fail with EACCES so AI coding agents don't drown
 * in millions of store entries.  Accident-preventer, not a security boundary.
 *
 * The variadic `mode` is forwarded unconditionally; the kernel ignores it
 * when O_CREAT/O_TMPFILE are absent, so no flag introspection is needed.
 */

#define _GNU_SOURCE
#include <dirent.h>
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <unistd.h>

#ifdef __APPLE__
struct dyld_interpose {
  const void *replacement;
  const void *replacee;
};
#define WRAPPER(ret, name) static ret _nostore_##name
#define LOOKUP_REAL(name) (&(name))
#define WRAPPER_DEF(name)                                                      \
  __attribute__((used)) static struct dyld_interpose _nostore_ip_##name        \
      __attribute__((section("__DATA,__interpose"))) = {                       \
          (const void *)&_nostore_##name, (const void *)&(name)};
#else
#define WRAPPER(ret, name) ret name
#define LOOKUP_REAL(name) dlsym(RTLD_NEXT, #name)
#define WRAPPER_DEF(name)
/* Pin pre-2.34 dlsym so the .so loads into binaries from any glibc era. */
#if defined(__GLIBC__) && defined(__x86_64__)
__asm__(".symver dlsym,dlsym@GLIBC_2.2.5");
#elif defined(__GLIBC__) && defined(__aarch64__)
__asm__(".symver dlsym,dlsym@GLIBC_2.17");
#endif
#endif

static int is_store_root(const char *p) {
  static const char want[] = "/nix/store";
  if (!p)
    return 0;
  unsigned i = 0;
  while (i < sizeof(want) - 1) {
    if (p[i] != want[i])
      return 0;
    i++;
  }
  while (p[i] == '/')
    i++;
  return p[i] == '\0';
}

static void hint(void) {
  static volatile int done;
  if (done)
    return;
  done = 1;
  static const char msg[] =
      "nostore-preload: listing /nix/store is intentionally blocked for AI "
      "agents (millions of entries). Use a known store path, nix-locate, "
      "`nix eval`/`nix path-info`, or devshell env vars (e.g. "
      "$NIX_CFLAGS_COMPILE, $PKG_CONFIG_PATH, $buildInputs, `env | rg "
      "/nix/store`) to discover dependencies. sudo will not bypass this.\n";
  ssize_t r = write(2, msg, sizeof(msg) - 1);
  (void)r;
}

static int deny(void) {
  hint();
  errno = EACCES;
  return -1;
}

/* nix itself open()s /nix/store with plain O_RDONLY just to fstat() it
 * (LocalFSStore type probe), so only refuse O_DIRECTORY opens. */
#define DENY_DIR_OPEN(path, flags)                                             \
  if (((flags) & O_DIRECTORY) && is_store_root(path))                          \
    return deny();

WRAPPER(int, open)(const char *path, int flags, ...) {
  DENY_DIR_OPEN(path, flags);
  static int (*real)(const char *, int, ...);
  if (!real)
    real = LOOKUP_REAL(open);
  va_list ap;
  va_start(ap, flags);
  unsigned mode = va_arg(ap, unsigned);
  va_end(ap);
  return real(path, flags, mode);
}
WRAPPER_DEF(open)

WRAPPER(int, openat)(int dirfd, const char *path, int flags, ...) {
  DENY_DIR_OPEN(path, flags);
  static int (*real)(int, const char *, int, ...);
  if (!real)
    real = LOOKUP_REAL(openat);
  va_list ap;
  va_start(ap, flags);
  unsigned mode = va_arg(ap, unsigned);
  va_end(ap);
  return real(dirfd, path, flags, mode);
}
WRAPPER_DEF(openat)

#ifndef __APPLE__
WRAPPER(int, open64)(const char *path, int flags, ...) {
  DENY_DIR_OPEN(path, flags);
  static int (*real)(const char *, int, ...);
  if (!real)
    real = LOOKUP_REAL(open64);
  va_list ap;
  va_start(ap, flags);
  unsigned mode = va_arg(ap, unsigned);
  va_end(ap);
  return real(path, flags, mode);
}
WRAPPER(int, openat64)(int dirfd, const char *path, int flags, ...) {
  DENY_DIR_OPEN(path, flags);
  static int (*real)(int, const char *, int, ...);
  if (!real)
    real = LOOKUP_REAL(openat64);
  va_list ap;
  va_start(ap, flags);
  unsigned mode = va_arg(ap, unsigned);
  va_end(ap);
  return real(dirfd, path, flags, mode);
}
WRAPPER(int, __open_2)(const char *path, int flags) {
  DENY_DIR_OPEN(path, flags);
  static int (*real)(const char *, int);
  if (!real)
    real = LOOKUP_REAL(__open_2);
  return real(path, flags);
}
WRAPPER(int, __open64_2)(const char *path, int flags) {
  DENY_DIR_OPEN(path, flags);
  static int (*real)(const char *, int);
  if (!real)
    real = LOOKUP_REAL(__open64_2);
  return real(path, flags);
}
WRAPPER(int, __openat_2)(int dirfd, const char *path, int flags) {
  DENY_DIR_OPEN(path, flags);
  static int (*real)(int, const char *, int);
  if (!real)
    real = LOOKUP_REAL(__openat_2);
  return real(dirfd, path, flags);
}
WRAPPER(int, __openat64_2)(int dirfd, const char *path, int flags) {
  DENY_DIR_OPEN(path, flags);
  static int (*real)(int, const char *, int);
  if (!real)
    real = LOOKUP_REAL(__openat64_2);
  return real(dirfd, path, flags);
}
#endif

/* glibc opendir/scandir call an internal __openat that bypasses the PLT,
 * so they need their own wrappers. */

WRAPPER(DIR *, opendir)(const char *path) {
  if (is_store_root(path)) {
    hint();
    errno = EACCES;
    return 0;
  }
  static DIR *(*real)(const char *);
  if (!real)
    real = LOOKUP_REAL(opendir);
  return real(path);
}
WRAPPER_DEF(opendir)

WRAPPER(int,
        scandir)(const char *path, struct dirent ***namelist,
                 int (*sel)(const struct dirent *),
                 int (*cmp)(const struct dirent **, const struct dirent **)) {
  if (is_store_root(path))
    return deny();
  static int (*real)(const char *, struct dirent ***,
                     int (*)(const struct dirent *),
                     int (*)(const struct dirent **, const struct dirent **));
  if (!real)
    real = LOOKUP_REAL(scandir);
  return real(path, namelist, sel, cmp);
}
WRAPPER_DEF(scandir)

#ifndef __APPLE__
WRAPPER(int, scandir64)(const char *path, struct dirent64 ***namelist,
                        int (*sel)(const struct dirent64 *),
                        int (*cmp)(const struct dirent64 **,
                                   const struct dirent64 **)) {
  if (is_store_root(path))
    return deny();
  static int (*real)(
      const char *, struct dirent64 ***, int (*)(const struct dirent64 *),
      int (*)(const struct dirent64 **, const struct dirent64 **));
  if (!real)
    real = LOOKUP_REAL(scandir64);
  return real(path, namelist, sel, cmp);
}
#endif

#ifdef __APPLE__
extern DIR *__opendir2(const char *, int);
WRAPPER(DIR *, __opendir2)(const char *path, int flags) {
  if (is_store_root(path)) {
    hint();
    errno = EACCES;
    return 0;
  }
  static DIR *(*real)(const char *, int);
  if (!real)
    real = LOOKUP_REAL(__opendir2);
  return real(path, flags);
}
WRAPPER_DEF(__opendir2)
#endif
