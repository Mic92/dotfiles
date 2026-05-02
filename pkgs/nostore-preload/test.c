#define _GNU_SOURCE
#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

int main(void) {
  errno = 0;
  assert(opendir("/nix/store") == NULL && errno == EACCES);
  errno = 0;
  assert(opendir("/nix/store/") == NULL && errno == EACCES);
  errno = 0;
  assert(open("/nix/store", O_RDONLY | O_DIRECTORY) == -1 && errno == EACCES);
  errno = 0;
  assert(openat(AT_FDCWD, "/nix/store", O_RDONLY | O_DIRECTORY) == -1 &&
         errno == EACCES);
  /* nix probes the store dir with plain O_RDONLY to fstat(); must succeed. */
  int probe = open("/nix/store", O_RDONLY | O_CLOEXEC);
  assert(probe >= 0);
  close(probe);
  errno = 0;
  struct dirent **nl;
  assert(scandir("/nix/store", &nl, NULL, NULL) == -1 && errno == EACCES);

  DIR *d = opendir("/");
  assert(d != NULL);
  closedir(d);
  d = opendir("/nix");
  assert(d != NULL);
  closedir(d);
  int fd = open("/nix/store/.links", O_RDONLY | O_DIRECTORY);
  assert(fd >= 0 || errno != EACCES);
  if (fd >= 0)
    close(fd);

  puts("nostore-preload: all tests passed");
  return 0;
}
