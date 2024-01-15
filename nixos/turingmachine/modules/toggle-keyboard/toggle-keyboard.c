#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

int main() {
  struct stat st = {0};
  if (stat("/sys/module/i8042", &st) != 0) {
    printf("Disabling keyboard\n");
    execlp("rmmod", "rmmod", "i8042", NULL);
    perror("fail to rmmod");
    return 1;
  }

  printf("Enabling keyboard\n");
  execlp("modprobe", "modprobe", "i8042", NULL);
  perror("fail to modprobe");
  return 1;
}
