#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main() {
  if (system("lsmod | grep -q i8042") == 0) {
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
