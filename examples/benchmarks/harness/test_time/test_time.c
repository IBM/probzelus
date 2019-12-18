#include <time.h>
#include <stdio.h>

int main() {
    struct timespec res;

    fprintf(stderr, "Getting clock resolution\n");
    if (clock_getres(CLOCK_MONOTONIC, &res)) return 1;

    printf("Seconds resolution: %ld\n", res.tv_sec);
    printf("Nanoseconds resolution: %ld\n", res.tv_nsec);
    return 0;
}
