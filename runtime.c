#include <stdio.h>
#include <inttypes.h>

extern int64_t entry();

int main(int argc, char **argv) {
    printf("%" PRIi64, entry());
    return 0;
}