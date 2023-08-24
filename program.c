#include <stdio.h>
#include <inttypes.h>

// a very boring C program that prints out the number 400000000

int64_t entry() {
    return 400000000;
}

int main(int argc, char **argv) {
    printf("%" PRIi64, entry());
    return 0;
}