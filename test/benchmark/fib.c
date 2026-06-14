#include <stdio.h>

long fib(long n) {
    if(n < 2) {
        return n;
    }
    return fib(n-1) + fib(n-2);
}

int main() {
    long result = fib(44);
    printf("%ld\n", result);
    return 0;
}
