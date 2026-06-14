#include <stdio.h>
#include <stdlib.h>

int main() {
    const long limit = 100000000;

    char* prime = malloc(limit + 1);

    for(long i = 0; i <= limit; i++) {
        prime[i] = 1;
    }

    prime[0] = 0;
    prime[1] = 0;

    for(int i = 2; i * i <= limit; i++) {
        if(prime[i]) {
            for(long j = i * i; j <= limit; j += i) {
                prime[j] = 0;
            }
        }
    }

    long count = 0;

    for(long i = 2; i <= limit; i++) {
        if(prime[i]) {
            count++;
        }
    }

    printf("%ld\n", count);

    free(prime);
    return 0;
}