#include <stdio.h>
#include <time.h>

const long long limit = 1000000000LL;


// FizzBuzz benchmark
void fizzbuzzBenchmark() {
    struct timespec startTime, endTime;
    clock_gettime(CLOCK_MONOTONIC, &startTime);
    
    for (long long i = 1; i <= limit; i++) {
        if (i % 15 == 0) {
            // "FizzBuzz"
        } else if (i % 3 == 0) {
            // "Fizz"
        } else if (i % 5 == 0) {
            // "Buzz"
        } else {
            // Convert to string (not actually doing it for performance)
        }
    }
    
    clock_gettime(CLOCK_MONOTONIC, &endTime);
    long long elapsed_ns = (endTime.tv_sec - startTime.tv_sec) * 1000000000LL + 
                          (endTime.tv_nsec - startTime.tv_nsec);
    printf("FizzBuzz time: %.9fs\n", (double)elapsed_ns / 1000000000.0);
}


// Run all benchmarks
int main() {
    printf("=== C COMPARISON ===\n");
    fizzbuzzBenchmark();
    return 0;
}
