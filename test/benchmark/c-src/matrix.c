#include <stdio.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <time.h>
#endif

static long long monotonic_ns(void) {
#ifdef _WIN32
    static LARGE_INTEGER freq = {0};
    if (freq.QuadPart == 0) QueryPerformanceFrequency(&freq);
    LARGE_INTEGER c;
    QueryPerformanceCounter(&c);
    unsigned long long ticks = (unsigned long long)c.QuadPart;
    unsigned long long f = (unsigned long long)freq.QuadPart;
    return (long long)((ticks / f) * 1000000000ULL + (ticks % f) * 1000000000ULL / f);
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000000000LL + ts.tv_nsec;
#endif
}

#define N 400
#define REPEATS 35

double a[N][N];
double b[N][N];
double c[N][N];

int main(void) {
    long long i, j, k, r;

    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
            a[i][j] = (double)(i + j);
            b[i][j] = (double)(i - j);
        }
    }

    /* Warm-up: a cheap pass over a and b (the matmul read pattern) before timing. */
    double warm = 0.0;
    for (i = 0; i < N; i++) {
        double s = 0.0;
        for (k = 0; k < N; k++) {
            s += a[i][k] * b[k][i];
        }
        warm += s;
    }

    long long t0 = monotonic_ns();

    double acc = 0.0;
    for (r = 0; r < REPEATS; r++) {
        a[r % N][r % N] += 1.0;
        for (i = 0; i < N; i++) {
            for (j = 0; j < N; j++) {
                double sum = 0.0;
                for (k = 0; k < N; k++) {
                    sum += a[i][k] * b[k][j];
                }
                c[i][j] = sum;
            }
        }
        acc += c[r % N][r % N];
    }

    long long t1 = monotonic_ns();

    printf("%lld, %.1f\n", t1 - t0, acc + warm);
    return 0;
}
