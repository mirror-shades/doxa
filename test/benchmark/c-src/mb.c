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

int main(void) {
    /* Warm-up: a smaller mandelbrot pass before timing. */
    long long warm = 0;
    for (long long y = 0; y < 256; y++) {
        for (long long x = 0; x < 256; x++) {
            double cr = (x - 128.0) / 64.0;
            double ci = (y - 128.0) / 64.0;
            double zr = 0.0;
            double zi = 0.0;
            long long iter = 0;
            while (zr * zr + zi * zi < 4.0 && iter < 1000) {
                double tmp = zr * zr - zi * zi + cr;
                zi = 2.0 * zr * zi + ci;
                zr = tmp;
                iter++;
            }
            warm += iter;
        }
    }

    long long count = 0;

    long long t0 = monotonic_ns();

    for (long long y = 0; y < 2000; y++) {
        for (long long x = 0; x < 2000; x++) {

            double cr = (x - 1000.0) / 500.0;
            double ci = (y - 1000.0) / 500.0;

            double zr = 0.0;
            double zi = 0.0;

            long long iter = 0;

            while (zr * zr + zi * zi < 4.0 && iter < 1000) {
                double tmp = zr * zr - zi * zi + cr;
                zi = 2.0 * zr * zi + ci;
                zr = tmp;
                iter++;
            }

            count += iter;
        }
    }

    long long t1 = monotonic_ns();

    printf("%lld, %lld\n", t1 - t0, count + warm);
    return 0;
}
