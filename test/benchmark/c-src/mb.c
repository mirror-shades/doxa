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
    return (long long)((unsigned long long)c.QuadPart * 1000000000ULL /
                       (unsigned long long)freq.QuadPart);
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000000000LL + ts.tv_nsec;
#endif
}

int main(void) {
    int count = 0;

    long long t0 = monotonic_ns();

    for (int y = 0; y < 2000; y++) {
        for (int x = 0; x < 2000; x++) {

            double cr = (x - 1000.0) / 500.0;
            double ci = (y - 1000.0) / 500.0;

            double zr = 0.0;
            double zi = 0.0;

            int iter = 0;

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

    printf("%lld, %d\n", t1 - t0, count);
    return 0;
}
