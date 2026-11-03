#include <stdio.h>
#include <stdlib.h>
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

typedef struct {
    const char *body;
    long long len;
} Doc;

static long long count_byte(const char *text, long long len, char wanted) {
    long long n = 0;
    for (long long i = 0; i < len; i++) {
        if (text[i] == wanted) n++;
    }
    return n;
}

/* The first "da" in each window, relative to the window, or -1, summed. */
static long long find_windows(const char *text, long long len, long long window) {
    long long total = 0;
    for (long long start = 0; start < len; start += window) {
        long long end = start + window < len ? start + window : len;
        long long found = -1;
        for (long long i = start; i + 1 < end; i++) {
            if (text[i] == 'd' && text[i + 1] == 'a') {
                found = i - start;
                break;
            }
        }
        total += found;
    }
    return total;
}

/* Byte scanning: a local, a parameter and a struct field, then a search in
   each window. The text cycles through "acebd". */
int main(void) {
    const long long size = 100000;
    const long long rounds = 20;
    char *text = malloc((size_t)size);
    for (long long i = 0; i < size; i++) text[i] = (char)(97 + (i * 7) % 5);
    Doc doc = {text, size};
    long long checksum = 0;

    long long t0 = monotonic_ns();

    for (long long r = 0; r < rounds; r++) {
        long long local = 0;
        for (long long i = 0; i < size; i++) {
            if (text[i] == 'e') local++;
        }
        checksum += local + count_byte(text, size, 'a') + count_byte(doc.body, doc.len, 'b') + find_windows(text, size, 64);
    }

    long long t1 = monotonic_ns();

    printf("%lld, %lld\n", t1 - t0, checksum);
    free(text);
    return 0;
}
