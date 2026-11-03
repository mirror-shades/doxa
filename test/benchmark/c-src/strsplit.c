#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
    char *bytes;
    size_t len;
} Part;

/* Each part is its own allocation, as on the Doxa side. */
static Part *split(const char *text, size_t len, size_t *count) {
    size_t cap = 16;
    size_t n = 0;
    Part *parts = malloc(cap * sizeof(Part));
    size_t start = 0;
    for (size_t i = 0; i <= len; i++) {
        if (i == len || text[i] == ',') {
            if (n == cap) {
                cap *= 2;
                parts = realloc(parts, cap * sizeof(Part));
            }
            size_t part_len = i - start;
            parts[n].bytes = malloc(part_len == 0 ? 1 : part_len);
            memcpy(parts[n].bytes, text + start, part_len);
            parts[n].len = part_len;
            n++;
            start = i + 1;
        }
    }
    *count = n;
    return parts;
}

/* Appends into a buffer that doubles when full. */
static char *join(const Part *parts, size_t count, size_t *out_len) {
    size_t cap = 8;
    size_t len = 0;
    char *out = malloc(cap);
    for (size_t i = 0; i < count; i++) {
        size_t need = len + parts[i].len + (i > 0 ? 1 : 0);
        while (need > cap) {
            cap *= 2;
            out = realloc(out, cap);
        }
        if (i > 0) out[len++] = ',';
        memcpy(out + len, parts[i].bytes, parts[i].len);
        len += parts[i].len;
    }
    *out_len = len;
    return out;
}

/* Split a text on commas, then join it back. */
int main(void) {
    const char *words[6] = {"red", "green", "blue", "cyan", "magenta", "yellow"};
    const long long count = 1700;
    const long long rounds = 50;
    size_t text_cap = 16;
    size_t text_len = 0;
    char *text = malloc(text_cap);
    for (long long i = 0; i < count; i++) {
        const char *word = words[(i * 5) % 6];
        size_t word_len = strlen(word);
        while (text_len + word_len + 1 > text_cap) {
            text_cap *= 2;
            text = realloc(text, text_cap);
        }
        if (i > 0) text[text_len++] = ',';
        memcpy(text + text_len, word, word_len);
        text_len += word_len;
    }
    long long checksum = 0;

    long long t0 = monotonic_ns();

    for (long long r = 0; r < rounds; r++) {
        size_t part_count = 0;
        Part *parts = split(text, text_len, &part_count);
        size_t back_len = 0;
        char *back = join(parts, part_count, &back_len);
        checksum += (long long)part_count + (long long)back_len;
        if (back_len == text_len && memcmp(back, text, text_len) == 0) checksum += 1;
        for (size_t i = 0; i < part_count; i++) free(parts[i].bytes);
        free(parts);
        free(back);
    }

    long long t1 = monotonic_ns();

    printf("%lld, %lld\n", t1 - t0, checksum);
    free(text);
    return 0;
}
