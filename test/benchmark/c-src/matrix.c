#include <stdio.h>

#define N 400

double a[N][N];
double b[N][N];
double c[N][N];

int main() {
    for(int i = 0; i < N; i++) {
        for(int j = 0; j < N; j++) {
            a[i][j] = i + j;
            b[i][j] = i - j;
        }
    }

    for(int i = 0; i < N; i++) {
        for(int j = 0; j < N; j++) {
            double sum = 0.0;

            for(int k = 0; k < N; k++) {
                sum += a[i][k] * b[k][j];
            }

            c[i][j] = sum;
        }
    }

    printf("%.0f\n", c[N-1][N-1]);

    return 0;
}