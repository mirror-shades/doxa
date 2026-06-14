#include <stdio.h>

int main() {
    int count = 0;

    for(int y = 0; y < 2000; y++) {
        for(int x = 0; x < 2000; x++) {

            double cr = (x - 1000.0) / 500.0;
            double ci = (y - 1000.0) / 500.0;

            double zr = 0.0;
            double zi = 0.0;

            int iter = 0;

            while(zr * zr + zi * zi < 4.0 && iter < 1000) {
                double tmp = zr * zr - zi * zi + cr;
                zi = 2.0 * zr * zi + ci;
                zr = tmp;
                iter++;
            }

            count += iter;
        }
    }

    printf("%d\n", count);

    return 0;
}