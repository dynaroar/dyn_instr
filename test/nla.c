
int main() {
    int a, n, x, y, z, k;
    n = 0;
    x = 0;
    y = 1;
    z = 6;
    while (1) {
        /* while (6 * n + 6 <= k) { */
        /* while (3 * n * n + 3 * n + 1 <= k) { */
        while (3 * n * n + 3 * n + 1 <= k) {
            n = n + 1;
            x = x + y;
            y = y + z;
            z = z + 6;
        }
        y = 0;
    }
    return 0;
}
