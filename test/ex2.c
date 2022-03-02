extern void reach_error();

int main() {
    int x;
    while (x * x > 100) {
        if (x <= 10 || x >= -10)
            reach_error();
    }
}