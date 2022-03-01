extern int reach_error();

int main() {
    int x;
    int y = x * x + 100;
    if (y > 100) {
        return 0;
    } else {
        reach_error();
    }
}