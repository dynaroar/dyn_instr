extern int reach_error();

int main() {
    int x;
    int y = x * x;
    if (y > 100) {
        return 0;
    } else {
        reach_error();
    }
}