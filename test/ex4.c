extern int reach_error();

int foo(int x) {
    return x;
}

int main() {
    int x;
    int y = foo(x*x + 100);
    if (y > 100) {
        return 0;
    } else {
        reach_error();
    }
}