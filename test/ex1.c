extern void reach_error();

void main() {
    int x;
    // int y = x;
    if (x * x > 100) {
        //x = x*x + 1;
        return;
    }
    else
        reach_error();
}