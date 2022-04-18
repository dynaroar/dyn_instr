extern void reach_error();
extern int __VERIFIER_nondet_int();

void main() {
    int x = __VERIFIER_nondet_int();
    int y = x;
    if (x * y > 100) {
        //x = x*x + 1;
        return;
    }
    else
        reach_error();
}