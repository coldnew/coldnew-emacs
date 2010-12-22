#include <stdio.h>

/*
  Select marked  region with  mouse and press  F11 or invoke  'Xref ->
  Refactor'.   In the  proposed menu  move to  the  'Extract Function'
  refactoring and press <return>.  
*/

void extractFunction() {
    int i,n,x,y,t;
    printf("Enter n: ");
    fflush(stdout);
    fscanf(stdin, " %d", &n);
    // region begin
    x=0; y=1;
    for(i=0; i<n; i++) {
        t=x+y; x=y; y=t;
    }
    // region end
    printf("%d-th fib == %d\n", n, x);
}

int main() {
    extractFunction();
    return(0);
}

/*
  F5 will bring you back to Index
*/

