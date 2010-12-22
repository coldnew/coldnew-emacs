#include <stdio.h>

/*
  Put cursor on the FUNCTION NAME (not on the parameter) and press F11
  or  invoke  'Xref  ->  Refactor'.   Then  move  to  the  appropriate
  refactoring in the proposed menu and press <return>.  
*/

static int arg = 1;

// add another parameter to function printint
void printint(int j) {
    printf("j == %d", j);
}

// remove parameter of printNewline
void printNewline(int i) {
    printf("\n");
}

// removing of used parameter issues warning message
void printNewlines(int i) {
    for(; i>0; i--) printf("\n");
}

// adding a parameter named 'arg' here will report symbol clash
void printArg() {
    printf("arg == %d\n", arg);
}

void addRemoveParameter() {
    printint(1);
    printNewline(1);
}


/*
  (multiple) F5 will bring you back to index
*/
