
/*
  Put cursor  on a symbol you wish  to rename and press  F11 or invoke
  'Xref -> Refactor'. In the proposed menu move to the 'Rename Symbol'
  refactoring and  press <return>.  If  a name collision  is detected,
  use 'Xref -> Undo Last Refactoring' to undo wrong renaming.  
*/

static int j;

void renameSymbol() {
    int i,k;

    // rename local variable 'i'
    for(i=0; i<10; i++) printf(" %d", i);
    printf("\n");

    // rename the 'renameSymbol' function
    if (0) renameSymbol();

#define PRINTJ() printf("j == %d\n", j)

    // works inside macros, rename for example 'j'
    j = 33; PRINTJ();


    // you can rename any kind of symbol, a macro parameter for example
#define PRINT(renameMe) printf("%d\n", renameMe)

    // renaming 'k' to 'x' will cause name collision
    k = 0;
    {
        int x; x = k; printf("x==%d\n", x);
    }

#define PRINTX() printf("x == %d\n", x);
    // problem can  occur also if a symbol inside  a macro is refering
    // to  various different variables.  Rename  for example following
    // 'x' variable
    {
        int x = 0; 
        PRINTX(); 
    }

}

int main() {
    int x = 1;
    renameSymbol();
    PRINTX();
}


/*
  (multiple) F5 will bring you back to index
*/
