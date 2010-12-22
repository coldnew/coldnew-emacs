package com.xrefactory.refactorings;

/*
  Put cursor on the symbol to  rename and press F11 or invoke 'Xref ->
  Refactor'.  Then move to the appropriate refactoring in the proposed
  menu and press <return>.

  When asked enter new name for the symbol.

*/

class RenameSymbol {

    int j = 0;

    // rename the variable i
    void method() {
        for(int i=0; i<10; i++) System.out.print(" " + j + " " + i);
    }

    // you can rename virtual methods as well, in preference the whole hierarchy.
    // In proposed resolution screen press <return> to continue
    class Super {
        void vmethod() {}
    }
    class Infer extends Super {
        void vmethod() {}
    }

    // if you rename x to j a name collision will be reported
    void method() {
        for(int x=0; x<10; x++) System.out.print(" " + j + " " + x);
    }


}


/*
  F5 will bring you back to Index
*/

