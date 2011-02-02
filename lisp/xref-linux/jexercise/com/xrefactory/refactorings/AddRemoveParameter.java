package com.xrefactory.refactorings;

/*
  Put cursor on  the METHOD NAME (not on the  parameter) and press F11
  or  invoke  'Xref  ->  Refactor'.   Then  move  to  the  appropriate
  refactoring in the proposed menu and press <return>.

  If  a  profile  collision  is  detected,  use  'Xref  ->  Undo  Last
  Refactoring' to undo wrong manipulation.

*/

class AddRemoveParameter {
    int arg = 1;

    // put cursor on 'printParameter' and add a new parameter
    void printParameter(int j) {
        System.out.println("j == " + j);
    }

    // works for constructors too, add new parameter to this constructor
    AddRemoveParameter() {}

    public static void main(String ss) {
        AddRemoveParameter pp = new AddRemoveParameter();
        pp.printParameter(1);
        pp.printParameter("toto");
    }

    // you can add/remove parameter on virtuals, 
    // in preference do it for the whole hierarchy at once.
    // in proposed resolution screen press <return> to continue
    class Super {
        void vmethod() {}
    }
    class Infer extends Super {
        void vmethod() {}
    }

    // deleting first parameter here will report profile clash
    void printParameter(int j, String x) {
        System.out.println("x == " + x);
    }

    void printParameter(String x) {
        System.out.println("x == " + x);
    }

    // inserting parameter 'int arg' will report symbol clash
    void printArg() {
        System.out.println("arg == " + arg);
    }

}

/*
  (multiple) F5 will bring you back to Index
*/
