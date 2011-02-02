package com.xrefactory.refactorings;

/*
  1.) Put cursor on definition of 'field'.

  2.)  Invoke 'Xref  -> Refactor'  and select  'Encapsulate  Field' in
  proposed menu.

  3.)  Undo last  refactoring and  proceed  one more  time with  'Self
  Encapsulate Field' to see where is the difference.

*/

class EncapsulateField {

    // put cursor on 'field' name before encapsulating
    public int field;

    void incrementField() {
        field = field + 1;
    }

}


class AnotherClass extends EncapsulateField {

    void printField() {
        System.out.println("field == " + field);
    }

    public static void main(String[] args) {
        EncapsulateField oo = new EncapsulateField();
        oo.field = 0;
        oo.printField();
    }

}

/*
  (multiple) F5 will bring you back to Index
*/
