package com.xrefactory.refactorings;

/*
  1.)   Put  cursor   inside  TARGET   class  and   invoke   'Xref  ->
  Refactor'. Select 'Set Target  for Next Moving Refactoring' from the
  proposed menu.


  2.)  Move  cursor onto  the definition of  the method to  move (onto
  'method' name), invoke 'Xref ->  Refactor' once more time and select
  'Move Static Method'.

*/

class Target {
    static int i=0;
    // move method here:
    
}

class Source {
    static int j=1;

    // cursor has to be on 'method' name.
    public static void method() {
        System.out.println("i, j == " + Target.i + ", " + j);
    }
}

class MoveStaticMethod {

    public static void main(String[] args) {
        Source.method();
    }

}


/*
  (multiple) F5 will bring you back to Index
*/

