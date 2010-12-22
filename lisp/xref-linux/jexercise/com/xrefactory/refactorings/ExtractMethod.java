package com.xrefactory.refactorings;

/*
  Select marked  region with  mouse and extract  it into  method named
  'fib' using 'Xref -> Refactor' and 'Extract Method'.
*/

class ExtractMethod {

    public static void main(String[] args) {
        int i,n,x,y,t;
        n = Integer.parseInt(args[0]);
        // region begin
        x=0; y=1;
        for(i=0; i<n; i++) {
            t=x+y; x=y; y=t;
        }
        // region end
        System.out.println("" + n + "-th fib == " + x);
    }

}


/*
  F5 will bring you back to Index
*/

