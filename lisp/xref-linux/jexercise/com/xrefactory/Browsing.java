package com.xrefactory;

import java.awt.*;
import javax.swing.*;

/*
  Follow  instructions  in  commentaries  to check  Xrefactory  source
  browser.
*/

class Browsing {

    void simplest() {
        int i;

        // put  cursor somewhere  on 'i' variable  and press  F6. Then
        // using  F3 and  F4 inspect all  its references.   Finaly pop
        // references by pressing F5.
        for(i=0; i<10; i++) System.out.println("this.i, i == " + this.i + i);
    }

    class Nested extends NestedSuperClass {
        void resolution() {
            int i = 2;

            // put cursor on following usages of variables i, j, k and
            // check if Xrefactory resolves them correctly by pressing
            // F6 followed by F5.
            System.out.println("i == " + i);              // i==2
            System.out.println("j == " + this.j);         // j==1
            System.out.println("k == " + k);              // k==0

            // check Xrefactory's overloading resolution.
            overloadedMethod(0);
            overloadedMethod("foo");
            overloadedMethod(this);
            overloadedMethod((NestedSuperClass)this);

        }
    }

    class NestedSuperClass {
        int i=1, j=1;
        private int k=1;
    }

    int i=0, j=0, k=0;

    void htmlBrowsing() {

        // When browsing a symbol  not defined in current project, but
        // with javadoc available, then a HTML browser is invoked.
        // For example, put cursor on 'JButton' and press F6.
        // (This option requires correct setting of javadocpath).
        JButton bb = new JButton("foo");

    }

    void usageLists() {

        // list all usages of 'k' by putting cursor on it and pressing
        // C-F6 key combination (i.e. control F6).
        k = 0;
        /*
          'F3' and 'F4' inspect previous-next reference.
          in created (right) window:
            'mouse-button12' goes to selected reference.
            'mouse-button3' pops up menu.
          always:    
            'F7' closes Xrefactory window.
        */

        // Now, put cursor on  'getComponentAt' and press C-F6 to list
        // all references of a virtual method
        component.getComponentAt(0,0);

        /*
          The left  window shows  how the method  is inherited  in the
          class tree.  It shows also how many usages of the method in your
          project are syntacticaly related to those classes.

          In left window:
            <space>         - select one class and move to the definition
            <Ins>           - toggle select/unselect
            mouse-button12  - select one class and move to the definition
            mouse-button3   - pop-up menu
          As usualy F7 closes new windows

        */

        // A  similar  dialog  appears  when  trying to  move  to  the
        // definition  (using   F6)  of  a  virtual   method  and  the
        // definition  can't  be  determined  by  static  analysis  of
        // program.
        component.getComponentAt(0,0);

        // this ambiguity does not occur in following case
        mylist.getComponentAt(0,0);

        // nor in following
        jtree.getComponentAt(0,0);

        // you can see field/method applications w.r.t. to inheritance
        // tree of  any browsed symbol by pushing  its references with 
        // C-F6.

        (new Nested()).j = 0;


        /*
          Presentation of  Xrefactory browser  is over, F7  will close
          Xrefactory's windows  and (probably multiple)  F5 will bring
          you back to Index.
        */
    }

    Component   component;
    JTree       jtree;
    MyList      mylist;

    class MyList extends JList {
        public Component getComponentAt(int x, int y) {
            Component res = null;
            // ...
            return(res);
        }
    }

    void overloadedMethod(int i) {
        System.out.println("overloadedMethod(int)");
    }
    void overloadedMethod(String s) {
        System.out.println("overloadedMethod(String)");
    }
    void overloadedMethod(Nested n) {
        System.out.println("overloadedMethod(Nested)");
    }
    void overloadedMethod(NestedSuperClass ns) {
        System.out.println("overloadedMethod(NestedSuperClass)");
    }

    public void setK(int k) {this.k = k;}
    public int getK() {return(k);}

    public static void main(String[] args) {
        Browsing bb = new Browsing();
        Nested nn = bb.new Nested();
        NestedSuperClass ss = bb.new NestedSuperClass();
        nn.resolution();
        bb.k = 0;
    }

}

