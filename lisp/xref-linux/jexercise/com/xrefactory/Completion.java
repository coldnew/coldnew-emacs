package com.xrefactory;

import java.util.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/*
 Check   completion   on    following   demonstration   lines!    Each
 demonstration   line  is   preceded   by  one   line  of   explaining
 commentary. Put cursor  at the end of demonstration  lines and invoke
 completion by pressing F8.
*/

class Completion extends JFrame {

    public static void main(String[] arguments) {

        // local variable completion
        arg
        ;
           
        // composed name completion
        java.ap
        ;

        // class name, multiple completions listed
        La
        ;
        /* In the proposed list:
             <return>            - select the completion.
             <space>             - inspect definition (or javadoc)
             C-q                 - return and close completion window
             letter & digits     - incremental search
             other characters    - leave completion window
             <escape>            - close completion window, no completion
           also:
             mouse-button12      - select the completion.
             mouse-button3       - pop-up menu for this item.
           Everywhere:
             F7                  - close Xrefactory's window
        */

        // method completion displays profile
        System.out.pr
        ;

        // completion of empty string at variable declaration place proposes 
        // the most often alternatives
        Button 
        ;

        // empty string after 'new' in an assignement is completed to class name
        Button mybutton = new 
        ;

        // completion on complete name displays its type
        arguments
        ;

        // inherited symbols are displayed with level of inheritance 
        // and origin class
        new JFrame().get
        ;

        // completion works after expressions
        ((String)(new Vector(new Collection().add("Foo"))).get(0)).to
        ;

        // works for constructors too
        new Button
        ;

        class Nested extends JButton implements ActionListener {
            int app;
            void foo() {
                
                // no problem with nested classes
                get
                ;
            }

            Nested() {
                // or explicit constructor invocations
                super
                ;
            }
            
            // when completing empty string on method definition context
            // inheritance wizard proposes inherited definitions 
    
            
    
    }

}


/*
  When  creating a new  file, you  can complete  full package  name by
  completing empty string after the 'package' keyword.

*/


/*
  F5 will bring you back to Index
*/
