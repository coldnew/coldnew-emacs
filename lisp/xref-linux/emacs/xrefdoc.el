;;; xref.el - (X)Emacs interface to Xrefactory (documentation)

;; Copyright (C) 1997-2007 Marian Vittek, Xref-Tech
 
;; This  file  is  part  of  Xrefactory  software;  it  implements  an
;; interface  between  xref  task   and  (X)Emacs  editors.   You  can
;; distribute it  under the  terms of the  GNU General  Public License
;; version 2 as published by the Free Software Foundation.  You should
;; have received a  copy of the GNU General  Public License along with
;; this program; if not, write  to the Free Software Foundation, Inc.,
;; 59  Temple Place  - Suite  330,  Boston, MA  02111-1307, USA.   The
;; content of  this file is  copyrighted by Xref-Tech. This  file does
;; not contain any code  written by independent developers.  Xref-Tech
;; reserves  all rights  to make  any  future changes  in this  file's
;; license conditions.

;; The  GNU  GPL  license  applies  only  to  the  files  xref.el  and
;; xrefactory.el.    Distribution  of   other  parts   of  Xrefactory,
;; especially of the  xref executable and its source  code is governed
;; by the Xrefactory License Agreement.

;; This  file   may  invoke   functions  which  were   implemented  by
;; independent developers.  All credit for those  functions belongs to
;; the   authors,  contributors  and   maintainers  of   the  packages
;; containing  those functions.  This  concerns mainly  the 'compile',
;; 'comint', 'vc' and 'browse-url' packages.  Xrefactory is not an IDE
;; but  we have  included the  "Emacs IDE"  submenu  into Xrefactory's
;; (X)Emacs interface  in order  to allow users  of those  packages to
;; profit from  Xrefactory's project  management.  Of course,  we also
;; acknowledge the developers and maintainers of GNU Emacs and XEmacs.

(provide 'xrefdoc)

(defun xref-refactoring-documentation ()
  (let ((res))
    (setq res "


----------------------------------------------------------
* Rename Symbol: 
* Rename Class: 
* Rename Package:
----------------------------------------------------------


Description: Change the name of a program symbol

Example:

  Before refactoring:

        public static void main(String[] args) {
               for(int a=0; a<args.length; a++) {
                       action(args[a]);
               }
        }

  After refactoring:

        public static void main(String[] args) {
               for(int i=0; i<args.length; i++) {
                       action(args[i]);
               }
        }

Refactoring Context: Cursor has to be on the symbol.

Input parameters: New name of the symbol (for example: 'i')

Mechanics:  Rename  all occurrences  of  the  symbol in  the
      project.   When the  symbol is  the name  of  a public
      class stored in  a file of the same  name, the file is
      also  renamed.   When the  symbol  is  the  name of  a
      package, the package directory is also renamed.






----------------------------------------------------------
* Add Parameter:
----------------------------------------------------------

Description: Add parameter to a method, function or macro.

Example:

        Before refactoring:

            public int method(int x) {
                   if (x<=1) return(1);
                   return(method(x-1)+method(x-2));
            }

        After refactoring:

            public int method(int x, int y) {
                   if (x<=1) return(1);
                   return(method(x-1, 0)+method(x-2, 0));
            }


Refactoring  Context:  Cursor  has  to be  on  the  method's
      (function's or macro's) name.

Input  parameters:  Position   of  the  new  parameter,  its
      declaration  and default  value.   (for example:  '2',
      'int y' and '0').

Mechanics: Inspect all references to the method (function or
      macro) and  add a declaration of the  new parameter to
      each definition and default value to each invocation.







----------------------------------------------------------
* Delete Parameter:
----------------------------------------------------------

Description: Delete parameter of a method, function or macro.

Example:

        Before refactoring:

            public int method(int x, int y) {
                   if (x<=1) return(1);
                   return(method(x-1, 0)+method(x-2, 0));
            }

        After refactoring:

            public int method(int x) {
                   if (x<=1) return(1);
                   return(method(x-1)+method(x-2));
            }


Refactoring  Context:  Cursor  has  to be  on  the  method's
      (function's or macro's) name.

Input parameters:  Position of the parameter  to delete (for
      example: '2').

Mechanics: Inspect all references to the method (function or
      macro) and remove the parameter.





----------------------------------------------------------
* Move Parameter:
----------------------------------------------------------

Description: Reorder parameter of a method, function or macro.

Example:

        Before refactoring:

            public int method(int x, int y) {
                   if (x<=1) return(1);
                   return(method(x-1, 0)+method(x-2, 0));
            }

        After refactoring:

            public int method(int y, int x) {
                   if (x<=1) return(1);
                   return(method(0, x-1)+method(0, x-2));
            }


Refactoring  Context:  Cursor  has  to be  on  the  method's
      (function's or macro's) name.

Input  parameters: Old  and new  positions of  the parameter
      (for example: '1' and '2').

Mechanics: Inspect all references to the method and move the
      parameter  from  its  original  position  to  its  new
      position.







----------------------------------------------------------
* Extract Method: 
* Extract Function: 
* Extract Macro:
----------------------------------------------------------

Description: Extract region into new method (function or macro).
Example:

        Before refactoring:

            public static void main(String[] args) {
                   int i,n,x,y,t;
                   n = Integer.parseInt(args[0]);
                   x=0; y=1;
                   for(i=0; i<n; i++) {
                            t=x+y; x=y; y=t;
                   }
                   System.out.println(\"\" + n + \"-th fib == \" + x);
            }

        After refactoring:

            static int fib(int n) {
                   int i, x, y, t;
                   x=0; y=1;
                   for(i=0; i<n; i++) {
                            t=x+y; x=y; y=t;
                   }
                   return(x);
            }


            public static void main(String[] args) {
                   int i,n,x,y,t;
                   n = Integer.parseInt(args[0]);
                   x = fib(n);
                   System.out.println(\"\" + n + \"-th fib == \" + x);
            }


Refactoring  Context:  The code  for  extraction  has to  be
      selected in the editor.


Input  Parameters:  Name  of  the new  method  (function  or
      macro).

Mechanics:  Move the  region out  of the  method (function),
      generate wrapper code based  on static analysis of the
      original context and generate a call to the new method
      from the code's original position.




----------------------------------------------------------
* Expand Names: 
----------------------------------------------------------

Description: Expand types to fully qualified names
Example:

        Before refactoring:

            package com.xrefactory.refactorings;
            import javax.swing.*;
            class Hello {
                public static void main(String argv[]) {
                    JOptionPane.showMessageDialog(null, \"Hello world\");
                }
            }


        After refactoring:

            package com.xrefactory.refactorings;
            import javax.swing.*;
            class Hello {
                public static void main(java.lang.String argv[]) {
                    javax.swing.JOptionPane.showMessageDialog(null, \"Hello world\");
                }
            }


Refactoring Context: Cursor has to be on a class definition.

Input Parameters: None.

Mechanics:  Replace  short  type  names by  fully  qualified
      names.




----------------------------------------------------------
* Reduce Names: 
----------------------------------------------------------

Description: Reduce fully qualified type names to short forms.
Example:

        Before refactoring:

            package com.xrefactory.refactorings;
            class Hello {
                public static void main(java.lang.String argv[]) {
                    javax.swing.JOptionPane.showMessageDialog(null, \"Hello world\");
                }
            }

        After refactoring:

            package com.xrefactory.refactorings;
            import javax.swing.JOptionPane;
            class Hello {
                public static void main(String argv[]) {
                    JOptionPane.showMessageDialog(null, \"Hello world\");
                }
            }


Refactoring Context: Cursor has to be on a class definition.

Input Parameters: None.

Mechanics: Replace fully qualified names by short names.  If
      the  type is not  imported then  add either  import on
      demand or single type import command.




----------------------------------------------------------
* Set Target for Next Moving Refactoring:
----------------------------------------------------------

Description: Set target position for moving refactorings.

Refactoring Context: Cursor has  to be on the position where
      the field, method or class will be moved, pulled up or
      pushed down.

Input Parameters: None.

Mechanics: This action does  not modify source code. It sets
      some internal values for future refactoring.



----------------------------------------------------------
* Move Static Field: 
* Move Static Method:
----------------------------------------------------------

Description: Move a static field or method to another place.
Example:

        Before refactoring:

        class Target {
            static int i=0;
        }               

        class Source {
            static int j=1;
            public static void method() {
                    System.out.println(\"i, j == \" + Target.i + \", \" + j);
            }
            public static void main(String[] args) {
                    method();
            }
        }


        After refactoring:

        class Target {
            static int i=0;
            public static void method() {
                 System.out.println(\"i, j == \" + i + \", \" + Source.j);
            }
        }

        class Source {
            static int j=1;
            public static void main(String[] args) {
                Target.method();
            }
        }


Refactoring Context:  Target position has to  set using 'Set
      Target Position', cursor has to  be on the name of the
      method (field) to be moved (at its definition).

Input Parameters: None.

Mechanics:  Move the method  (field), adjust  all references
      within   its  body   (initialisation)   and  all   its
      references in the project.





----------------------------------------------------------
* Move Class:
----------------------------------------------------------

Description: Move a class from one place to another.

Example:
        Before refactoring:

            class A {
                static int i;
                static class B {
                    static void method() {
                        System.out.println(\"i==\" + i);
                    }
                }
            }

        After refactoring:

            class A {
                static int i;   
            }

            class B {
                static void method() {
                    System.out.println(\"i==\" + A.i);
                }
            }

Refactoring Context:  Target position has to  set using 'Set
      Target Position', cursor has to  be on the name of the
      class to be moved (at its definition).

Input Parameters: None.

Mechanics: Move the class and adjust all its references.





----------------------------------------------------------
* Move Class to New File:
----------------------------------------------------------

Description: Move a class into its own file.

Example:
        Before refactoring:

---- file A.java
            package pack;

            class A {
                static int i;
                static class B {
                    static void method() {
                        System.out.println(\"i==\" + i);
                    }
                }
            }

        After refactoring:

---- file A.java
            package pack;

            class A {
                static int i;   
            }

---- file B.java
            package pack;

            public class B {
                static void method() {
                    System.out.println(\"i==\" + A.i);
                }
            }

Refactoring Context:  Cursor has  to be on  the name  of the
      class to be moved (at its definition).

Input Parameters: Name of the file to create.

Mechanics: Create new file, add package and imports and move
      the class and adjust all its references.





----------------------------------------------------------
* Move Field:
----------------------------------------------------------

Description: Move a field from one class to another
Example:
        Before refactoring:

            class Target {
    
            }

            class Source {
                  Target link;
                  int field;
                  public int method() {
                         return(field);
                  }
            }

        After refactoring:

            class Target {
                  int field;
            }
    
            class Source {
                  Target link;
                  public int method() {
                         return(link.field);
                  }
            }

Refactoring Context:  Target position has to  set using 'Set
      Target Position', cursor has to  be on the name of the
      field to be moved (at its definition).

Input Parameters:  the field  pointing from source  class to
      target class (in the example: 'link').

Mechanics: Move  the field,  inspect all its  references add
      insert the field pointing from source to target.

Comment: This  refactoring relies on  semantic properties of
      the program;  it cannot be guaranteed to  be 100% safe
      by static analysis of the program.






----------------------------------------------------------
* Pull Up Field: 
* Pull Up Method: 
* Push Down Field: 
* Push Down Method:
----------------------------------------------------------

Description:  Move a method/field  up or  down in  the class
      hierarchy.

Example:
        Before refactoring:

            class SuperClass {
                int x = 0;
                int y = 0;
            
            }
            
            class InferClass extends SuperClass {
                int y = 1;
            
                void method() {
                    System.out.println(\"x == \" + x);
                    System.out.println(\"this.x == \" + this.x);
                    System.out.println(\"this.y == \" + this.y);
                    System.out.println(\"super.y == \" + super.y);
                }
            
                public static void main(String args[]) {
                    (new InferClass()).method();
                }
            }

        After refactoring:

            class SuperClass {
                int x = 0;
                int y = 0;
            
                public void method() {
                    System.out.println(\"x == \" + x);
                    System.out.println(\"this.x == \" + this.x);
                    System.out.println(\"this.y == \" + ((InferClass)this).y);
                    System.out.println(\"super.y == \" + this.y);
                }            
            }

            class InferClass extends SuperClass {
                int y = 1;
            
                public static void main(String args[]) {
                    (new InferClass()).method();
                }
            }

Refactoring  Context: Target  position has  to be  set using
      'Set Target Position', cursor has to be on the name of
      the method (field) to be moved (at its definition).

Input Parameters: None.

Mechanics: Move the method  and adjust references inside its
      body (initialisation).





----------------------------------------------------------
* Encapsulate Field:
* Self Encapsulate Field:
----------------------------------------------------------

Description: Generate field accessors and their invocations.

Example:
        Before refactoring:

            class EncapsulateField {
                public int field;
            
                void incrementField() {
                    field = field + 1;
                }
            }

            class AnotherClass extends EncapsulateField {
                void printField() {
                    System.out.println(\"field == \" + field);
                }
            }

        After refactoring:

            class EncapsulateField {
                private int field;
                public int getField() {return field;}
                public int setField(int field) { this.field=field; return field;}
            
                void incrementField() {
                    setField(getField() + 1);
                }
            }
            
            class AnotherClass extends EncapsulateField {
                void printField() {
                    System.out.println(\"field == \" + getField());
                }
            }

Refactoring Context:  Cursor has  to be on  the name  of the
      field to encapsulate (on its definition).

Input Parameters: None.

Mechanics: Generate  getter and setter  methods. Inspect all
      references   of  the   field  and   replace   them  by
      appropriate  accessor.   Self  Encapsulate field  also
      processes  references within  the  class defining  the
      field  (see example).  Encapsulate  Field leaves  such
      references untouched.




----------------------------------------------------------
* Make Virtual Method Static:
----------------------------------------------------------

Description: Make a virtual method static.

Example:
        Before refactoring:

            class Project {
                Person[] participants;
            }

            class Person {
                int id;
                public boolean participate(Project proj) {
                    for(int i=0; i<proj.participants.length; i++) {
                        if (proj.participants[i].id == id) return(true);
                    }
                    return(false);
                }   
            }


        After refactoring:

            class Project {
                Person[] participants;
                static public boolean participate(Person person, Project proj) {
                    for(int i=0; i<proj.participants.length; i++) {
                        if (proj.participants[i].id == person.id) return(true);
                    }
                    return(false);
                }   
            }
            
            class Person {
                int id;
            }

Refactoring Context: Cursor has to on the name of the method
      to be made static (on its name).

Input  Parameters: The  name of  the new  parameter  (in the
      example: 'person').

Mechanics:  Add  a  new  parameter  to the  method  for  its
      invocation  object.   Make  all accesses  to  method's
      object  via  the new  parameter.   Declare the  method
      static and make all method invocations static.

Comment: This refactoring is  usually used to move a virtual
      method  from one  class to  another with  the sequence
      'Make  Virtual Method  Static', 'Move  Static Method',
      'Make Static Method Virtual'.




----------------------------------------------------------
* Make Static Method Virtual:
----------------------------------------------------------

Description: Make a static method virtual.
Example:

        Before refactoring:

            class Target {
                int field;
                static int method(Source ss) {
                    System.out.println(\"field==\" + ss.link.field);
                }
            }

            class Source {
                Target link;
                static void main(String[] args) {
                    Target.method(new Source());
                }
            }

        After refactoring:

            class Target {
                int field;
                int method() {
                    System.out.println(\"field==\" + field);
                }
            }

            class Source {
                Target link;
                static void main(String[] args) {
                    new Source().link.method();
                }
            }


Refactoring Context: Cursor has to on the name of the method
      to be made virtual (on its name).

Input   Parameters:   The   parameter  containing   method's
      object.  Optionally a  field  getting method's  object
      from the parameter. In the example: '1' and \"link\".

Mechanics:   Remove  the   'static'  keyword,   inspect  all
      references  and apply  method to  the  method's object
      (method's object is determined from the combination of
      parameter and field).

Comment: This refactoring is  usually used to move a virtual
      method  from one  class to  another with  the sequence
      'Make  Virtual Method  Static', 'Move  Static Method',
      'Make Static Method Virtual'.




----------------------------------------------------------
* Undo Last Refactoring:
----------------------------------------------------------

Description: Undo sequence of refactorings.

Mechanics:  Undo  all changes  made  in  buffers  up to  and
      including the  last refactoring. Optionally  also undo
      previous refactorings.


")
    res
))

