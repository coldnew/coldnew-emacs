#include <stdio.h>

/*
  Follow  instructions  in  commentaries  to check  Xrefactory  source
  browser.
*/


void browsing() {
    struct ilist {int i;    struct ilist *next;} *il;
    struct dlist {double d; struct dlist *next;} *dl;

    int i,j,k;

    // put cursor  somewhere on 'i' variable and  press F6. Then using
    // F3 and F4 inspect all its references.  Finaly pop references by
    // pressing F5.
    for(i=0; i<10; i++) printf("i == %d\n", i );

    // put cursor  on following usages of 'i' and  'j' and check scope
    // resolution by pressing F6 followed by F5.
    { int i;
      i = i + 1;
      j = j + 1;
    }

    // structure records can be  browsed as well, browse 'next' or 'i'
    // records for example.
    il = il-> next ;

    il-> i = 0;

    // list all usages of this 'i' by pressing C-F6 (control F6);
    i = 0;
    /*
      'F3' and 'F4' inspect previous-next reference.
      in created (right) window:
        'mouse-button12' goes to selected reference.
        'mouse-button3' pops up menu.
      always:    
        'F7' closes Xrefactory window.
    */


    // browse  'next' symbol from  following line to  check resolution
    // inside used macros
#define LISTLEN(ll,res) {res=0; for(; ll!=NULL; ll=ll-> next ) res++;}

    LISTLEN(dl, k);

    // ambiguity  handling: put cursor  on 'next' symbol and  list all
    // its usages with C-F6
#define LISTLOOP(ll,ii) for(ll=ii; ll!=NULL; ll=ll-> next )
    /*
      In the created (left) window:
        <space>     - select one symbol
        o           - toggle to other browser window
      also:
        'mouse-button12' select/unselect symbol.
        'mouse-button3' pops up menu.

      As usualy 'F7' closes new windows.
    */

    LISTLOOP(il, NULL) {}
    LISTLOOP(dl, NULL) {}


}

// put cursor on the 'include' keyword and move to the included file
// by pressing F6, then return back using F5.

#include <stdlib.h>

/*
  Presentation of Xrefactory browser is over, F7 will close Xrefactory
  window and (probably multiple) F5 will bring you back to index.  
*/



