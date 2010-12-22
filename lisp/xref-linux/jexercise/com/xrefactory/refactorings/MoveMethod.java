package com.xrefactory.refactorings;

/*

  Move method  'participate' from class  Person into class  Project by
  composing following three refactorings:

  1.)   Put cursor  on  'participate'  method and  make  it static  by
  pressing  F11   and  selecting  'Turn  Dynamic   Method  to  Static'
  refactoring.  When  prompted  for  the  new  parameter  name,  press
  <return> to accept default value.

  2.)   Move it into  class Project  using F11,  'Set Target  for Next
  Moving Refactoring' and F11, 'Move Static Method'.

  3.)   Turn it  back to  virtual method  using F11  and  'Turn Static
  Method to Dynamic'.  Determine  target object via 2-nd parameter and
  empty field name.

*/

class Project {
    Person[] participants;
    // set target position here

}

class Person {
    int id;
    // cursor has to be on method name
    public boolean participate(Project proj) {
        for(int i=0; i<proj.participants.length; i++) {
            if (proj.participants[i].id == id) return(true);
        }
        return(false);
    }   
    Person(int id) {this.id=id;}
}

class MoveMethod {
    public static void main(String[] args) {
        Person[] pparticipants = {new Person(5), new Person(7)};
        Project pp = new Project();
        pp.participants = pparticipants;
        System.out.println("part 3 "+ (new Person(3)).participate(pp));
        System.out.println("part 5 "+ (new Person(5)).participate(pp));
    }
}


/*
  (multiple) F5 will bring you back to Index
*/

