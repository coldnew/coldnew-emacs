.PS
# `Fuses.m4'
cct_init

movewid = 0.25

 {fuse ; {"`fuse'" at last line.c+(0,-0.2)}
  move
  fuse(,D) ; {"`fuse(,D)'" at last line.c+(0,-0.2)}
  move
  fuse(,B) ; {"`fuse(,B)'" at last line.c+(0,-0.2)}
  move
  fuse(,C) ; {"`fuse(,C)'" at last line.c+(0,-0.2)}
  move
  fuse(,S) ; {"`fuse(,S)'" at last line.c+(0,-0.2)}
  move
  fuse(,HB) ; {"`fuse(,HB)'" at last line.c+(0,-0.2)}
  }
  move down; right_
  fuse(,HC) ; {"`fuse(,HC)'" at last line.c+(0,-0.25)}
  move
  fuse(,HC,0.5) ; {"`(,HC,0.5)'" at last line.c+(0,-0.25)}
  move
  fuse(,HC,0.5,0.3) ; {"`(,HC,0.5,0.3)'" at last line.c+(0,-0.25)}
  move
  cbreaker; {"`cbreaker'" at last line.c+(0,-0.25)}
  move
  cbreaker(,R); {"`cbreaker(,R)'" at last line.c+(0,-0.25)}
  move
  cbreaker(,,D); {"`...(,,D)'" at last line.c+(0,-0.25)}

.PE
