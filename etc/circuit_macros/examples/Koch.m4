% Koch snowflake
.PS
gen_init

  leng = 4
  X: 3,5
  nsides = 3; rot[1] = 0; rot[2] = -120; rot[3] = -120
  depth=4

  for i=1 to depth do {
    for j=1 to nsides do { tmp[j] = rot[j] } 
    leng = leng/3
    k = 0
    for j=1 to nsides do {
      rot[k+1] = tmp[j]
      rot[k+2] = 60
      rot[k+3] = -120
      rot[k+4] = 60
      k += 4
      }
    nsides = k
    }

  a = 0
  line from X to X
  for i=1 to nsides do {
   a += rot[i]
   X: X+(Rect_(leng,a))
   continue to X
   }

.PE
