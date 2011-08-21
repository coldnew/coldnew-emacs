% ex10.m4
.PS
cct_init
dt = 360/5
r = 0.75

N0: dot(at (Rect_(r,90)))
N1: dot(at (Rect_(r,(90-dt))))
N2: dot(at (Rect_(r,(90-2*dt))))
N3: dot(at (Rect_(r,(90-3*dt))))
N4: dot(at (Rect_(r,(90-4*dt))))

  line from N0 to N1 then to N2 then to N3 then to N4 then to N0
L14: line from N1 to N4
L24: line from N2 to N4
C02: crossover(from N0 to N2,,L14)
C13: crossover(from N1 to N3,,C02,L24)
C03: crossover(from N0 to N3,,L14,L24)

.PE
