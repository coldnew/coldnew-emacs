% Test.m4
.PS
# Gosper C curve

`define' c_curve {if $3==0 then { continue by ($1,$2) } else {
    c_curve( ($1-$2)/2, ($1+$2)/2, $3-1)
    c_curve( ($1+$2)/2, ($2-$1)/2, $3-1)}}

line from 0,4 to 0,4
c_curve(0,4,10)

.PE
