.PS                            # Pic input begins with .PS
cct_init                       # Set defaults

elen = 0.75                    # Variables are allowed; default units are inches
move right 0.4
Origin: Here                   # Position names are capitalized
   svgLink(SourcesSVG.svg,source(up_ elen))
   llabel(-,v`'svg_sub(s),+)
   svgLink(CctTableSVG.svg,resistor(right_ elen));  rlabel(,R,)
   dot
   {                           # Save current position and direction
      capacitor(down_ to (Here,Origin))     #(Here,Origin) = (Here.x,Origin.y)
      rlabel(+,v,-); llabel(,,C)
      dot
      }                        # Restore position and direction
   line right_ elen*2/3
   {move right 0.2}
   inductor(down_ Here.y-Origin.y); rlabel(,L,); b_current("i")
   line to Origin
.PE                            # Pic input ends
