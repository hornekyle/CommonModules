title: PlPlotLib Escape Codes

Character Escape Codes
----------------------

Escape codes can be inserted into character input to the plotting 
routines in order to change the formatting of the text.
The supported codes are listed below:

+ `#u`: move up to the superscript position (ended with #d)
+ `#d`: move down to subscript position (ended with #u)
+ `#b`: backspace (to allow overprinting)
+ `##`: number symbol
+ `#+`: toggle overline mode
+ `#-`: toggle underline mode
+ `#gx`: Greek letter corresponding to Roman letter x (see below)
+ `#fn`: switch to normal (sans-serif) font
+ `#fr`: switch to Roman (serif) font
+ `#fi`: switch to italic font
+ `#fs`: switch to script font

When greek letters are inserted using the code '#gx', the replacements 
are made according to the following rules:

|= ROMAN =|= GREEK =| |= roman =|= greek =|
|:-:|:-:|-|:-:|:-:|
| A | Α | | a | α |
| B | Β | | b | β |
| G | Γ | | g | γ |
| D | Δ | | d | δ |
| E | Ε | | e | ε |
| Z | Ζ | | z | ζ |
| Y | Η | | y | η |
| H | Θ | | h | θ |
| I | Ι | | i | ι |
| K | Κ | | k | κ |
| L | Λ | | l | λ |
| M | Μ | | m | μ |
| N | Ν | | n | ν |
| C | Ξ | | c | ξ |
| O | Ο | | o | ο |
| P | Π | | p | π |
| R | Ρ | | r | ρ |
| S | Σ | | s | σ |
| T | Τ | | t | τ |
| U | Υ | | u | υ |
| F | Φ | | f | φ |
| X | Χ | | x | χ |
| Q | Ψ | | q | ψ |
| W | Ω | | w | ω |

One obvious application of escape codes is the formatting of axis 
labels, where symbols are usually italic to match their usage in the 
text of the document. Eg:

~~~
:::fortran
call labels('#fix#fn','#fiy#fn','#fiy=f#d0#u(x)#fn')
~~~
