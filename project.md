project: CommonModules
summary: Common modules for using in other programs
author: Kyle Horne
email: kyle.horne@gmail.com
project_dir: ./src
exclude: fftw3.f90
output_dir: ./doc
page_dir: ./pages
media_dir: ./examples
source: true
display: public
graph: true

This project contains numerous modules for use in other projects with the specific intent of speeding up development of new simulations.
The various routines and types are grouped according to task or data type, depending on the module.

@todo
[ ] Clean up the documentation of interfaces and their implementations  
[ ] Add rigorous tests for new modules  
[ ] Add tdma to array_mod

