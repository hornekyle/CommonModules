#!/usr/bin/env python

from sympy import *

a,b,c = symbols('a b c')
dtm,dtp = symbols('dt_+ dt_-')

A = Matrix( [
	[1,1,1],
	[-dtm,0,dtp],
	[dtm**2,0,dtp**2]
	] )

b = Matrix( [
	[0],
	[1],
	[0]
	] )

x = factor( A**(-1)*b )

pprint(x)
