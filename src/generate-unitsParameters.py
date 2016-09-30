#!/usr/bin/env python

#==========#
#= Length =#
#==========#

Length = {}
Length[' m' ] = 1.0

Length['cm'] = 1.0E-2
Length['mm'] = 1.0E-3
Length['um'] = 1.0E-6
Length['nm'] = 1.0E-9
Length['A' ] = 1.0E-10
Length['km'] = 1.0E3
Length['AU'] = 1.4960E11
Length['ly'] = 9.4607E15
Length['pc'] = 3.0857E16

Length['yd'] = 0.9144
Length['ft'] = 0.3048
Length['in'] = 0.0254
Length['mi'] = 1.609344E3

#========#
#= Mass =#
#========#

Mass = {}
Mass[' kg'] = 1.0

Mass['g' ] = 1.0E-3
Mass['mg'] = 1.0E-6
Mass['u' ] = 1.66053904020E-27

Mass['slug' ] = 14.5939029372
Mass['lbm'  ] = 0.45359237
Mass['stone'] = 6.35029

#========#
#= Time =#
#========#

Time = {}
Time[' s'] = 1.0

Time['ms'] = 1.0E-3
Time['us'] = 1.0E-6
Time['ns'] = 1.0E-9
Time['min'] = 60.0
Time['hr' ] = 3600.0
Time['d'  ] = 86400.0
Time['yr'] = 3.154E7

#===============#
#= Temperature =#
#===============#

Temp = {}
Temp[' K'] = 1.0

Temp[' R'] = 0.5555555555555556

def getCode(D,pre):
	N = len(D)
	lines = []
	
	lines.append('\tinteger,parameter::%(pre)s_COUNT = %(count)d'%{'pre':pre,'count':N})
	lines.append('\tcharacter(10),dimension(%(pre)s_COUNT)::%(pre)s_NAMES = [character(10):: &'%{'pre':pre})
	buf = '\t\t&'
	keys = sorted(D.keys())
	for k in range(len(keys)):
		buf = '%s \'%s\''%(buf,keys[k].lstrip())
		if k<len(keys)-1:
			buf = '%s,'%buf
		if (k+1)%3==0 and k!=len(keys)-1:
			buf = '%s & \n\t\t&'%buf
		if k==len(keys)-1:
			buf = '%s &'%buf
	lines.append(buf)
	lines.append('\t\t& ]')
	
	lines.append('\treal(wp),dimension(%(pre)s_COUNT)::%(pre)s_SCALES = [real(wp):: &'%{'pre':pre})
	buf = '\t\t&'
	keys = sorted(D.keys())
	for k in range(len(keys)):
		buf = '%s %.15e_wp'%(buf,D[keys[k]])
		if k<len(keys)-1:
			buf = '%s,'%buf
		if (k+1)%3==0 and k!=len(keys)-1:
			buf = '%s & \n\t\t&'%buf
		if k==len(keys)-1:
			buf = '%s &'%buf
	BS = buf.split('\n')
	for b in BS:
		lines.append(b)
	lines.append('\t\t& ]')
	
	return lines

#===================#
#= Assemble Module =#
#===================#

head = []
head.append('module unitsParameters_mod')
head.append('\tuse kinds_mod')
head.append('\timplicit none')
head.append('\t')

L = getCode(Length,'UL')+['\t']
M = getCode(Mass,'UM')+['\t']
t = getCode(Time,'UC')+['\t']
T = getCode(Temp,'UT')
body = L+M+t+T

foot = []
foot.append('end module unitsParameters_mod')

full = head+body+foot

#==============#
#= Write File =#
#==============#

fh = open('unitsParameters.f90','w')
for l in full:
	fh.write('%s\n'%l)
fh.close()
