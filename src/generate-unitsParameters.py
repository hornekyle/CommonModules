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

#===================#
#= Composite Units =#
#===================#

Comp = {}

Comp['N'  ] = {' kg':1,' m':1,' s':-2}
Comp['lbf'] = {'slug':1,'ft':1,' s':-2}

Comp['Pa' ] = {' kg':1,' m':-1,' s':-2}
Comp['psi'] = {'slug':1,'ft':1,'in':-2,' s':-2}

Comp['kph'] = {'km':1,'hr':-1}
Comp['mph'] = {'mi':1,'hr':-1}

Comp['J'  ] = {' kg':1,' m':2,' s':-2}
Comp['btu'] = {' kg':1,' m':2,' s':-2,'_scale':1055.06}

Comp['W'  ] = {' kg':1,' m':2,' s':-3}
Comp['hp' ] = {' kg':1,' m':2,' s':-3,'_scale':745.7}

#===================#
#= Code Generators =#
#===================#

def getParameters(D,pre):
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
	BS = buf.split('\n')
	for b in BS:
		lines.append(b)
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

def getAliasSubroutine(D):
	lines = []
	lines.append('\tpure subroutine getAliasUnits(alias,names,powers,scale)')
	lines.append('\t\tcharacter(*),intent(in)::alias')
	lines.append('\t\tcharacter(10),dimension(:),allocatable,intent(inout)::names')
	lines.append('\t\tinteger,dimension(:),allocatable,intent(inout)::powers')
	lines.append('\t\treal(wp),intent(out)::scale')
	lines.append('\t\t')
	lines.append('\t\tif(allocated(names)) deallocate(names)')
	lines.append('\t\tif(allocated(powers)) deallocate(powers)')
	lines.append('\t\tscale = 1.0_wp')
	lines.append('\t\t')
	lines.append('\t\tselect case(alias)')
	for k in sorted(D.keys()):
		lines.append('\t\tcase(\'%s\')'%k)
		units = [ d.lstrip() for d in sorted(D[k].keys()) if d!='_scale' ]
		names = '\', \''.join(units)
		ints  = [ '%d'%D[k][d] for d in sorted(D[k].keys()) if d!='_scale' ]
		powers = ', '.join(ints)
		lines.append('\t\t\tnames = [character(10)::\'%s\']'%names)
		lines.append('\t\t\tpowers = [%s]'%powers)
		if '_scale' in D[k].keys():
			lines.append('\t\tscale = %e'%D[k]['_scale'])
	lines.append('\t\tend select')
	lines.append('\tend subroutine getAliasUnits')
	return lines

#===================#
#= Assemble Module =#
#===================#

head = []
head.append('module unitsParameters_mod')
head.append('\t!! Automatically generated by generate-unitsParameters.py')
head.append('\tuse kinds_mod')
head.append('\timplicit none')
head.append('\t')

L = getParameters(Length,'UL')+['\t']
M = getParameters(Mass,'UM')+['\t']
t = getParameters(Time,'UC')+['\t']
T = getParameters(Temp,'UT')
declarations = L+M+t+T

contains = ['','contains','']

routines = getAliasSubroutine(Comp)

foot = []
foot.append('')
foot.append('end module unitsParameters_mod')

full = head+declarations+contains+routines+foot

#==============#
#= Write File =#
#==============#

fh = open('unitsParameters.f90','w')
for l in full:
	fh.write('%s\n'%l)
fh.close()
