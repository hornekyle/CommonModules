program basic_prg
	!! Create a basic plot
	!!
	!! ![plot](|media|/basic-1.svg)
	use array_mod
	use plplotlib_mod
	implicit none
	
	real(wp),dimension(:),allocatable::x,y
	
	x = linspace(-2.0_wp,2.0_wp,101)
	y = x**2-1.0_wp
	
	call setup(device='svg',fileName='media/basic-%n.svg',figSize=[400,300])
	call figure()
	call subplot(1,1,1)
	call xylim(mixval(x),mixval(y))
	call plot(x,y,lineColor='C0',lineWidth=2.0_wp)
	call ticks()
	call labels('#fix#fn','#fiy#fn','Plot')
	call show()
end program basic_prg
