program logo_prg
	!! Create the project logo
	use kinds_mod
	use array_mod
	use plplotlib_mod
	implicit none
	
	call setup(device='pngqt',fileName='logo-%n.png',figSize=[128,128]) 
	call makeLogo
	call show()
	
contains

	subroutine makeLogo
		real(wp),dimension(:),allocatable::x,y1,y2,y3
		
		x  = linspace(0.0_wp,1.0_wp,100)
		y1 = x**2-1.0_wp
		y2 = 2.0_wp*x-1.0_wp
		y3 = x
		y3 = cos(2.0_wp*PI*x)
		
		call figure()
		call subplot(1,1,1)
		call xylim(mixval(x),mixval([y1,y2,y3])*1.1_wp)
		
		call plot(x,y1,lineColor='b',lineWidth=10.0_wp)
		call plot(x,y2,lineColor='r',lineWidth=10.0_wp)
		call plot(x,y3,lineColor='c',lineWidth=10.0_wp)
		
		call ticks(lineWidth=5.0_wp)
		call labels('','','')
	end subroutine makeLogo

end program logo_prg
