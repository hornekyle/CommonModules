program testEval_prg
	!! Test program for eval_mod
	use kinds_mod
	use eval_mod
	use array_mod
	use plplotlib_mod
	implicit none
	
	call testFunction
	
contains

	subroutine testFunction
		!! Verify operation of newFunction and eval
		call setup()
		call doFunction('f(x) = x^2-1',[-2.0_wp,2.0_wp])
		call doFunction('f(x) = exp(x)',[0.0_wp,2.0_wp*E])
		call doFunction('f(x) = sin(x)',[0.0_wp,2.0_wp*PI])
		call doFunction('f(x) = cos(x)',[0.0_wp,2.0_wp*PI])
		call show()
	end subroutine testFunction

	subroutine doFunction(s,r)
		character(*),intent(in)::s
		real(wp),dimension(2),intent(in)::r
		
		integer,parameter::N = 50
		type(function_t)::f
		real(wp),dimension(N)::x,y
		integer::k
		
		f = newFunction(s)
		
		write(*,*) 'Arguments'
		write(*,*) f%ar
		write(*,*) 'Expression'
		do k=1,size(f%ex)
			write(*,*) f%ex(k)
		end do
		
		x = linspace(r(1),r(2),N)
		do k=1,N
			y(k) = f%eval([x(k)])
		end do
		
		call figure()
		call subplot(1,1,1)
		call xylim(mixval(x),mixval(y))
		call plot(x,y,lineColor='r',lineWidth=2.0_wp)
		call ticks()
		call labels('x','y',s)
		
	end subroutine doFunction

end program testEval_prg