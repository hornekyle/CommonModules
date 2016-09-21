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
		call setup(fileName='testsEval-%n.svg',figSize=[400,300])
		
! 		call doFunction('f(x) = x+1j',[-2.0_wp,2.0_wp])
		
		call doFunction('f(x) = x+1',[-2.0_wp,2.0_wp])
		call doFunction('f(x) = x-1',[-2.0_wp,2.0_wp])
		call doFunction('f(x) = 2*x',[-2.0_wp,2.0_wp])
		call doFunction('f(x) = x/2',[-2.0_wp,2.0_wp])
		call doFunction('f(x) = x^2',[-2.0_wp,2.0_wp])
		
		call doFunction('f(x) = sqrt(x^2)',[-2.0_wp,2.0_wp])
		call doFunction('f(x) = abs(x)',[-2.0_wp,2.0_wp])
		call doFunction('f(x) = exp(x)',[0.0_wp,1.0_wp])
		call doFunction('f(x) = log(exp(x))',[0.0_wp,1.0_wp])
		call doFunction('f(x) = sin(x)',[0.0_wp,2.0_wp*PI])
		call doFunction('f(x) = asin(sin(x))',[0.0_wp,2.0_wp*PI])
		call doFunction('f(x) = cos(x)',[0.0_wp,2.0_wp*PI])
		call doFunction('f(x) = acos(cos(x))',[0.0_wp,2.0_wp*PI])
		
		call doFunction('f(x) = 2*(x+1)/3',[0.0_wp,2.0_wp*PI])
		call doFunction('f(x) = (x+1)*(x-1)/6',[0.0_wp,2.0_wp*PI])
		call doFunction('f(x) = 4*(x+1)/(x^2+1)',[0.0_wp,2.0_wp*PI])
		
		call show()
	end subroutine testFunction

	subroutine doFunction(s,r)
		character(*),intent(in)::s
		real(wp),dimension(2),intent(in)::r
		
		integer,parameter::N = 150
		type(function_t)::f
		real(wp),dimension(N)::x,y
		integer::k
		
		f = function_t(s)
		
		write(*,*) f%str
		
		x = linspace(r(1),r(2),N)
		do k=1,N
			y(k) = f%eval([x(k)])
		end do
		
		call figure()
		call subplot(1,1,1,aspect=span(y)/span(x))
		call xylim(mixval(x),mixval(y)+0.05_wp*[-1.0_wp,1.0_wp*span(y)])
		call plot(x,y,lineColor='r',lineWidth=2.0_wp)
		call ticks()
		call labels('x','y',s)
		
	end subroutine doFunction

end program testEval_prg
