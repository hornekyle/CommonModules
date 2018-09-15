module objective_mod
	use optimize_mod
	use autoDiff_mod
	implicit none
	
	integer::kLog = 1
	real(wp),dimension(1000,2)::xLog
	
	type,extends(obj_t)::test_t
		real(wp),private::b = -1.0_wp
	contains
		procedure::eval => eval_test
	end type
	
	type,extends(objN_t)::testN_t
		real(wp),dimension(2)::c0 = [2.0_wp,3.0_wp]
		real(wp),dimension(2)::s0 = [1.0_wp,2.0_wp]
	contains
		procedure::eval => eval_testN
	end type
	
	type,extends(objN_t)::testNa_t
		real(wp),dimension(2)::c0 = [2.0_wp,3.0_wp]
		real(wp),dimension(2)::s0 = [1.0_wp,2.0_wp]
	contains
		procedure::eval => eval_testNa
		procedure::grad => grad_testNa
	end type
	
contains

	function eval_test(self,x) result(o)
		class(test_t),intent(in)::self
		real(wp),intent(in)::x
		real(wp)::o
		
		o = x**2-self%b
	end function eval_test

	function eval_testN(self,x) result(o)
		class(testN_t),intent(in)::self
		real(wp),dimension(:),intent(in)::x
			!! Must be dimension(2)
		real(wp)::o
		
		if( kLog<=size(xLog,1) ) then
			xLog(kLog,1:2) = x(1:2)
			kLog = kLog+1
		end if
		
		o = norm2( (x(1:2)-self%c0)*self%s0 )**2-1.0_wp
	end function eval_testN

	function eval_testNa(self,x) result(o)
		class(testNa_t),intent(in)::self
		real(wp),dimension(:),intent(in)::x
			!! Must be dimension(2)
		real(wp)::o
		
		if( kLog<=size(xLog,1) ) then
			xLog(kLog,1:2) = x(1:2)
			kLog = kLog+1
		end if
		
		o = norm2( (x(1:2)-self%c0)*self%s0 )**2-1.0_wp
	end function eval_testNa

	function grad_testNa(self,x) result(o)
		class(testNa_t),intent(in)::self
		real(wp),dimension(:),intent(in)::x
			!! Must be dimension(2)
		real(wp),dimension(:),allocatable::o
		
		type(ad_t),dimension(2)::ax
		type(ad_t)::ao
		
		if( kLog<=size(xLog,1) ) then
			xLog(kLog,1:2) = x(1:2)
			kLog = kLog+1
		end if
		
		ax(1) = ad_t( x(1) , 2 , 1 )
		ax(2) = ad_t( x(2) , 2 , 2 )
		
		ao = norm2( (ax(1:2)-self%c0)*self%s0 )**2-1.0_wp
		
		o = ao%grad()
	end function grad_testNa

end module objective_mod

program testOptimize_prg
	!! Test program for Optimize_mod
	!! @todo
	!! Finish tests
	use objective_mod
	use array_mod
	use plplotlib_mod
	use text_mod
	implicit none
	
	call testObjective
	call testObjectiveN
	call testPlot
	
contains

	subroutine testObjective
		!! Verify operation of obj_t
		type(test_t)::test
		
		write(*,*) test%eval(2.0_wp)-3.0_wp
		write(*,*) test%der1(0.0_wp)
		write(*,*) test%der2(0.0_wp)
		write(*,*) test%rootNewton(4.0_wp,tol=1.0E-10_wp,maxIts=1000000)
		write(*,*) test%minNewton(4.0_wp,tol=1.0E-10_wp,maxIts=1000000)
	end subroutine testObjective

	subroutine testObjectiveN
		type(testNa_t)::test
		real(wp),dimension(:),allocatable::xSD,xMN,xNM
		
		xSD = test%steepestDescent([5.0_wp,5.0_wp])
		xMN = test%minNewton([5.0_wp,5.0_wp])
		xNM = test%nelderMead([5.0_wp,5.0_wp])
		write(*,*) xSD
		write(*,*) xMN
		write(*,*) xNM
	end subroutine testObjectiveN

	subroutine testPlot
		type(testNa_t)::test
		real(wp),dimension(:),allocatable::x,y
		real(wp),dimension(:,:),allocatable::f
		integer::N,i,j
		
		real(wp),dimension(:),allocatable::xm
		
		N = 100
		x = linspace(0.0_wp,7.0_wp,N)
		y = linspace(0.0_wp,7.0_wp,N)
		allocate( f(N,N) )
		
		do j=1,N
			do i=1,N
				f(i,j) = -test%eval([ x(i) , y(j) ])
			end do
		end do
		
		call setup(device='pngcairo',fileName='testsOptimize-%n.png',figSize=[400,350])
		
		kLog = 1
		xm = test%steepestDescent([5.0_wp,5.0_wp])
		call figure()
		call subplot(1,1,1,aspect=span(y)/span(x))
		call xylim(mixval(x),mixval(y))
		call contourf(x,y,f,30)
		call plot(xLog(:kLog-1,1),xLog(:kLog-1,2),lineStyle='-',markStyle='x',markColor='k')
		call colorbar2(f,30)
		call ticks()
		call labels('x','y','Steepest Descent F['//intToChar(kLog-1)//']')
		
		kLog = 1
		xm = test%minNewton([5.0_wp,5.0_wp])
		call figure()
		call subplot(1,1,1,aspect=span(y)/span(x))
		call xylim(mixval(x),mixval(y))
		call contourf(x,y,f,30)
		call plot(xLog(:kLog-1,1),xLog(:kLog-1,2),lineStyle='-',markStyle='x',markColor='k')
		call colorbar2(f,30)
		call ticks()
		call labels('x','y','Newton-Raphson G['//intToChar(kLog-1)//']')
		
		kLog = 1
		xm = test%nelderMead([5.0_wp,5.0_wp])
		call figure()
		call subplot(1,1,1,aspect=span(y)/span(x))
		call xylim(mixval(x),mixval(y))
		call contourf(x,y,f,30)
		call plot(xLog(:kLog-1,1),xLog(:kLog-1,2),lineStyle='-',markStyle='x',markColor='k')
		call colorbar2(f,30)
		call ticks()
		call labels('x','y','Nelder-Mead Simplex F['//intToChar(kLog-1)//']')
		
		call show()
		
	end subroutine testPlot

end program testOptimize_prg
