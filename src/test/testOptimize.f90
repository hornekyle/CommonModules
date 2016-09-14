module objective_mod
	use kinds_mod
	use optimize_mod
	implicit none
	
	type,extends(obj_t)::test_t
	contains
		procedure::eval => eval_test
	end type
	
	type,extends(objN_t)::testN_t
		real(wp),dimension(2)::c0
	contains
		procedure::eval => eval_testN
	end type
	
contains

	function eval_test(self,x) result(o)
		class(test_t),intent(in)::self
		real(wp),intent(in)::x
		real(wp)::o
		
		o = x**2-1.0_wp
	end function eval_test

	function eval_testN(self,x) result(o)
		class(testN_t),intent(in)::self
		real(wp),dimension(:),intent(in)::x
		real(wp)::o
		
		o = norm2( (x-self%c0)*[1.0_wp,2.0_wp] )**2-1.0_wp
	end function eval_testN

end module objective_mod

program testOptimize_prg
	!! Test program for Optimize_mod
	!! @todo
	!! Finish tests
	use kinds_mod
	use optimize_mod
	use objective_mod
	implicit none
	
! 	call testObjective
	call testObjectiveN
	
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
		type(testN_t)::test
		real(wp),dimension(:),allocatable::xm
		real(wp)::x
		
		test%c0 = [2.0_wp,3.0_wp]
		
		xm = test%steepestDescent([5.0_wp,5.0_wp])
		write(*,*) xm
	end subroutine testObjectiveN

end program testOptimize_prg
