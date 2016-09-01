module objective_mod
	use kinds_mod
	use optimize_mod
	implicit none
	
	type,extends(obj_t)::test_t
	contains
		procedure::eval => eval_test
	end type
	
contains

	function eval_test(self,x) result(o)
		class(test_t),intent(in)::self
		real(wp),intent(in)::x
		real(wp)::o
		
		o = x**2-1.0_wp
	end function eval_test

end module objective_mod

program testOptimize_prg
	!! Test program for Optimize_mod
	use kinds_mod
	use optimize_mod
	use objective_mod
	implicit none
	
	call testObjective
	
contains

	subroutine testObjective
		!! Verify operation of obj_t
		type(test_t)::test
		
		write(*,*) test%eval(2.0_wp)
	end subroutine testObjective

end program testOptimize_prg