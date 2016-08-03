program testQuaternion_prg
	!! Test program for quaternion_mod
	!! @todo
	!! Add real tests
	use kinds_mod
	use quaternion_mod
	implicit none
	
	call testBasic
	
contains

	subroutine testBasic
		type(quat_t)::u,v,w
		
		u%r = 1.0_wp
		v%r = 2.0_wp
		
		w = u*v
		
		write(*,*) scaler(w),vector(w)
	end subroutine testBasic

end program testQuaternion_prg