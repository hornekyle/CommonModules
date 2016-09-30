program testQuaternion_prg
	!! Test program for quaternion_mod
	!! @todo
	!! Add real tests
	use quaternion_mod
	implicit none
	
	call testBasic
	
contains

	subroutine testBasic
		type(quat_t)::u,v,w
		
		u%s = 1.0_wp
		v%s = 2.0_wp
		
		w = u*v
		
		write(*,*) scaler(w),vector(w)
		write(*,*) w%getRotationMatrix()
	end subroutine testBasic

end program testQuaternion_prg
