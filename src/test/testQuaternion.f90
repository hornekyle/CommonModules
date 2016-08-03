program testQuaternion_prg
	!! Test program for quaternion_mod
	use kinds_mod
	use tensor_mod
	use quaternion_mod
	implicit none
	
	call testBasic
	
contains

	subroutine testBasic
		type(quat_t)::u,v
	end subroutine testBasic

end program testQuaternion_prg