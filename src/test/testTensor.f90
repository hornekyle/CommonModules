program testTensor_prg
	!! Test program for Tensor_mod
	use tensor_mod
	implicit none
	
	call testDot
	
contains

	subroutine testDot
		!! Verify operation of Dot and Dyadic
		real(wp),dimension(3)::u,v
		real(wp),dimension(3,3)::D
		real(wp)::r
		
		u = [1.0_wp,2.0_wp,0.0_wp]
		v = [0.0_wp,2.0_wp,3.0_wp]
		
		r = u.o.v
		D = u.d.v
	end subroutine testDot

end program testTensor_prg
