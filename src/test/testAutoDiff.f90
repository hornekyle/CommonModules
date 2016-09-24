program testAutoDiff_prg
	!! Test program for autoDiff_mod
	use kinds_mod
	use autoDiff_mod
	implicit none
	
	call testDiff
	
contains

	subroutine testDiff
		type(ad1_t)::x
		
		x = diff1(1.0_wp,1)
	end subroutine testDiff

end program testAutoDiff_prg 
 