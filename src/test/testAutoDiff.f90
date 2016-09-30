program testAutoDiff_prg
	!! Test program for autoDiff_mod
	!! @todo
	!! Needs serious improvements
	use autoDiff_mod
	implicit none
	
	call testDiff
	
contains

	subroutine testDiff
		type(ad_t)::x
		
		x = ad_t(1.0_wp,1,1)
	end subroutine testDiff

end program testAutoDiff_prg 
 
