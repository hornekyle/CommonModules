program testConstants_prg
	!! Test program for kinds_mod
	use constants_mod
	implicit none
	
	call testConstants
	
contains

	subroutine testConstants
		!! Test standard constants and verify accuracy to type-level precision
		character(41),parameter::cPI = '3.141592653589793238462643383279502884197'
		character(41),parameter::cE  = '2.718281828459045235360287471352662497757'
		
		integer::dPI,dE
		
		dPI = checkPrecision(PI,cPI)
		dE  = checkPrecision( E,cE )
		
		if( any([dPI,dE]<precision(1.0_wp)-2) ) error stop "Failed real constants precision check"
	end subroutine testConstants

	function checkPrecision(r,c) result(o)
		!! Compare a real value with its true value in string form; return the correct digit count
		real(wp),intent(in)::r
			!! Real value
		character(*),intent(in)::c
			!! True value
		integer::o
			!! Correct digits
		
		character(41)::buf
		integer::k
		
		write(buf,'(1F41.39)') r
		do k=1,len(c)
			if(buf(k:k)/=c(k:k)) exit
		end do
		o = k-2
	end function checkPrecision

end program testConstants_prg
