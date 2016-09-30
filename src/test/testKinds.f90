program testKinds_prg
	!! Test program for kinds_mod
	use kinds_mod
	implicit none
	
	call testPrecision
! 	call testConstants
	
contains

	subroutine testPrecision
		!! Test real precision constants and actual accuracy
		integer,dimension(4)::prec
		
		prec(1) = precision(1.0_sp)
		prec(2) = precision(1.0_dp)
		prec(3) = precision(1.0_ep)
		prec(4) = precision(1.0_qp)
		
		if( any(prec<[6,15,18,33]) ) then
			call printTypes
			error stop "Failed real kinds precision check"
		end if
	end subroutine testPrecision

! 	subroutine testConstants
! 		!! Test standard constants and verify accuracy to type-level precision
! 		character(41),parameter::cPI = '3.141592653589793238462643383279502884197'
! 		character(41),parameter::cE  = '2.718281828459045235360287471352662497757'
! 		
! 		integer::dPI,dE
! 		
! 		dPI = checkPrecision(PI,cPI)
! 		dE  = checkPrecision( E,cE )
! 		
! 		if( any([dPI,dE]<precision(1.0_wp)-2) ) error stop "Failed real constants precision check"
! 	end subroutine testConstants

! 	function checkPrecision(r,c) result(o)
! 		!! Compare a real value with its true value in string form; return the correct digit count
! 		real(wp),intent(in)::r
! 			!! Real value
! 		character(*),intent(in)::c
! 			!! True value
! 		integer::o
! 			!! Correct digits
! 		
! 		character(41)::buf
! 		integer::k
! 		
! 		write(buf,'(1F41.39)') r
! 		do k=1,len(c)
! 			if(buf(k:k)/=c(k:k)) exit
! 		end do
! 		o = k-2
! 	end function checkPrecision

end program testKinds_prg
