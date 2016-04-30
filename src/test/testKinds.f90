program testKinds_prg
	use kinds_mod
	implicit none
	
	character(20)::fmp = '(1A2,1X,1I2)'
	character(20)::fmc = '(1A2,1X,1F40.35)'
	
	call testPrecision
	call testConstants
	
contains

	subroutine testPrecision
		integer,dimension(4)::prec
		
		prec(1) = precision(1.0_sp)
		prec(2) = precision(1.0_dp)
		prec(3) = precision(1.0_ep)
		prec(4) = precision(1.0_qp)
		
		if( any(prec<[6,15,18,33]) ) error stop "Failed real kinds precision check"
	end subroutine testPrecision

	subroutine testConstants
		character(41),parameter::cPI = '3.141592653589793238462643383279502884197'
		character(41),parameter::cE  = '2.718281828459045235360287471352662497757'
		
		integer::dPI,dE
		
		dPI = check(PI,cPI)
		dE  = check( E,cE )
		
		if( any([dPI,dE]<precision(1.0_wp)-1) ) error stop "Failed real constants precision check"
	end subroutine testConstants

	function check(r,c) result(o)
		real(wp),intent(in)::r
		character(*),intent(in)::c
		integer::o
		
		character(41)::buf
		integer::k
		
		write(buf,'(1F41.39)') r
		do k=1,len(c)
			if(buf(k:k)/=c(k:k)) exit
		end do
		o = k-1
	end function check

end program testKinds_prg
