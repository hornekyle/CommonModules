module kinds_mod
	!! Module to manage real kinds and basic constants
	implicit none
	
	!==============!
	!= Real Kinds =!
	!==============!
	
	integer,parameter::sp = selected_real_kind(6)
		!! Single precision
	integer,parameter::dp = selected_real_kind(15)
		!! Double precision
	integer,parameter::ep = selected_real_kind(18)
		!! Extended precision
	integer,parameter::qp = selected_real_kind(32)
		!! Quad precision
	
	integer,dimension(4),parameter::rkinds = [sp,dp,ep,qp]
		!! List of real kinds
	
	integer,parameter::wp = dp
		!! Set working precision to double
	
	!==================!
	!= Math Constants =!
	!==================!
	
	real(wp),parameter::PI = real(4.0_qp*atan(1.0_qp),wp)
		!! Archimedes' constant
	real(wp),parameter::E  = real(exp(1.0_qp),wp)
		!! Euler's constant
	
	!===========!
	!= Exports =!
	!===========!
	
	public::sp,dp,ep,qp,wp
	public::PI,E
	
	public::printTypes
	public::arg
	
contains

	subroutine printTypes
		!! Print the integer kinds for each real type
		write(*,*) 'sp: ',sp
		write(*,*) 'dp: ',dp
		write(*,*) 'ep: ',ep
		write(*,*) 'qp: ',qp
		write(*,*) 'wp: ',wp
	end subroutine printTypes

	elemental function arg(z) result(v)
		!! Compute the argument of a complex number
		complex(wp),intent(in)::z
		real(wp)::v
		
		v = atan2(aimag(z),real(z))
	end function arg

end module kinds_mod
 
