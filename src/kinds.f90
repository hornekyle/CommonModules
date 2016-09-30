module kinds_mod
	!! Module to manage real kinds
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
	
	!===========!
	!= Exports =!
	!===========!
	
	public::sp,dp,ep,qp,wp
	
	public::printTypes
	
contains

	subroutine printTypes
		!! Print the integer kinds for each real type
		write(*,*) 'sp: ',sp
		write(*,*) 'dp: ',dp
		write(*,*) 'ep: ',ep
		write(*,*) 'qp: ',qp
		write(*,*) 'wp: ',wp
	end subroutine printTypes

end module kinds_mod
 
