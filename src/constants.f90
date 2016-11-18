module constants_mod
	!! Module to manage basic constants
	use kinds_mod
	implicit none
	
	!==================!
	!= Math Constants =!
	!==================!
	
	real(wp),parameter::PI = 4.0_wp*atan(1.0_wp)
		!! Archimedes' constant
	real(wp),parameter::E  = exp(1.0_wp)
		!! Euler's constant
	
	!===========!
	!= Exports =!
	!===========!
	
	public::PI,E
	
	! Kinds
	public::wp
	
contains

	elemental function arg(z) result(v)
		!! Compute the argument of a complex number
		complex(wp),intent(in)::z
		real(wp)::v
		
		v = atan2(aimag(z),real(z))
	end function arg

end module constants_mod
 
