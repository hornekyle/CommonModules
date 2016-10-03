module constants_mod
	!! Module to manage basic constants
	use kinds_mod
	use units_mod
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
	public::arg
	public::getConstant
	
	! Kinds
	public::wp
	public::quantity_t
	
contains

	elemental function arg(z) result(v)
		!! Compute the argument of a complex number
		complex(wp),intent(in)::z
		real(wp)::v
		
		v = atan2(aimag(z),real(z))
	end function arg

	function getConstant(name) result(o)
		character(*),intent(in)::name
		type(quantity_t)::o
		
		character(10),dimension(:),allocatable::names
		integer,dimension(:),allocatable::powers
		real(wp)::value
		
		select case(name)
		case('surfaceGravity')
			value  = 9.80665_wp
			names  = [character(10):: 'm','kg','s','K']
			powers = [integer:: 1,0,-2,0]
		case('gravitation')
			value  = 6.67408E-11_wp
			names  = [character(10):: 'm','kg','s','K']
			powers = [integer:: 3,-1,-2,0]
		case('boltzman')
			value  = 1.38064852E-23_wp
			names  = [character(2):: 'm','kg','s','K']
			powers = [integer:: 2,1,-2,-1]
		case('stephanBoltzman')
			value  = 5.67051E-8_wp
			names  = [character(2):: 'm','kg','s','K']
			powers = [integer:: 0,1,-3,-4]
		case('protonMass')
			value  = 1.672623E-27_wp
			names  = [character(2):: 'm','kg','s','K']
			powers = [integer:: 0,1,0,0]
		case('plank')
			value  = 6.6260755E-34_wp
			names  = [character(2):: 'm','kg','s','K']
			powers = [integer:: 2,1,-1,0]
		case('lightSpeed')
			value  = 299792458.0_wp
			names  = [character(2):: 'm','kg','s','K']
			powers = [integer:: 1,0,-1,0]
		end select
		
		o = quantity_t(value,names,powers)
	end function getConstant

end module constants_mod
 
