module units_mod
	use text_mod
	use unitsParameters_mod
	use ieee_arithmetic
	use ieee_exceptions
	use ieee_features
	implicit none
	private
		
	!==================================!
	!= quantity_t Type and Interfaces =!
	!==================================!
	
	type::quantity_t
		real(wp)::value = 0.0_wp
		
		integer,dimension(UL_COUNT)::length = 0
		integer,dimension(UM_COUNT)::mass   = 0
		integer,dimension(UC_COUNT)::time   = 0
		integer,dimension(UT_COUNT)::temp   = 0
	contains
		procedure::getDims
		procedure::getScale
		procedure::getBase
		procedure::getChar
		procedure::convert
	end type
	
	interface quantity_t
		module procedure newQuantity_basic
		module procedure newQuantity_list
	end interface
	
	public::quantity_t
	
	!=============!
	!= Overloads =!
	!=============!
	
	! Addition
	interface operator(+)
		module procedure add_qq
	end interface
	public::operator(+)
	
	! Subtraction
	interface operator(-)
		module procedure sub_qq
	end interface
	public::operator(-)
	
	! Multiplication
	interface operator(*)
		module procedure mul_rq
		module procedure mul_qr
		module procedure mul_qq
	end interface
	public::operator(*)
	
	! Division
	interface operator(/)
		module procedure div_rq
		module procedure div_qr
		module procedure div_qq
	end interface
	public::operator(/)
	
	! Power
	interface operator(**)
		module procedure pow_qi
	end interface
	public::operator(**)
	
	! Sqrt
	interface sqrt
		module procedure sqrt_q
	end interface
	public::sqrt
	
	!=================!
	!= Other Exports =!
	!=================!
	
	! Types
	public::wp
	
contains

	!================!
	!= Constructors =!
	!================!

	elemental function newQuantity_basic(value,unit) result(self)
		real(wp),intent(in)::value
		character(*),intent(in)::unit
		type(quantity_t)::self
		
		character(10),dimension(:),allocatable::names
		integer,dimension(:),allocatable::powers
		real(wp)::sca
		
		self%value = value
		where(UL_NAMES==unit) self%length = 1
		where(UM_NAMES==unit) self%mass   = 1
		where(UC_NAMES==unit) self%time   = 1
		where(UT_NAMES==unit) self%temp   = 1
		
		if( sum( self%getDims() )>0 ) return
		
		call getAliasUnits(unit,names,powers,sca)
		
		self = newQuantity_list(value*sca,names,powers)
	end function newQuantity_basic

	pure function newQuantity_list(value,names,powers) result(self)
		real(wp),intent(in)::value
		character(*),dimension(:),intent(in)::names
		integer,dimension(:),intent(in)::powers
		type(quantity_t)::self
		
		integer::k
		
		self%value = value
		do k=1,size(names)
			where(UL_NAMES==names(k)) self%length = self%length+powers(k)
			where(UM_NAMES==names(k)) self%mass   = self%mass+powers(k)
			where(UC_NAMES==names(k)) self%time   = self%time+powers(k)
			where(UT_NAMES==names(k)) self%temp   = self%temp+powers(k)
		end do
	end function newQuantity_list

	!=======================!
	!= quantity_t Routines =!
	!=======================!

	pure function getDims(self) result(o)
		class(quantity_t),intent(in)::self
		integer,dimension(4)::o
		
		o(1) = sum(self%length)
		o(2) = sum(self%mass)
		o(3) = sum(self%time)
		o(4) = sum(self%temp)
	end function getDims

	elemental function getScale(self) result(o)
		class(quantity_t),intent(in)::self
		real(wp)::o
		
		real(wp),dimension(4)::scales
		
		scales(1) = product(UL_SCALES**self%length)
		scales(2) = product(UM_SCALES**self%mass)
		scales(3) = product(UC_SCALES**self%time)
		scales(4) = product(UT_SCALES**self%temp)
		
		o = product(scales)
	end function getScale

	elemental function getBase(self) result(o)
		class(quantity_t),intent(in)::self
		type(quantity_t)::o
		
		integer,dimension(4)::dims
		
		dims = self%getDims()
		
		o%value = self%getScale()*self%value
		o%length(1) = dims(1)
		o%mass(1)   = dims(2)
		o%time(1)   = dims(3)
		o%temp(1)   = dims(4)
	end function getBase

	elemental function getChar(self) result(o)
		class(quantity_t),intent(in)::self
		character(:),allocatable::o
		
		integer::k
		
		allocate(o,source='')
		
		do k=1,UL_COUNT
			if(self%length(k)==0) cycle
			o = o//trim(UL_NAMES(k))//'^('//intToChar(self%length(k))//') '
		end do
		
		do k=1,UM_COUNT
			if(self%mass(k)==0) cycle
			o = o//trim(UM_NAMES(k))//'^('//intToChar(self%mass(k))//') '
		end do
		
		do k=1,UC_COUNT
			if(self%time(k)==0) cycle
			o = o//trim(UC_NAMES(k))//'^('//intToChar(self%time(k))//') '
		end do
		
		do k=1,UT_COUNT
			if(self%temp(k)==0) cycle
			o = o//trim(UT_NAMES(k))//'^('//intToChar(self%temp(k))//') '
		end do
	end function getChar

	elemental function convert(self,u) result(o)
		class(quantity_t),intent(in)::self
		type(quantity_t),intent(in)::u
		type(quantity_t)::o
		
		o%value  = ( self%getScale()/u%getScale() )*self%value
		o%length = u%length
		o%mass   = u%mass
		o%time   = u%time
		o%temp   = u%temp
	end function convert

	!=============!
	!= Operators =!
	!=============!

	elemental function add_qq(u,v) result(o)
		type(quantity_t),intent(in)::u
		type(quantity_t),intent(in)::v
		type(quantity_t)::o
		
		integer,dimension(4)::uD,vD
		real(wp)::uS,vS
		
		uD = u%getDims()
		vD = v%getDims()
		if( .not. all(uD==vD) ) then
			call ieee_set_flag(IEEE_INVALID,.true.)
		end if
		
		uS = u%getScale()
		vS = v%getScale()
		
		o%value  = u%value+(vS/uS)*v%value
		o%length = u%length
		o%mass   = u%mass
		o%time   = u%time
		o%temp   = u%temp
	end function add_qq

	elemental function sub_qq(u,v) result(o)
		type(quantity_t),intent(in)::u
		type(quantity_t),intent(in)::v
		type(quantity_t)::o
		
		integer,dimension(4)::uD,vD
		real(wp)::uS,vS
		
		uD = u%getDims()
		vD = v%getDims()
		if( .not. all(uD==vD) ) return
		
		uS = u%getScale()
		vS = v%getScale()
		
		o%value  = u%value-(vS/uS)*v%value
		o%length = u%length
		o%mass   = u%mass
		o%time   = u%time
		o%temp   = u%temp
	end function sub_qq

	elemental function mul_rq(u,v) result(o)
		real(wp),intent(in)::u
		type(quantity_t),intent(in)::v
		type(quantity_t)::o
		
		o%value  = u*v%value
		o%length = v%length
		o%mass   = v%mass
		o%time   = v%time
		o%temp   = v%temp
	end function mul_rq

	elemental function mul_qr(u,v) result(o)
		type(quantity_t),intent(in)::u
		real(wp),intent(in)::v
		type(quantity_t)::o
		
		o%value  = u%value*v
		o%length = u%length
		o%mass   = u%mass
		o%time   = u%time
		o%temp   = u%temp
	end function mul_qr

	elemental function mul_qq(u,v) result(o)
		type(quantity_t),intent(in)::u
		type(quantity_t),intent(in)::v
		type(quantity_t)::o
		
		o%value  = u%value*v%value
		o%length = u%length+v%length
		o%mass   = u%mass+v%mass
		o%time   = u%time+v%time
		o%temp   = u%temp+v%temp
	end function mul_qq

	elemental function div_rq(u,v) result(o)
		real(wp),intent(in)::u
		type(quantity_t),intent(in)::v
		type(quantity_t)::o
		
		o%value  = u/v%value
		o%length = -v%length
		o%mass   = -v%mass
		o%time   = -v%time
		o%temp   = -v%temp
	end function div_rq

	elemental function div_qr(u,v) result(o)
		type(quantity_t),intent(in)::u
		real(wp),intent(in)::v
		type(quantity_t)::o
		
		o%value  = u%value/v
		o%length = u%length
		o%mass   = u%mass
		o%time   = u%time
		o%temp   = u%temp
	end function div_qr

	elemental function div_qq(u,v) result(o)
		type(quantity_t),intent(in)::u
		type(quantity_t),intent(in)::v
		type(quantity_t)::o
		
		o%value  = u%value/v%value
		o%length = u%length-v%length
		o%mass   = u%mass-v%mass
		o%time   = u%time-v%time
		o%temp   = u%temp-v%temp
	end function div_qq

	elemental function pow_qi(u,p) result(o)
		type(quantity_t),intent(in)::u
		integer,intent(in)::p
		type(quantity_t)::o
		
		o%value  = u%value**p
		o%length = u%length*p
		o%mass   = u%mass*p
		o%time   = u%time*p
		o%temp   = u%temp*p
	end function pow_qi

	elemental function sqrt_q(u) result(o)
		!! Only works for even powers
		!! Always converts to SI
		type(quantity_t),intent(in)::u
		type(quantity_t)::o
		
		type(quantity_t)::b
		
		b = u%getBase()
		
		o%value  = sqrt(b%value)
		o%length = b%length/2
		o%mass   = b%mass/2
		o%time   = b%time/2
		o%temp   = b%temp/2
	end function sqrt_q

end module units_mod
