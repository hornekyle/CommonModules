module autoDiffOperator_mod
	use kinds_mod
	use autoDiffType_mod
	implicit none
	private
	
	! Assignment
	interface assignment(=)
		module procedure assign_ra
	end interface
	public::assignment(=)
	
	! Addition
	interface operator(+)
		module procedure add_ra
		module procedure add_ar
		module procedure add_aa
	end interface
	public::operator(+)
	
	! Subtraction
	interface operator(-)
		module procedure neg_a
		module procedure sub_ra
		module procedure sub_ar
		module procedure sub_aa
	end interface
	public::operator(-)
	
	! Multiplication
	interface operator(*)
		module procedure mul_ra
		module procedure mul_ar
		module procedure mul_aa
	end interface
	public::operator(*)
	
	! Division
	interface operator(/)
		module procedure div_ra
		module procedure div_ar
		module procedure div_aa
	end interface
	public::operator(/)
	
	! Power
	interface operator(**)
		module procedure pow_ra
		module procedure pow_ar
		module procedure pow_aa
	end interface
	public::operator(**)
	
contains

	!==============!
	!= Assignment =!
	!==============!

	elemental subroutine assign_ra(u,v)
		real(wp),intent(out)::u
		type(ad_t),intent(in)::v
		
		u = v%val()
	end subroutine assign_ra

	!============!
	!= Addition =!
	!============!

	elemental function add_ra(u,v) result(o)
		real(wp),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o = ad_t( u+v%val() , v%grad() )
	end function add_ra

	elemental function add_ar(u,v) result(o)
		type(ad_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad_t)::o
		
		o = ad_t( u%val()+v , u%grad() )
	end function add_ar

	elemental function add_aa(u,v) result(o)
		type(ad_t),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o = ad_t( u%val()+v%val() , u%grad()+v%grad() )
	end function add_aa

	!===============!
	!= Subtraction =!
	!===============!

	elemental function neg_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o = ad_t( -u%val() , -u%grad() )
	end function neg_a

	elemental function sub_ra(u,v) result(o)
		real(wp),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o = ad_t( u-v%val() , -v%grad() )
	end function sub_ra

	elemental function sub_ar(u,v) result(o)
		type(ad_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad_t)::o
		
		o = ad_t( u%val()-v , u%grad() )
	end function sub_ar

	elemental function sub_aa(u,v) result(o)
		type(ad_t),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o = ad_t( u%val()-v%val() , u%grad()-v%grad() )
	end function sub_aa

	!==================!
	!= Multiplication =!
	!==================!

	elemental function mul_ra(u,v) result(o)
		real(wp),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o = ad_t( u+v%val() , v%grad()*u )
	end function mul_ra

	elemental function mul_ar(u,v) result(o)
		type(ad_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad_t)::o
		
		o = ad_t( u%val()+v , u%grad()*v )
	end function mul_ar

	elemental function mul_aa(u,v) result(o)
		type(ad_t),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o = ad_t( u%val()+v%val() , u%grad()*v%val()+v%grad()*u%val() )
	end function mul_aa

	!============!
	!= Division =!
	!============!

	elemental function div_ra(u,v) result(o)
		real(wp),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o = ad_t( u/v%val() , (-v%grad()*u)/(v%val()**2) )
	end function div_ra

	elemental function div_ar(u,v) result(o)
		type(ad_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad_t)::o
		
		o = ad_t( u%val()/v , (u%grad()*v)/(v**2) )
	end function div_ar

	elemental function div_aa(u,v) result(o)
		type(ad_t),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o = ad_t( u%val()/v%val() , (u%grad()*v%val()-v%grad()*u%val())/(v%val()**2) )
	end function div_aa

	!=========!
	!= Power =!
	!=========!

	elemental function pow_ra(u,v) result(o)
		real(wp),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		real(wp)::val
		real(wp),dimension(:),allocatable::grad
		
		val = u**v%val()
		grad  = u**v%val()*( log(u)*v%grad() )
		
		o = ad_t( val , grad )
	end function pow_ra

	elemental function pow_ar(u,v) result(o)
		type(ad_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad_t)::o
		
		real(wp)::val
		real(wp),dimension(:),allocatable::grad
		
		val = u%val()**v
		grad  = u%val()**v*( v*u%grad()/u%val() )
		
		o = ad_t( val , grad )
	end function pow_ar

	elemental function pow_aa(u,v) result(o)
		type(ad_t),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		real(wp)::val
		real(wp),dimension(:),allocatable::grad
		
		val = u%val()**v%val()
		grad  = u%val()**v%val()*( log(u%val())*v%grad()+v%val()*u%grad()/u%val() )
		
		o = ad_t( val , grad )
	end function pow_aa

end module autoDiffOperator_mod 
