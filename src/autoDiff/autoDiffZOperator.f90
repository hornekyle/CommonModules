module autoDiffZOperator_mod
	use autoDiffZType_mod
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
		module procedure pow_ai
		module procedure pow_ar
		module procedure pow_aa
	end interface
	public::operator(**)
	
	! Square Root
	interface sqrt
		module procedure sqrt_a
	end interface
	public::sqrt
	
	! Absolute Value
	interface abs
		module procedure abs_a
	end interface
	public::abs
	
contains

	!==============!
	!= Assignment =!
	!==============!

	elemental subroutine assign_ra(u,v)
		complex(wp),intent(out)::u
		type(adZ_t),intent(in)::v
		
		u = v%x
	end subroutine assign_ra

	!============!
	!= Addition =!
	!============!

	elemental function add_ra(u,v) result(o)
		complex(wp),intent(in)::u
		type(adZ_t),intent(in)::v
		type(adZ_t)::o
		
		o = adZ_t( u+v%x , v%d )
	end function add_ra

	elemental function add_ar(u,v) result(o)
		type(adZ_t),intent(in)::u
		complex(wp),intent(in)::v
		type(adZ_t)::o
		
		o = adZ_t( u%x+v , u%d )
	end function add_ar

	elemental function add_aa(u,v) result(o)
		type(adZ_t),intent(in)::u
		type(adZ_t),intent(in)::v
		type(adZ_t)::o
		
		o = adZ_t( u%x+v%x , u%d+v%d )
	end function add_aa

	!===============!
	!= Subtraction =!
	!===============!

	elemental function neg_a(u) result(o)
		type(adZ_t),intent(in)::u
		type(adZ_t)::o
		
		o = adZ_t( -u%x , -u%d )
	end function neg_a

	elemental function sub_ra(u,v) result(o)
		complex(wp),intent(in)::u
		type(adZ_t),intent(in)::v
		type(adZ_t)::o
		
		o = adZ_t( u-v%x , -v%d )
	end function sub_ra

	elemental function sub_ar(u,v) result(o)
		type(adZ_t),intent(in)::u
		complex(wp),intent(in)::v
		type(adZ_t)::o
		
		o = adZ_t( u%x-v , u%d )
	end function sub_ar

	elemental function sub_aa(u,v) result(o)
		type(adZ_t),intent(in)::u
		type(adZ_t),intent(in)::v
		type(adZ_t)::o
		
		o = adZ_t( u%x-v%x , u%d-v%d )
	end function sub_aa

	!==================!
	!= Multiplication =!
	!==================!

	elemental function mul_ra(u,v) result(o)
		complex(wp),intent(in)::u
		type(adZ_t),intent(in)::v
		type(adZ_t)::o
		
		o = adZ_t( u*v%x , v%d*u )
	end function mul_ra

	elemental function mul_ar(u,v) result(o)
		type(adZ_t),intent(in)::u
		complex(wp),intent(in)::v
		type(adZ_t)::o
		
		o = adZ_t( u%x*v , u%d*v )
	end function mul_ar

	elemental function mul_aa(u,v) result(o)
		type(adZ_t),intent(in)::u
		type(adZ_t),intent(in)::v
		type(adZ_t)::o
		
		o = adZ_t( u%x*v%x , u%d*v%x+v%d*u%x )
	end function mul_aa

	!============!
	!= Division =!
	!============!

	elemental function div_ra(u,v) result(o)
		complex(wp),intent(in)::u
		type(adZ_t),intent(in)::v
		type(adZ_t)::o
		
		o = adZ_t( u/v%x , (-v%d*u)/(v%x**2) )
	end function div_ra

	elemental function div_ar(u,v) result(o)
		type(adZ_t),intent(in)::u
		complex(wp),intent(in)::v
		type(adZ_t)::o
		
		o = adZ_t( u%x/v , (u%d*v)/(v**2) )
	end function div_ar

	elemental function div_aa(u,v) result(o)
		type(adZ_t),intent(in)::u
		type(adZ_t),intent(in)::v
		type(adZ_t)::o
		
		o = adZ_t( u%x/v%x , (u%d*v%x-v%d*u%x)/(v%x**2) )
	end function div_aa

	!=========!
	!= Power =!
	!=========!

	elemental function pow_ra(u,v) result(o)
		complex(wp),intent(in)::u
		type(adZ_t),intent(in)::v
		type(adZ_t)::o
		
		complex(wp)::val
		complex(wp),dimension(:),allocatable::grad
		
		val  = u**v%x
		grad = u**v%x*( log(u)*v%d )
		
		o = adZ_t( val , grad )
	end function pow_ra

	elemental function pow_ai(u,v) result(o)
		type(adZ_t),intent(in)::u
		integer,intent(in)::v
		type(adZ_t)::o
		
		complex(wp)::val
		complex(wp),dimension(:),allocatable::grad
		
		val  = u%x**v
		grad = u%x**(v-1)*( real(v,wp)*u%d )
		
		o = adZ_t( val , grad )
	end function pow_ai

	elemental function pow_ar(u,v) result(o)
		type(adZ_t),intent(in)::u
		complex(wp),intent(in)::v
		type(adZ_t)::o
		
		complex(wp)::val
		complex(wp),dimension(:),allocatable::grad
		
		val  = u%x**v
! 		grad = u%x**v*( v*u%d/u%x )
		grad = u%x**(v-1.0_wp)*( v*u%d )
		
		o = adZ_t( val , grad )
	end function pow_ar

	elemental function pow_aa(u,v) result(o)
		type(adZ_t),intent(in)::u
		type(adZ_t),intent(in)::v
		type(adZ_t)::o
		
		complex(wp)::val
		complex(wp),dimension(:),allocatable::grad
		
		val   = u%x**v%x
		grad  = u%x**v%x*( log(u%x)*v%d+v%x*u%d/u%x )
		
		o = adZ_t( val , grad )
	end function pow_aa

	!===============!
	!= Square Root =!
	!===============!

	elemental function sqrt_a(u) result(o)
		type(adZ_t),intent(in)::u
		type(adZ_t)::o
		
		o = adZ_t( sqrt(u%x) , u%d/( 2.0_wp*sqrt(u%x) ) )
	end function sqrt_a

	!==================!
	!= Absolute Value =!
	!==================!

	elemental function abs_a(u) result(o)
		type(adZ_t),intent(in)::u
		type(adZ_t)::o
		
		!FIXME: This derivative might be wrong
		o = adZ_t( abs(u%x) , u%x/abs(u%x)*u%d )
	end function abs_a

end module autoDiffZOperator_mod 
