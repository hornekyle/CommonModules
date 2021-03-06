module autoDiffTrigonometric_mod
	!! @todo
	!! Add inverse operators
	use autoDiffType_mod
	implicit none
	private
	
	! Sine
	interface sin
		module procedure sin_a
	end interface
	public::sin
	
	! Cosine
	interface cos
		module procedure cos_a
	end interface
	public::cos
	
	! Tangent
	interface tan
		module procedure tan_a
	end interface
	public::tan
	
	interface atan
		module procedure atan_a
	end interface
	public::atan
	
contains

	!========!
	!= Sine =!
	!========!

	elemental function sin_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o = ad_t( sin(u%x) , cos(u%x)*u%d )
	end function sin_a

	!==========!
	!= Cosine =!
	!==========!

	elemental function cos_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o = ad_t( cos(u%x) , -sin(u%x)*u%d )
	end function cos_a

	!===========!
	!= Tangent =!
	!===========!

	elemental function tan_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o = ad_t( tan(u%x) , (tan(u%x)**2+1.0_wp)*u%d )
	end function tan_a
	
	elemental function atan_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o = ad_t( atan(u%x) , u%d*(1.0_wp+u%x**2)**(-1) )
	end function atan_a

end module autoDiffTrigonometric_mod 
