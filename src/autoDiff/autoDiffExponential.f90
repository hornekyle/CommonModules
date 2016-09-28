 module autoDiffExponential_mod
	!! @todo
	!! Add cosh and sinh
	use kinds_mod
	use autoDiffType_mod
	implicit none
	private
	
	! Exponential
	interface exp
		module procedure exp_a
	end interface
	public::exp
	
	! Logarithm
	interface log
		module procedure log_a
	end interface
	public::log
	
	interface log10
		module procedure log10_a
	end interface
	public::log10
	
contains

	!===============!
	!= Exponential =!
	!===============!

	elemental function exp_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o = ad_t( exp(u%x) , exp(u%x)*u%d )
	end function exp_a

	!=============!
	!= Logarithm =!
	!=============!

	elemental function log_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o = ad_t( log(u%x) , u%d/u%x )
	end function log_a

	elemental function log10_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o = ad_t( log(u%x)/log10(E) , (u%d/u%x)/log10(E) )
	end function log10_a

end module autoDiffExponential_mod
