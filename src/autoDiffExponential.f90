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
		
		o = ad_t( exp(u%val()) , exp(u%val())*u%grad() )
	end function exp_a

	!=============!
	!= Logarithm =!
	!=============!

	elemental function log_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o = ad_t( log(u%val()) , u%grad()/u%val() )
	end function log_a

	elemental function log10_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o = ad_t( log(u%val())/log10(E) , (u%grad()/u%val())/log10(E) )
	end function log10_a

end module autoDiffExponential_mod
