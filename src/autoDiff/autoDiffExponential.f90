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
	public::exp_a
	
	! Logarithm
	interface log
		module procedure log_a
	end interface
	public::log_a
	
	interface log10
		module procedure log10_a
	end interface
	public::log10_a
	
contains

	!===============!
	!= Exponential =!
	!===============!

	function exp_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o = ad_t( exp(u%val()) , exp(u%val())*u%grad() )
	end function exp_a

	!=============!
	!= Logarithm =!
	!=============!

	function log_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o = ad_t( log(u%val()) , u%grad()/u%val() )
	end function log_a

	function log10_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o = ad_t( log(u%val())/log10(E) , (u%grad()/u%val())/log10(E) )
	end function log10_a

end module autoDiffExponential_mod
