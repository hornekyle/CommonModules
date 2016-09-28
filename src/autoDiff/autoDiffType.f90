module autoDiffType_mod
	!! @todo
	!! Add second type for complex numbers
	use kinds_mod
	implicit none
	private
	
	type::ad_t
		real(wp)::x
		real(wp),dimension(:),allocatable::d
	contains
		procedure::val
		procedure::der
		procedure::grad
	end type
	
	interface ad_t
		module procedure newAD_valIdx
		module procedure newAD_valGrad
	end interface
	
	interface nint
		module procedure nint_a
	end interface
	
	public::ad_t
	public::nint
	
contains
	
	!================!
	!= Constructors =!
	!================!

	elemental function newAD_valIdx(value,N,idx) result(self)
		real(wp),intent(in)::value
		integer,intent(in)::N
		integer,intent(in),optional::idx
		type(ad_t)::self
		
		self%x = value
		allocate(self%d(N))
		self%d = 0.0_wp
		
		if(present(idx)) then
			self%d(idx) = 1.0_wp
		end if
	end function newAD_valIdx

	pure function newAD_valGrad(value,grad) result(self)
		real(wp),intent(in)::value
		real(wp),dimension(:),intent(in)::grad
		type(ad_t)::self
		
		self%x = value
		self%d = grad
	end function newAD_valGrad

	!=================!
	!= ad_t Routines =!
	!=================!

	elemental function val(self) result(o)
		class(ad_t),intent(in)::self
		real(wp)::o
		
		o = self%x
	end function val

	elemental function der(self,idx) result(o)
		class(ad_t),intent(in)::self
		integer,intent(in)::idx
		real(wp)::o
		
		o = self%d(idx)
	end function der

	pure function grad(self) result(o)
		class(ad_t),intent(in)::self
		real(wp),dimension(:),allocatable::o
		
		o = self%d
	end function grad

	!=============!
	!= Utilities =!
	!=============!

	elemental function nint_a(u) result(o)
		type(ad_t),intent(in)::u
		integer::o
		
		o = nint(u%val())
	end function nint_a
end module autoDiffType_mod
