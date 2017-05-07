module autoDiffZType_mod
	!! @todo
	!! Add second type for complex numbers
	use kinds_mod
	implicit none
	private
	
	type::adZ_t
		complex(wp)::x
		complex(wp),dimension(:),allocatable::d
	contains
		procedure::val
		procedure::der
		procedure::grad
	end type
	
	interface adZ_t
		module procedure newAD_valIdx
		module procedure newAD_valGrad
	end interface
	
	public::adZ_t
	
	public::wp
	
contains
	
	!================!
	!= Constructors =!
	!================!

	elemental function newAD_valIdx(value,N,idx) result(self)
		complex(wp),intent(in)::value
		integer,intent(in)::N
		integer,intent(in),optional::idx
		type(adZ_t)::self
		
		self%x = value
		allocate(self%d(N))
		self%d = 0.0_wp
		
		if(present(idx)) then
			self%d(idx) = 1.0_wp
		end if
	end function newAD_valIdx

	pure function newAD_valGrad(value,grad) result(self)
		complex(wp),intent(in)::value
		complex(wp),dimension(:),intent(in)::grad
		type(adZ_t)::self
		
		self%x = value
		self%d = grad
	end function newAD_valGrad

	!=================!
	!= adZ_t Routines =!
	!=================!

	elemental function val(self) result(o)
		class(adZ_t),intent(in)::self
		complex(wp)::o
		
		o = self%x
	end function val

	elemental function der(self,idx) result(o)
		class(adZ_t),intent(in)::self
		integer,intent(in)::idx
		complex(wp)::o
		
		o = self%d(idx)
	end function der

	pure function grad(self) result(o)
		class(adZ_t),intent(in)::self
		complex(wp),dimension(:),allocatable::o
		
		o = self%d
	end function grad

	!=============!
	!= Utilities =!
	!=============!

end module autoDiffZType_mod
