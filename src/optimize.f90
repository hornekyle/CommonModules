module optimize_mod
	use kinds_mod
	implicit none
	
	type,abstract::obj_t
	contains
		procedure(eval_p),deferred::eval
	end type
	
	type,abstract::objN_t
	contains
		procedure(evalN_p),deferred::eval
	end type
	
	abstract interface
		function eval_p(self,x) result(o)
			import
			class(obj_t),intent(in)::self
			real(wp),intent(in)::x
			real(wp)::o
		end function eval_p
		
		function evalN_p(self,x) result(o)
			import
			class(objN_t),intent(in)::self
			real(wp),dimension(:),intent(in)::x
			real(wp)::o
		end function evalN_p
	end interface
	
contains


end module optimize_mod