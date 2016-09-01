module optimize_mod
	use kinds_mod
	implicit none
	
	type,abstract::obj_t
		integer::derivativeOrder = 2
		real(wp)::stepSize = 1.0E-3_wp
	contains
		procedure::der
		procedure::rootNewton
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

	function der(self,x) result(o)
		class(obj_t),intent(in)::self
		real(wp),intent(in)::x
		real(wp)::o
		
		real(wp)::h
		
		h = self%stepSize
		
		select case(self%derivativeOrder)
		case(-1)
			o = ( self%eval(x)-self%eval(x-h) )/( h )
		case( 1)
			o = ( self%eval(x+h)-self%eval(x) )/( h )
		case( 2)
			o = ( self%eval(x+h)-self%eval(x-h) )/( 2.0_wp*h )
		end select
	end function der

	function rootNewton(self,x0,tol,maxIts) result(o)
		class(obj_t),intent(in)::self
		real(wp),intent(in)::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		real(wp)::o
		
		real(wp)::lTol
		integer::lMaxIts
		real(wp)::x,xn
		integer::k
		
		lTol = 1.0E-6_wp
		lMaxIts = 10000
		
		if(present(tol)) lTol = tol
		if(present(maxIts)) lMaxIts = maxIts
		
		xn = x0
		
		do k=1,lMaxIts
			x = xn
			xn = x-self%eval(x)/self%der(x)
			
			if(abs(xn-x)<lTol) exit
		end do
		
		o = xn
	end function rootNewton

end module optimize_mod