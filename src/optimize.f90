module optimize_mod
	use kinds_mod
	implicit none
	
	!==================!
	!= Abstract Types =!
	!==================!
	
	type,abstract::obj_t
		integer::derivativeOrder = 2
		real(wp)::stepSize = 1.0E-3_wp
	contains
		procedure::der1
		procedure::der2
		procedure::rootNewton
		procedure::minNewton
		procedure(eval_p),deferred::eval
	end type
	
	type,abstract::objN_t
		integer::derivativeOrder = 2
		real(wp)::stepSize = 1.0E-3_wp
	contains
		procedure::grad
		procedure(evalN_p),deferred::eval
	end type
	
	!=======================!
	!= Abstract Interfaces =!
	!=======================!
	
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
	
	!==================!
	!= Definite Types =!
	!==================!
	
! 	type,extends(obj_t)::lineSearch_t
! 		class(objN_t),allocatable::parent
! 	contains
! 		procedure(evalN_p)::eval_lineSearch
! 	end type
	
contains

	function der1(self,x) result(o)
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
	end function der1

	function der2(self,x) result(o)
		class(obj_t),intent(in)::self
		real(wp),intent(in)::x
		real(wp)::o
		
		real(wp)::h
		
		h = self%stepSize
		
		select case(self%derivativeOrder)
		case(-1)
			o = ( self%eval(x-2.0_wp*h)-2.0_wp*self%eval(x-h)+self%eval(x) )/( h**2 )
		case( 1)
			o = ( self%eval(x+2.0_wp*h)-2.0_wp*self%eval(x+h)+self%eval(x) )/( h**2 )
		case( 2)
			o = ( self%eval(x+h)-2.0_wp*self%eval(x)+self%eval(x-h) )/( h**2 )
		end select
	end function der2

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
			xn = x-self%eval(x)/self%der1(x)
			
			if(abs(xn-x)<lTol) exit
		end do
		
		o = xn
	end function rootNewton

	function minNewton(self,x0,tol,maxIts) result(o)
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
			xn = x-self%der1(x)/self%der2(x)
			
			if(abs(xn-x)<lTol) exit
		end do
		
		o = xn
	end function minNewton

	function grad(self,x) result(o)
		class(objN_t),intent(in)::self
		real(wp),dimension(:),intent(in)::x
		real(wp),dimension(:),allocatable::o
		
		real(wp)::h
		real(wp),dimension(:,:),allocatable::I
		integer::N,k
		
		N = size(x)
		
		o = [( 0.0_wp, k=1,N )]
		allocate(I(N,N))
		I = 0.0_wp
		forall(k=1:N) I(k,k) = 1.0_wp
		
		h = self%stepSize
		
		select case(self%derivativeOrder)
		case(-1)
			do k=1,N
				o(k) = ( self%eval(x)-self%eval(x-h*I(k,:)) )/( h )
			end do
		case( 1)
			do k=1,N
				o(k) = ( self%eval(x+h*I(k,:))-self%eval(x) )/( h )
			end do
		case( 2)
			do k=1,N
				o(k) = ( self%eval(x+h*I(k,:))-self%eval(x-h*I(k,:)) )/( 2.0_wp*h )
			end do
		end select
	end function grad

end module optimize_mod