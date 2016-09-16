module optimize_mod
	use kinds_mod
	use array_mod
	implicit none
	private
	
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
		procedure::minNewton => minNewton_obj
		procedure(eval_p),deferred::eval
	end type
	
	type,abstract::objN_t
		integer::derivativeOrder = 2
		real(wp)::stepSize = 1.0E-3_wp
	contains
		procedure::grad
		procedure::hessian
		procedure::steepestDescent
		procedure::nelderMead
		procedure::minNewton => minNewton_objN
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
	
	type,extends(obj_t)::lineSearch_t
		class(objN_t),allocatable::parent
		real(wp),dimension(:),allocatable::x0
		real(wp),dimension(:),allocatable::n0
	contains
		procedure::parentX
		procedure::eval => eval_lineSearch
	end type
	
	!==============!
	!= Interfaces =!
	!==============!
	
	interface lineSearch_t
		module procedure newLineSearch
	end interface
	
	public::obj_t
	public::objN_t
	
	public::lineSearch_t
	
contains

	!==================!
	!= obj_t Routines =!
	!==================!

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
			x  = xn
			xn = x-self%eval(x)/self%der1(x)
			
			if(abs(xn-x)<lTol) exit
		end do
		
		o = xn
	end function rootNewton

	function minNewton_obj(self,x0,tol,maxIts) result(o)
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
			
			if( abs(xn-x)<lTol ) exit
		end do
		
		o = xn
	end function minNewton_obj

	!===================!
	!= objN_t Routines =!
	!===================!

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
		
		do k=1,N
			select case(self%derivativeOrder)
			case(-1)
				o(k) = ( self%eval(x)-self%eval(x-h*I(k,:)) )/( h )
			case( 1)
				o(k) = ( self%eval(x+h*I(k,:))-self%eval(x) )/( h )
			case( 2)
				o(k) = ( self%eval(x+h*I(k,:))-self%eval(x-h*I(k,:)) )/( 2.0_wp*h )
			end select
		end do
	end function grad

	function hessian(self,x) result(o)
		class(objN_t),intent(in)::self
		real(wp),dimension(:),intent(in)::x
		real(wp),dimension(:,:),allocatable::o
		
		real(wp)::h
		real(wp),dimension(:,:),allocatable::I
		integer::N,k
		
		N = size(x)
		
		allocate( o(N,N) , I(N,N) )
		I = 0.0_wp
		forall(k=1:N) I(k,k) = 1.0_wp
		
		h = self%stepSize
		
		do k=1,N
			select case(self%derivativeOrder)
			case(-1)
				o(k,:) = ( self%grad(x)-self%grad(x-h*I(k,:)) )/( h )
			case( 1)
				o(k,:) = ( self%grad(x+h*I(k,:))-self%grad(x) )/( h )
			case( 2)
				o(k,:) = ( self%grad(x+h*I(k,:))-self%grad(x-h*I(k,:)) )/( 2.0_wp*h )
			end select
		end do
	end function hessian

	function steepestDescent(self,x0,tol,maxIts) result(o)
		class(objN_t),intent(in)::self
		real(wp),dimension(:),intent(in)::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		real(wp),dimension(:),allocatable::o
		
		real(wp)::lTol
		integer::lMaxIts
		
		real(wp),dimension(:),allocatable::x,xn
		type(lineSearch_t)::line
		real(wp)::rss,t
		integer::k
		
		lTol = 1.0E-6_wp
		lMaxIts = 10000
		
		if(present(tol)) lTol = tol
		if(present(maxIts)) lMaxIts = maxIts
		
		x  = x0
		
		do k=1,lMaxIts
			line = lineSearch_t(self,x)
			t    = line%minNewton(0.0_wp)
			xn   = line%parentX(t)
			
			rss = norm2(xn-x)
			if( rss<lTol ) exit
			
			x = xn
		end do
		
		o = xn
	end function steepestDescent

	function nelderMead(self,x0,tol,maxIts) result(o)
		class(objN_t),intent(in)::self
		real(wp),dimension(:),intent(in)::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		real(wp),dimension(:),allocatable::o
		
		real(wp)::lTol
		integer::lMaxIts
		
		real(wp),parameter::alpha = 1.0_wp
		real(wp),parameter::beta  = 0.5_wp
		real(wp),parameter::gamma = 2.0_wp
		
		real(wp),dimension(:,:),allocatable::v
			!! Vertices of simplex
		real(wp),dimension(:),allocatable::f
			!! Function values at vertices
		
		real(wp),dimension(:),allocatable::vr
			!! Reflection vertex
		real(wp),dimension(:),allocatable::ve
			!! Expansion vertex
		real(wp),dimension(:),allocatable::vc
			!! Contraction vertex
		real(wp),dimension(:),allocatable::vm
			!! Centroid vertex
		
		real(wp)::fr
			!! Function value at reflection vertex
		real(wp)::fe
			!! Function value at expansion vertex
		real(wp)::fc
			!! Function value at contraction vertex
		
		integer::vg,vh,vs
		integer::N,i,j,k
		real(wp)::rss
		
		lTol = 1.0E-6_wp
		lMaxIts = 10000
		
		if(present(tol)) lTol = tol
		if(present(maxIts)) lMaxIts = maxIts
		
		N = size(x0)
		
		allocate( v(N,N+1) , f(N+1) )
		
		! Assume one vertex is \vec{0.0}
		v(:,1) = 0.0_wp
		do j=1,N
			forall(i=1:N,i/=j) v(i,j+1) = ( sqrt(real(N+1,wp))-1.0_wp )/( real(N,wp)*sqrt(2.0_wp) ) + x0(i)
			v(j,j+1) = ( sqrt(real(N+1,wp))-1.0_wp+real(N,wp) )/( real(N,wp)*sqrt(2.0_wp) ) + x0(j)
		end do
		
		do j=1,N+1
			f(j) = self%eval( v(:,j) )
		end do
		
		do k=1,lMaxIts
			! Find greatest, high, and small values
			vg = maxloc(f,1)
			vh = maxloc(f,1,f<f(vg))
			vs = minloc(f,1)
			
			! Compute centroid ignoring greatest vertex
			vm = ( sum(v,2)-v(:,vg) )/real(N,wp)
			
			! Consider reflection vertex
			vr = (1.0_wp+alpha)*vm-alpha*v(:,vg)
			fr = self%eval(vr)
			if( fr<f(vh) .and. fr>f(vs) ) then
				v(:,vg) = vr
				f(vg)   = fr
			end if
			
			! Invetigate another step in this direction
			if( fr<f(vs) ) then
				ve = gamma*vr+(1.0_wp-gamma)*vm
				fe = self%eval(ve)
				if( fe<f(vs) ) then
					v(:,vg) = ve
					f(vg)   = fe
				else
					v(:,vg) = vr
					f(vg)   = fr
				end if
			end if
			
			! Check if contraction needed
			if( fr>f(vh) ) then
				vc = beta*v(:,vg)+(1.0_wp-beta)*vm
				fc = self%eval(vc)
				if( fc<f(vg) ) then
					v(:,vg) = vc
					f(vg)   = fc
				else
					forall(j=1:N+1,j/=vs) v(:,j) = v(:,vs)+( v(:,j)-v(:,vs) )/2.0_wp
					f(vg) = self%eval( v(:,vg) )
					f(vh) = self%eval( v(:,vh) )
				end if
			end if
			
			! Check convergence
			vm = ( sum(v,2)-v(:,vs) )/real(N,wp)
			rss = norm2(v(:,vs)-vm)
			
			if( rss<lTol) exit
		end do
		
		o = v(:,vs)
	end function nelderMead

	function minNewton_objN(self,x0,tol,maxIts) result(o)
		class(objN_t),intent(in)::self
		real(wp),dimension(:),intent(in)::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		real(wp),dimension(:),allocatable::o
		
		real(wp)::lTol
		integer::lMaxIts
		real(wp),dimension(:),allocatable::x,xn
		integer::k
		
		lTol = 1.0E-6_wp
		lMaxIts = 10000
		
		if(present(tol)) lTol = tol
		if(present(maxIts)) lMaxIts = maxIts
		
		xn = x0
		
		do k=1,lMaxIts
			x  = xn
			xn = x-solveLU(self%hessian(x),self%grad(x))
			
			if( norm2(xn-x)<lTol ) exit
		end do
		
		o = xn
	end function minNewton_objN

	!=========================!
	!= lineSearch_t Routines =!
	!=========================!

	function newLineSearch(obj,x) result(self)
		class(objN_t),intent(in)::obj
		real(wp),dimension(:),intent(in)::x
		type(lineSearch_t)::self
		
		real(wp),dimension(:),allocatable::g
		
		g = obj%grad(x)
		
		allocate(self%parent,source=obj)
		self%x0 = x
		self%n0 = g/norm2(g)
	end function newLineSearch

	function parentX(self,x) result(o)
		class(lineSearch_t),intent(in)::self
		real(wp),intent(in)::x
		real(wp),dimension(:),allocatable::o
		
		o = self%x0+x*self%n0
	end function parentX

	function eval_lineSearch(self,x) result(o)
		class(lineSearch_t),intent(in)::self
		real(wp),intent(in)::x
		real(wp)::o
		
		o = self%parent%eval(self%parentX(x))
	end function eval_lineSearch

end module optimize_mod
