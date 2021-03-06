module spline_mod
	!! Module to construct and evaluate splines of multivariate datasets
	use array_mod
	implicit none
	private
	
	!================================!
	!= splint_t Type and Interfaces =!
	!================================!
	
	type,abstract::spline_t
		!! Abstract type from which others derive
		real(wp),dimension(:),allocatable::t0
			!! Parameterization variable data
		real(wp),dimension(:,:),allocatable::x0
			!! Datapoints for spline
	contains
		procedure(x_p),deferred::x
	end type
	
	abstract interface
		function x_p(self,t) result(o)
			!! Evaluate spline coordinate \(\vec{x}\) at point \(t\)
			import
			class(spline_t),intent(in)::self
				!! Spline
			real(wp),intent(in)::t
				!! Parametric sampling point
			real(wp),dimension(:),allocatable::o
				!! Location \(\vec{x}\) along spline
		end function x_p
	end interface
	
	!======================================!
	!= linearSpline_t Type and Interfaces =!
	!======================================!
	
	type,extends(spline_t)::linearSpline_t
		!! Linear spline type
	contains
		procedure::x => x_linearSpline
	end type
	
	interface linearSpline_t
		!! Constructor for linearSpline_t
		module procedure newLinearSpline
	end interface
	
	!=====================================!
	!= cubicSpline_t Type and Interfaces =!
	!=====================================!
	
	type,extends(spline_t)::cubicSpline_t
		!! Cubic spline type
		!!
		!! Uses hermite interpolation between datapoints with
		!! derivaties computed at construction time.
		real(wp),dimension(:,:),allocatable::d0
			!! Derivatives at datapoints \(\vec{x}_0\)
		character(:),allocatable::method
			!! Method used to compute derivatives
		real(wp)::c
			!! Tension parameter for cardinal splines
	contains
		procedure::x => x_cubicSpline
	end type
	
	interface cubicSpline_t
		!! Constructor for cubicSpline_t
		module procedure newCubicSpline
	end interface
	
	!===========!
	!= Exports =!
	!===========!
	
	public::spline_t
	public::linearSpline_t
	public::cubicSpline_t
	
	! Types
	public::wp
	
contains

	!==========================!
	!= cubicSpline_t Routines =!
	!==========================!

	function newCubicSpline(t0,x0,method,c) result(self)
		!! Constructor for cubicSpline_t
		real(wp),dimension(:),intent(in)::t0
			!! Parameterization data \(t_0\)
		real(wp),dimension(:,:),intent(in)::x0
			!! Datapoints for spline \(\vec{x}_0\)
		character(*),intent(in),optional::method
			!! Derivative calculation method
			!!
			!! Valid methods are the following:  
			!! * 'finiteDifference'  
			!! * 'catmullRom'  
			!! * 'cardinal'  
			!! * 'conventional'  
		real(wp),intent(in),optional::c
			!! Tension parameter for cardinal splines
			!!
			!! Not used by other spline types
		type(cubicSpline_t)::self
			!! New spline
		
		self%t0 = t0
		self%x0 = x0
		
		if(present(method)) then
			self%method = method
		else
			self%method = 'finiteDifference'
		end if
		
		if(present(c)) then
			self%c = c
		else
			self%c = 0.0_wp
		end if
		
		select case(self%method)
		case('finiteDifference')
			write(*,*) 'finiteDifference'
			self%d0 = finiteDifference(t0,x0)
		case('catmullRom')
			write(*,*) 'catmullRom'
			self%d0 = catmullRom(t0,x0)
		case('cardinal')
			write(*,*) 'cardinal'
			self%d0 = (1.0_wp-self%c)*catmullRom(t0,x0)
		case('conventional')
			write(*,*) 'conventional'
			self%d0 = conventional(t0,x0)
		end select
		
	contains
	
		function finiteDifference(t0,x0) result(d0)
			!! Use finite differences from \(\vec{x}_0\) to compute \(\vec{d}_0\)
			real(wp),dimension(:),intent(in)::t0
			real(wp),dimension(:,:),intent(in)::x0
			real(wp),dimension(:,:),allocatable::d0
			
			real(wp)::dtm,dtp
			real(wp)::a,b,c
			integer::N,M,k
			
			N = size(x0,1)
			M = size(x0,2)
			allocate( d0(N,M) )
			
			k = 1
			d0(k,:) = (x0(k+1,:)-x0(k  ,:))/(t0(k+1)-t0(k  ))
			
			do k=2,N-1
				dtm = t0( k )-t0(k-1)
				dtp = t0(k+1)-t0( k )
				
				a = -dtm/dtp * (dtm+dtp)**(-1)
				b =  (dtm-dtp)/(dtm*dtp)
				c =  dtp/dtm * (dtm+dtp)**(-1)
				
				d0(k,:) = a*x0(k-1,:)+b*x0(k,:)+c*x0(k+1,:)
			end do
			
			k = N
			d0(k,:) = (x0(k  ,:)-x0(k-1,:))/(t0(k  )-t0(k-1))
		end function finiteDifference
	
		function catmullRom(t0,x0) result(d0)
			!! Use finite differences from \(\vec{x}_0\) to compute \(\vec{d}_0\)
			!!
			!! Assumes second order differencing on regularly-spaced data
			real(wp),dimension(:),intent(in)::t0
			real(wp),dimension(:,:),intent(in)::x0
			real(wp),dimension(:,:),allocatable::d0
			
			integer::N,M,k
			integer::km,kp
			
			N = size(x0,1)
			M = size(x0,2)
			allocate( d0(N,M) )
			
			do k=1,N
				km = max(k-1,1)
				kp = min(k+1,N)
				
				d0(k,:) = ( x0(kp,:)-x0(km,:) )/( t0(kp)-t0(km) )
			end do
		end function catmullRom
	
		function conventional(t0,x0) result(d0)
			!! Use finite differences from \(\vec{x}_0\) to compute \(\vec{d}_0\)
			!!
			!! Selects \(\vec{d}_0\) to force second derivaties to be continuous.
			!! Endpoints have a zero second derivative.
			real(wp),dimension(:),intent(in)::t0
			real(wp),dimension(:,:),intent(in)::x0
			real(wp),dimension(:,:),allocatable::d0
			
			real(wp),dimension(:,:),allocatable::A
			real(wp),dimension(:,:),allocatable::b
			
			real(wp)::dt,dtm,dtp
			integer::N,M,k
			
			N = size(x0,1)
			M = size(x0,2)
			
			allocate( A(N,-1:+1) , b(N,M) )
			
			k  = 1
			dt = t0(k+1)-t0(k)
			A(k,-1) = 0.0_wp
			A(k, 0) = 2.0_wp/dt
			A(k,+1) = 1.0_wp/dt
			b(k, :) = -3.0_wp/dt*x0(k,:)+3.0_wp/dt*x0(k+1,:)
			
			do k=2,N-1
				dtm = t0( k )-t0(k-1)
				dtp = t0(k+1)-t0( k )
				
				A(k,-1) = 1.0_wp/dtm
				A(k, 0) = 2.0_wp/dtm+2.0_wp/dtp
				A(k,+1) = 1.0_wp/dtp
				
				b(k,:) = -3.0_wp/dtm**2*x0(k-1,:) + &
				       & (3.0_wp/dtm**2-3.0_wp/dtp**2)*x0(k,:) + &
				       &  3.0_wp/dtp**2*x0(k+1,:)
			end do
			
			k  = N
			dt = t0(k)-t0(k-1)
			A(k,-1) = 1.0_wp/dt
			A(k, 0) = 2.0_wp/dt
			A(k,+1) = 0.0_wp
			b(k, :) = -3.0_wp/dt*x0(k-1,:)+3.0_wp/dt*x0(k,:)
			
			d0 = TDMA(A,b)
		end function conventional
	
	end function newCubicSpline

	function x_cubicSpline(self,t) result(o)
		!! Evalute the cubic spline at point \(t\)
		class(cubicSpline_t),intent(in)::self
			!! Spline of curve
		real(wp),intent(in)::t
			!! Parameterization point
		real(wp),dimension(:),allocatable::o
			!! Spline location \(\vec{x}\)
		
		integer::N,M,k,i
		real(wp)::dt,xi
		
		N = size(self%x0,1)
		M = size(self%x0,2)
		
		if( t<=self%t0(1) ) then
			o = self%x0(1,:)
			return
		else if( t>=self%t0(N) ) then
			o = self%x0(N,:)
			return
		end if
		
		k  = findInterval(t,self%t0)
		dt = self%t0(k+1)-self%t0(k)
		xi = ( t-self%t0(k) )/dt
		
		allocate( o(M) )
		forall(i=1:M) o(i) = sum( H(xi)*J(dt)*C(k,i) )
		
	contains
	
		pure function H(xi) result(o)
			!! Hermite shape functions
			real(wp),intent(in)::xi
			real(wp),dimension(2,2)::o
			
			o(1,1) = (1.0_wp+2.0_wp*xi)*(1.0_wp-xi)**2
			o(1,2) = (3.0_wp-2.0_wp*xi)*xi**2
			o(2,1) = xi*(1.0_wp-xi)**2
			o(2,2) = (xi-1.0_wp)*xi**2
		end function H
	
		pure function C(k,v) result(o)
			!! Spline datapoints and derivatives
			integer,intent(in)::k,v
			real(wp),dimension(2,2)::o
			
			o(1,1) = self%x0(k  ,v)
			o(1,2) = self%x0(k+1,v)
			o(2,1) = self%d0(k  ,v)
			o(2,2) = self%d0(k+1,v)
		end function C
	
		pure function J(dt) result(o)
			!! Interval Jacobian
			real(wp),intent(in)::dt
			real(wp),dimension(2,2)::o
			
			o(1,1:2) = 1.0_wp
			o(2,1:2) = dt
		end function J
	
	end function x_cubicSpline

	!===========================!
	!= linearSpline_t Routines =!
	!===========================!

	function newLinearSpline(t0,x0) result(self)
		!! Constructor for linearSpline_t
		real(wp),dimension(:),intent(in)::t0
			!! Parameterization data \(t_0\)
		real(wp),dimension(:,:),intent(in)::x0
			!! Datapoints for spline \(\vec{x}_0\)
		type(linearSpline_t)::self
			!! New spline
		
		self%t0 = t0
		self%x0 = x0
	end function newLinearSpline

	function x_linearSpline(self,t) result(o)
		!! Evalute the linear spline at point \(t\)
		class(linearSpline_t),intent(in)::self
			!! Spline of curve
		real(wp),intent(in)::t
			!! Parameterization point
		real(wp),dimension(:),allocatable::o
			!! Spline location \(\vec{x}\)
		
		integer::N,M,k,i
		real(wp)::dt,xi
		
		N = size(self%x0,1)
		M = size(self%x0,2)
		
		if( t<=self%t0(1) ) then
			o = self%x0(1,:)
			return
		else if( t>=self%t0(N) ) then
			o = self%x0(N,:)
			return
		end if
		
		k  = findInterval(t,self%t0)
		dt = self%t0(k+1)-self%t0(k)
		xi = ( t-self%t0(k) )/dt
		
		allocate( o(M) )
		forall(i=1:M) o(i) = sum( L(xi)*self%x0(k:k+1,i) )
		
	contains
	
		pure function L(xi) result(o)
			!! Linear shape functions
			real(wp),intent(in)::xi
			real(wp),dimension(2)::o
			
			o = [1.0_wp-xi,xi]
		end function L
	
	end function x_linearSpline

end module spline_mod
