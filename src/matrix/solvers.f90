module solvers_mod
	use kinds_mod
	use time_mod
	use text_mod
	use sparse_mod
	implicit none
	private
	
	!=========================!
	!= Abstract Declarations =!
	!=========================!
	
	type,abstract::solver_t
		integer::maxIts = -1
		real(wp)::tol = 1.0E-6_wp
		character(:),allocatable::name_long
		character(:),allocatable::name_short
	contains
		procedure,private::startReport
		procedure,private::stepReport
		procedure(setup_p),deferred::setup
		procedure(step_p),deferred::step
		procedure(solve_p),deferred::solve
	end type
	
	abstract interface
		subroutine setup_p(self,A)
			import
			class(solver_t),intent(inout)::self
			class(sparse_t),intent(in)::A
		end subroutine setup_p
		
		function step_p(self,A,b,x0,l) result(x1)
			import
			class(solver_t),intent(inout)::self
			class(sparse_t),intent(in)::A
			real(wp),dimension(A%N),intent(in)::b
			real(wp),dimension(A%N),intent(in)::x0
			integer,intent(in)::l
			real(wp),dimension(:),allocatable::x1
		end function step_p
		
		function solve_p(self,A,b,x0) result(x)
			import
			class(solver_t),intent(inout)::self
			class(sparse_t),intent(in)::A
			real(wp),dimension(A%N),intent(in)::b
			real(wp),dimension(A%N),intent(in),optional::x0
			real(wp),dimension(:),allocatable::x
		end function solve_p
	end interface
	
	!=======================!
	!= Jacobi Declarations =!
	!=======================!
	
	type,extends(solver_t)::jacobi_t
		real(wp),dimension(:),allocatable::D
	contains
		procedure::setup => setup_jkb
		procedure::step  =>  step_jkb
		procedure::solve => solve_jkb
	end type
	
	interface jacobi_t
		module procedure newJacobi
	end interface
	
	!============================!
	!= GaussSeidel Declarations =!
	!============================!
	
	type,extends(solver_t)::gaussSeidel_t
		real(wp),dimension(:),allocatable::D
	contains
		procedure::setup => setup_gs
		procedure::step  =>  step_gs
		procedure::solve => solve_gs
	end type
	
	interface gaussSeidel_t
		module procedure newGaussSeidel
	end interface
	
	!===========!
	!= Exports =!
	!===========!
	
	public::solver_t
	public::jacobi_t
	public::gaussSeidel_t
	
contains

	!===================!
	!= Common Routines =!
	!===================!

	subroutine startReport(self)
		class(solver_t),intent(in)::self
		
		call showProgress(self%name_long,0.0_wp)
	end subroutine startReport

	subroutine stepReport(self,k,r)
		class(solver_t),intent(in)::self
		integer,intent(in)::k
		real(wp),intent(in)::r
		
		real(wp)::progress
		
		progress = log( r )/log(self%tol)
		call showProgress(self%name_long,progress)
	end subroutine stepReport

	!===================!
	!= Jacobi Routines =!
	!===================!

	function newJacobi() result(self)
		type(jacobi_t)::self
		
		self%name_long = 'Jacobi'
		self%name_short = ' JKB '
	end function newJacobi

	subroutine setup_jkb(self,A)
		class(jacobi_t),intent(inout)::self
		class(sparse_t),intent(in)::A
		
		self%D = A%getDiagonal()
		self%maxIts = A%N**3
	end subroutine setup_jkb
	
	function step_jkb(self,A,b,x0,l) result(x)
		class(jacobi_t),intent(inout)::self
		class(sparse_t),intent(in)::A
		real(wp),dimension(A%N),intent(in)::b
		real(wp),dimension(A%N),intent(in)::x0
		integer,intent(in)::l
		real(wp),dimension(:),allocatable::x
		
		real(wp),dimension(:),allocatable::r
		integer::k
		
		x = x0
		
		do k=1,l
			r = b-matmul(A,x)
			x = x+r/self%D
		end do
	end function step_jkb
	
	function solve_jkb(self,A,b,x0) result(x)
		class(jacobi_t),intent(inout)::self
		class(sparse_t),intent(in)::A
		real(wp),dimension(A%N),intent(in)::b
		real(wp),dimension(A%N),intent(in),optional::x0
		real(wp),dimension(:),allocatable::x
		
		real(wp),dimension(:),allocatable::r
		real(wp)::rss0,rss
		integer::k
		
		if(present(x0)) then
			x = x0
		else
			x = b/self%D
		end if
		
		r = b-matmul(A,x)
		rss0 = norm2(r)
		
		call self%startReport()
		do k=1,self%maxIts
			r = b-matmul(A,x)
			x = x+r/self%D
			
			rss = norm2(r)
			call self%stepReport(k,rss/rss0)
			if(rss/rss0<self%tol) exit
		end do
	end function solve_jkb

	!========================!
	!= GaussSeidel Routines =!
	!========================!

	function newGaussSeidel() result(self)
		type(gaussSeidel_t)::self
		
		self%name_long = 'GaussSeidel'
		self%name_short = ' G_S '
	end function newGaussSeidel

	subroutine setup_gs(self,A)
		class(gaussSeidel_t),intent(inout)::self
		class(sparse_t),intent(in)::A
		
		self%D = A%getDiagonal()
		self%maxIts = A%N**3
	end subroutine setup_gs
	
	function step_gs(self,A,b,x0,l) result(x)
		class(gaussSeidel_t),intent(inout)::self
		class(sparse_t),intent(in)::A
		real(wp),dimension(A%N),intent(in)::b
		real(wp),dimension(A%N),intent(in)::x0
		integer,intent(in)::l
		real(wp),dimension(:),allocatable::x
		
		real(wp),dimension(:),allocatable::r
		integer::k,i
		
		x = x0
		r = [( 0.0_wp, i=1,A%N )]
		
		do k=1,l
			do i=1,A%N
				r(i) = b(i)-(A%rows(i).o.x)
				x(i) = x(i)+r(i)/self%D(i)
			end do
		end do
	end function step_gs
	
	function solve_gs(self,A,b,x0) result(x)
		class(gaussSeidel_t),intent(inout)::self
		class(sparse_t),intent(in)::A
		real(wp),dimension(A%N),intent(in)::b
		real(wp),dimension(A%N),intent(in),optional::x0
		real(wp),dimension(:),allocatable::x
		
		real(wp),dimension(:),allocatable::r
		real(wp)::rss0,rss
		integer::k,i
		
		if(present(x0)) then
			x = x0
		else
			x = b/self%D
		end if
		
		r = b-matmul(A,x)
		rss0 = norm2(r)
		
		call self%startReport()
		do k=1,self%maxIts
			do i=1,A%N
				r(i) = b(i)-(A%rows(i).o.x)
				x(i) = x(i)+r(i)/self%D(i)
			end do
			
			rss = norm2(r)
			call self%stepReport(k,rss/rss0)
			if(rss/rss0<self%tol) exit
		end do
	end function solve_gs

end module solvers_mod