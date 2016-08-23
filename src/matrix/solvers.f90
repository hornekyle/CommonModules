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
		real(wp)::solveTime
	contains
		procedure,private::startReport
		procedure,private::stepReport
		procedure,private::stopReport
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
	
	!====================!
	!= SOR Declarations =!
	!====================!
	
	type,extends(solver_t)::SOR_t
		real(wp),dimension(:),allocatable::D
		real(wp)::w = 1.0_wp
	contains
		procedure::setup => setup_sor
		procedure::step  =>  step_sor
		procedure::solve => solve_sor
	end type
	
	interface SOR_t
		module procedure newSOR
	end interface
	
	!==================================!
	!= conjugateGradient Declarations =!
	!==================================!
	
	type,extends(solver_t)::conjugateGradient_t
	contains
		procedure::setup => setup_cg
		procedure::step  =>  step_cg
		procedure::solve => solve_cg
	end type
	
	interface conjugateGradient_t
		module procedure newConjugateGradient
	end interface
	
	!===========!
	!= Exports =!
	!===========!
	
	public::solver_t
	public::jacobi_t
	public::gaussSeidel_t
	public::SOR_t
	public::conjugateGradient_t
	
contains

	!===================!
	!= Common Routines =!
	!===================!

	subroutine startReport(self)
		class(solver_t),intent(inout)::self
		
		call showProgress(self%name_long,0.0_wp)
		self%solveTime = cpuTime()
	end subroutine startReport

	subroutine stepReport(self,k,r)
		class(solver_t),intent(in)::self
		integer,intent(in)::k
		real(wp),intent(in)::r
		
		real(wp)::progress
		
		progress = log( r )/log(self%tol)
		if(progress>=1.0_wp) progress = 0.995_wp
		call showProgress(self%name_long,progress)
	end subroutine stepReport

	subroutine stopReport(self)
		class(solver_t),intent(inout)::self
		real(wp)::t0,t1
		
		call showProgress(self%name_long,1.0_wp)
		t0 = self%solveTime
		t1 = cpuTime()
		self%solveTime = t1-t0
	end subroutine stopReport

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
		
		x = b/self%D
		if(present(x0)) x = x0
		
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
		call self%stopReport()
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
		
		x = b/self%D
		if(present(x0)) x = x0
		
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
		call self%stopReport()
	end function solve_gs

	!================!
	!= SOR Routines =!
	!================!

	function newSOR(w) result(self)
		real(wp),intent(in)::w
		type(SOR_t)::self
		
		self%name_long = 'SuccessiveOverRelaxation'
		self%name_short = ' SOR '
		self%w = w
	end function newSOR

	subroutine setup_sor(self,A)
		class(SOR_t),intent(inout)::self
		class(sparse_t),intent(in)::A
		
		self%D = A%getDiagonal()
		self%maxIts = A%N**3
	end subroutine setup_sor
	
	function step_sor(self,A,b,x0,l) result(x)
		class(SOR_t),intent(inout)::self
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
				x(i) = x(i)+self%w*r(i)/self%D(i)
			end do
		end do
	end function step_sor
	
	function solve_sor(self,A,b,x0) result(x)
		class(SOR_t),intent(inout)::self
		class(sparse_t),intent(in)::A
		real(wp),dimension(A%N),intent(in)::b
		real(wp),dimension(A%N),intent(in),optional::x0
		real(wp),dimension(:),allocatable::x
		
		real(wp),dimension(:),allocatable::r
		real(wp)::rss0,rss
		integer::k,i
		
		x = b/self%D
		if(present(x0)) x = x0
		
		r = b-matmul(A,x)
		rss0 = norm2(r)
		
		call self%startReport()
		do k=1,self%maxIts
			do i=1,A%N
				r(i) = b(i)-(A%rows(i).o.x)
				x(i) = x(i)+self%w*r(i)/self%D(i)
			end do
			
			rss = norm2(r)
			call self%stepReport(k,rss/rss0)
			if(rss/rss0<self%tol) exit
		end do
		call self%stopReport()
	end function solve_sor

	!================!
	!= SOR Routines =!
	!================!

	function newConjugateGradient() result(self)
		type(conjugateGradient_t)::self
		
		self%name_long = 'ConjugateGradient'
		self%name_short = ' C_G '
	end function newConjugateGradient

	subroutine setup_cg(self,A)
		class(conjugateGradient_t),intent(inout)::self
		class(sparse_t),intent(in)::A
		
		self%maxIts = A%N
	end subroutine setup_cg
	
	function step_cg(self,A,b,x0,l) result(x)
		class(conjugateGradient_t),intent(inout)::self
		class(sparse_t),intent(in)::A
		real(wp),dimension(A%N),intent(in)::b
		real(wp),dimension(A%N),intent(in)::x0
		integer,intent(in)::l
		real(wp),dimension(:),allocatable::x
		
		real(wp),dimension(:),allocatable::r
		integer::k
		
		x = x0
		r = b-matmul(A,x)
		
		
	end function step_cg
	
	function solve_cg(self,A,b,x0) result(x)
		class(conjugateGradient_t),intent(inout)::self
		class(sparse_t),intent(in)::A
		real(wp),dimension(A%N),intent(in)::b
		real(wp),dimension(A%N),intent(in),optional::x0
		real(wp),dimension(:),allocatable::x
		
		real(wp),dimension(:),allocatable::r
		real(wp)::rss0,rss
		integer::k
		
		real(wp),dimension(:),allocatable::Ap,p
		real(wp)::alpha,beta,rTr,rTr_old
		
		x = b/A%getDiagonal()
		if(present(x0)) x = x0
		
		r = b-matmul(A,x)
		rss0 = norm2(r)
		
		p = r
		rTr = norm2(r)
		
		call self%startReport()
		do k=1,self%maxIts
			Ap = matmul(A,p)
			alpha = rTr/dot_product(p,Ap)
			x = x+alpha*p
			r = r-alpha*Ap
			rTr_old = rTr
			rTr = norm2(r)
			beta = rTr/rTr_old
			p = r+beta*p
			
			rss = norm2(r)
			call self%stepReport(k,rss/rss0)
			if(rss/rss0<self%tol) exit
		end do
		call self%stopReport()
	end function solve_cg

end module solvers_mod