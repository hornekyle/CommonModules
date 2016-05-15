module solver_mod
	!! Module for solving sparse linear systems
	use kinds_mod
	use sparse_mod
	use text_mod
	implicit none
	
	integer,parameter::SO_QUIET  = 1
	integer,parameter::SO_SIMPLE = 2
	integer,parameter::SO_FANCY  = 3
	
	integer::SO_TYPE = SO_FANCY
	
contains

	function jacobi(A,b,x0,tol,maxIts) result(x)
		type(sparse_t),intent(in)::A
		real(wp),dimension(:),intent(in)::b
		real(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		real(wp),dimension(:),allocatable::x
		
		real(wp),dimension(:),allocatable::D,r
		integer::lmaxIts
		real(wp)::ltol
		real(wp)::rss0,rss,progress
		integer::N,k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		N = A%N
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		
		rss0 = sqrt(sum(r**2))
		
		if(SO_TYPE==SO_FANCY) call showProgress('Jacobi Solver',0.0_wp)
		
		do k=1,lmaxIts
			r = b-matmul(A,x)
			x = x+r/D
			rss = sqrt(sum(r**2))
			
			select case(SO_TYPE)
			case(SO_QUIET)
			case(SO_SIMPLE)
				write(*,*) k,rss/rss0
			case(SO_FANCY)
				progress = log( rss/rss0 )/log(ltol)
				call showProgress('Jacobi Solver',progress)
			end select
			
			if(rss/rss0<ltol) exit
		end do
	end function jacobi

	function gaussSeidel(A,b,x0,tol,maxIts) result(x)
		type(sparse_t),intent(in)::A
		real(wp),dimension(:),intent(in)::b
		real(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		real(wp),dimension(:),allocatable::x
		
		real(wp),dimension(:),allocatable::D,r
		integer::lmaxIts
		real(wp)::ltol
		real(wp)::rss0,rss,progress
		integer::N,i,k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		N = A%N
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		
		rss0 = sqrt(sum(r**2))
		
		if(SO_TYPE==SO_FANCY) call showProgress('Gauss-Seidel Solver',0.0_wp)
		
		do k=1,lmaxIts
			do i=1,N
				r(i) = b(i)-(A%rows(i).o.x)
				x(i) = x(i)+r(i)/D(i)
			end do
			rss = sqrt(sum(r**2))
			
			select case(SO_TYPE)
			case(SO_QUIET)
			case(SO_SIMPLE)
				write(*,*) k,rss/rss0
			case(SO_FANCY)
				progress = log( rss/rss0 )/log(ltol)
				call showProgress('Gauss-Seidel Solver',progress)
			end select
			
			if(rss/rss0<ltol) exit
		end do
	end function gaussSeidel

	function symmetricGaussSeidel(A,b,x0,tol,maxIts) result(x)
		type(sparse_t),intent(in)::A
		real(wp),dimension(:),intent(in)::b
		real(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		real(wp),dimension(:),allocatable::x
		
		real(wp),dimension(:),allocatable::D,r
		integer::lmaxIts
		real(wp)::ltol
		real(wp)::rss0,rss,progress
		integer::N,i,k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		N = A%N
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		
		rss0 = sqrt(sum(r**2))
		
		if(SO_TYPE==SO_FANCY) call showProgress('Gauss-Seidel (Sym) Solver',0.0_wp)
		
		do k=1,lmaxIts
			do i=1,N,1
				r(i) = b(i)-(A%rows(i).o.x)
				x(i) = x(i)+r(i)/D(i)
			end do
			do i=N,1,-1
				r(i) = b(i)-(A%rows(i).o.x)
				x(i) = x(i)+r(i)/D(i)
			end do
			rss = sqrt(sum(r**2))
			
			select case(SO_TYPE)
			case(SO_QUIET)
			case(SO_SIMPLE)
				write(*,*) k,rss/rss0
			case(SO_FANCY)
				progress = log( rss/rss0 )/log(ltol)
				call showProgress('Gauss-Seidel (Sym) Solver',progress)
			end select
			
			if(rss/rss0<ltol) exit
		end do
	end function symmetricGaussSeidel

	function successiveOverRelaxation(A,b,w,x0,tol,maxIts) result(x)
		type(sparse_t),intent(in)::A
		real(wp),dimension(:),intent(in)::b
		real(wp),intent(in)::w
		real(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		real(wp),dimension(:),allocatable::x
		
		real(wp),dimension(:),allocatable::D,r
		integer::lmaxIts
		real(wp)::ltol
		real(wp)::rss0,rss,progress
		integer::N,i,k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		N = A%N
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		
		rss0 = sqrt(sum(r**2))
		
		if(SO_TYPE==SO_FANCY) call showProgress('SOR Solver',0.0_wp)
		
		do k=1,lmaxIts
			do i=1,N
				r(i) = b(i)-(A%rows(i).o.x)
				x(i) = x(i)+w*r(i)/D(i)
			end do
			rss = sqrt(sum(r**2))
			
			select case(SO_TYPE)
			case(SO_QUIET)
			case(SO_SIMPLE)
				write(*,*) k,rss/rss0
			case(SO_FANCY)
				progress = log( rss/rss0 )/log(ltol)
				call showProgress('SOR Solver',progress)
			end select
			
			if(rss/rss0<ltol) exit
		end do
	end function successiveOverRelaxation

	function symmetricSuccessiveOverRelaxation(A,b,w,x0,tol,maxIts) result(x)
		type(sparse_t),intent(in)::A
		real(wp),dimension(:),intent(in)::b
		real(wp),intent(in)::w
		real(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		real(wp),dimension(:),allocatable::x
		
		real(wp),dimension(:),allocatable::D,r
		integer::lmaxIts
		real(wp)::ltol
		real(wp)::rss0,rss,progress
		integer::N,i,k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		N = A%N
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		
		rss0 = sqrt(sum(r**2))
		
		if(SO_TYPE==SO_FANCY) call showProgress('SOR (Sym) Solver',0.0_wp)
		
		do k=1,lmaxIts
			do i=1,N,1
				r(i) = b(i)-(A%rows(i).o.x)
				x(i) = x(i)+w*r(i)/D(i)
			end do
			do i=N,1,-1
				r(i) = b(i)-(A%rows(i).o.x)
				x(i) = x(i)+w*r(i)/D(i)
			end do
			rss = sqrt(sum(r**2))
			
			select case(SO_TYPE)
			case(SO_QUIET)
			case(SO_SIMPLE)
				write(*,*) k,rss/rss0
			case(SO_FANCY)
				progress = log( rss/rss0 )/log(ltol)
				call showProgress('SOR (Sym) Solver',progress)
			end select
			
			if(rss/rss0<ltol) exit
		end do
	end function symmetricSuccessiveOverRelaxation

	function steepestDescent(A,b,x0,tol,maxIts) result(x)
		type(sparse_t),intent(in)::A
		real(wp),dimension(:),intent(in)::b
		real(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		real(wp),dimension(:),allocatable::x
		
		real(wp),dimension(:),allocatable::D,r,Ar
		real(wp)::alpha
		integer::lmaxIts
		real(wp)::ltol
		real(wp)::rss0,rss,progress
		integer::N,k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		N = A%N
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		
		rss0 = sqrt(sum(r**2))
		
		if(SO_TYPE==SO_FANCY) call showProgress('Steepest Descent Solver',0.0_wp)
		
		do k=1,lmaxIts
			Ar = matmul(A,r)
			alpha = sum(r*r)/sum(Ar*r)
			x = x+alpha*r
			r = b-matmul(A,x)
			rss = sqrt(sum(r**2))
			
			select case(SO_TYPE)
			case(SO_QUIET)
			case(SO_SIMPLE)
				write(*,*) k,rss/rss0
			case(SO_FANCY)
				progress = log( rss/rss0 )/log(ltol)
				call showProgress('Steepest Descent Solver',progress)
			end select
			
			if(rss/rss0<ltol) exit
		end do
	end function steepestDescent

	function minimumResidual(A,b,x0,tol,maxIts) result(x)
		type(sparse_t),intent(in)::A
		real(wp),dimension(:),intent(in)::b
		real(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		real(wp),dimension(:),allocatable::x
		
		real(wp),dimension(:),allocatable::D,r,Ar
		real(wp)::alpha
		integer::lmaxIts
		real(wp)::ltol
		real(wp)::rss0,rss,progress
		integer::N,k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		N = A%N
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		
		rss0 = sqrt(sum(r**2))
		
		if(SO_TYPE==SO_FANCY) call showProgress('Minimum Residual Solver',0.0_wp)
		
		do k=1,lmaxIts
			Ar = matmul(A,r)
			alpha = sum(Ar*r)/sum(Ar*Ar)
			x = x+alpha*r
			r = b-matmul(A,x)
			rss = sqrt(sum(r**2))
			
			select case(SO_TYPE)
			case(SO_QUIET)
			case(SO_SIMPLE)
				write(*,*) k,rss/rss0
			case(SO_FANCY)
				progress = log( rss/rss0 )/log(ltol)
				call showProgress('Minimum Residual Solver',progress)
			end select
			
			if(rss/rss0<ltol) exit
		end do
	end function minimumResidual

	function conjugateGradient(A,b,x0,tol,maxIts) result(x)
		type(sparse_t),intent(in)::A
		real(wp),dimension(:),intent(in)::b
		real(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		real(wp),dimension(:),allocatable::x
		
		real(wp),dimension(:),allocatable::D,r,Ap,p
		real(wp)::alpha,beta,rTr,rTr_old
		integer::lmaxIts
		real(wp)::ltol
		real(wp)::rss0,rss,progress
		integer::N,k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		N = A%N
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		p = r
		rTr = sum(r*r)
		
		rss0 = sqrt(sum(r**2))
		
		if(SO_TYPE==SO_FANCY) call showProgress('Conjugate Gradient Solver',0.0_wp)
		
		do k=1,lmaxIts
			Ap = matmul(A,p)
			alpha = rTr/sum(p*Ap)
			x = x+alpha*p
			r = r-alpha*Ap
			rTr_old = rTr
			rTr = sum(r*r)
			beta = rTr/rTr_old
			p = r+beta*p
			rss = sqrt(sum(r**2))
			
			select case(SO_TYPE)
			case(SO_QUIET)
			case(SO_SIMPLE)
				write(*,*) k,rss/rss0
			case(SO_FANCY)
				progress = log( rss/rss0 )/log(ltol)
				call showProgress('Conjugate Gradient Solver',progress)
			end select
			
			if(rss/rss0<ltol) exit
		end do
	end function conjugateGradient

	function biConjugateGradientStabilized(A,b,x0,tol,maxIts) result(x)
		type(sparse_t),intent(in)::A
		real(wp),dimension(:),intent(in)::b
		real(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		real(wp),dimension(:),allocatable::x
		
		real(wp),dimension(:),allocatable::D,r,Ap,p,r0,s,As
		real(wp)::orho,rho,alpha,beta,omega
		integer::lmaxIts
		real(wp)::ltol
		real(wp)::rss0,rss,progress
		integer::N,k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		N = A%N
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		r0 = r
		
		rho   = 1.0_wp
		alpha = 1.0_wp
		omega = 1.0_wp
		
		Ap = [(0.0_wp,k=1,size(b))]
		p  = [(0.0_wp,k=1,size(b))]
		
		rss0 = sqrt(sum(r**2))
		
		if(SO_TYPE==SO_FANCY) call showProgress('Bi-Conjugate Gradient (Stab) Solver',0.0_wp)
		
		do k=1,lmaxIts
			orho = rho
			rho = sum(r0*r)
			beta = (rho/orho)*(alpha/omega)
			p = r+beta*(p-omega*Ap)
			Ap = matmul(A,p)
			alpha = rho/sum(r0*Ap)
			s = r-alpha*Ap
			As = matmul(A,s)
			omega = sum(As*s)/sum(As*As)
			x = x+alpha*p+omega*s
			r = s-omega*As
			rss = sqrt(sum(r**2))
			
			select case(SO_TYPE)
			case(SO_QUIET)
			case(SO_SIMPLE)
				write(*,*) k,rss/rss0
			case(SO_FANCY)
				progress = log( rss/rss0 )/log(ltol)
				call showProgress('Bi-Conjugate Gradient (Stab) Solver',progress)
			end select
			
			if(rss/rss0<ltol) exit
		end do
	end function biConjugateGradientStabilized

end module solver_mod
