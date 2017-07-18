module basicSolversZ_mod
	!! Module for solving sparse linear systems
	use sparseZ_mod
	use text_mod
	implicit none
	
	integer,parameter::SO_QUIET  = 1
	integer,parameter::SO_SIMPLE = 2
	integer,parameter::SO_FANCY  = 3
	
	integer::SO_TYPE = SO_FANCY
	
	! Kinds
	public::wp
	
contains

	function jacobi(A,b,x0,tol,maxIts) result(x)
		class(sparseZ_t),intent(in)::A
		complex(wp),dimension(:),intent(in)::b
		complex(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		complex(wp),dimension(:),allocatable::x
		
		complex(wp),dimension(:),allocatable::D,r
		integer::lmaxIts
		real(wp)::ltol
		complex(wp)::rss0,rss
		integer::k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		
		rss0 = sqrt(sum(r**2))
		
		call startReport('Jacobi Solver')
		do k=1,lmaxIts
			r = b-matmul(A,x)
			x = x+r/D
			rss = sqrt(sum(r**2))
			
			call solverProgress(k,abs(rss/rss0),abs(ltol),'Jacobi Solver')
			if( abs(rss/rss0)<ltol ) exit
		end do
	end function jacobi

	function gaussSeidel(A,b,x0,tol,maxIts) result(x)
		class(sparseZ_t),intent(in)::A
		complex(wp),dimension(:),intent(in)::b
		complex(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		complex(wp),dimension(:),allocatable::x
		
		complex(wp),dimension(:),allocatable::D,r
		integer::lmaxIts
		real(wp)::ltol
		complex(wp)::rss0,rss
		integer::i,k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		
		rss0 = sqrt(sum(r**2))
		
		call startReport('Gauss-Seidel Solver')
		do k=1,lmaxIts
			do i=1,A%N
				r(i) = b(i)-(A%rows(i).o.x)
				x(i) = x(i)+r(i)/D(i)
			end do
			rss = sqrt(sum(r**2))
			
			call solverProgress(k,abs(rss/rss0),abs(ltol),'Gauss-Seidel Solver')
			if( abs(rss/rss0)<ltol ) exit
		end do
	end function gaussSeidel

	function symmetricGaussSeidel(A,b,x0,tol,maxIts) result(x)
		class(sparseZ_t),intent(in)::A
		complex(wp),dimension(:),intent(in)::b
		complex(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		complex(wp),dimension(:),allocatable::x
		
		complex(wp),dimension(:),allocatable::D,r
		integer::lmaxIts
		real(wp)::ltol
		complex(wp)::rss0,rss
		integer::i,k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		
		rss0 = sqrt(sum(r**2))
		
		call startReport('Gauss-Seidel (Sym) Solver')
		do k=1,lmaxIts
			do i=1,A%N,1
				r(i) = b(i)-(A%rows(i).o.x)
				x(i) = x(i)+r(i)/D(i)
			end do
			do i=A%N,1,-1
				r(i) = b(i)-(A%rows(i).o.x)
				x(i) = x(i)+r(i)/D(i)
			end do
			rss = sqrt(sum(r**2))
			
			call solverProgress(k,abs(rss/rss0),abs(ltol),'Gauss-Seidel (Sym) Solver')
			if( abs(rss/rss0)<ltol ) exit
		end do
	end function symmetricGaussSeidel

	function successiveOverRelaxation(A,b,w,x0,tol,maxIts) result(x)
		class(sparseZ_t),intent(in)::A
		complex(wp),dimension(:),intent(in)::b
		complex(wp),intent(in)::w
		complex(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		complex(wp),dimension(:),allocatable::x
		
		complex(wp),dimension(:),allocatable::D,r
		integer::lmaxIts
		real(wp)::ltol
		complex(wp)::rss0,rss
		integer::i,k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		
		rss0 = sqrt(sum(r**2))
		
		call startReport('SOR Solver')
		do k=1,lmaxIts
			do i=1,A%N
				r(i) = b(i)-(A%rows(i).o.x)
				x(i) = x(i)+w*r(i)/D(i)
			end do
			rss = sqrt(sum(r**2))
			
			call solverProgress(k,abs(rss/rss0),abs(ltol),'SOR Solver')
			if( abs(rss/rss0)<ltol ) exit
		end do
	end function successiveOverRelaxation

	function symmetricSuccessiveOverRelaxation(A,b,w,x0,tol,maxIts) result(x)
		class(sparseZ_t),intent(in)::A
		complex(wp),dimension(:),intent(in)::b
		complex(wp),intent(in)::w
		complex(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		complex(wp),dimension(:),allocatable::x
		
		complex(wp),dimension(:),allocatable::D,r
		integer::lmaxIts
		real(wp)::ltol
		complex(wp)::rss0,rss
		integer::i,k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		
		rss0 = sqrt(sum(r**2))
		
		call startReport('SOR (Sym) Solver')
		do k=1,lmaxIts
			do i=1,A%N,1
				r(i) = b(i)-(A%rows(i).o.x)
				x(i) = x(i)+w*r(i)/D(i)
			end do
			do i=A%N,1,-1
				r(i) = b(i)-(A%rows(i).o.x)
				x(i) = x(i)+w*r(i)/D(i)
			end do
			rss = sqrt(sum(r**2))
			
			call solverProgress(k,abs(rss/rss0),abs(ltol),'SOR (Sym) Solver')
			if( abs(rss/rss0)<ltol ) exit
		end do
	end function symmetricSuccessiveOverRelaxation

	function steepestDescent(A,b,x0,tol,maxIts) result(x)
		class(sparseZ_t),intent(in)::A
		complex(wp),dimension(:),intent(in)::b
		complex(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		complex(wp),dimension(:),allocatable::x
		
		complex(wp),dimension(:),allocatable::D,r,Ar
		complex(wp)::alpha
		integer::lmaxIts
		real(wp)::ltol
		complex(wp)::rss0,rss
		integer::k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		
		rss0 = sqrt(sum(r**2))
		
		call startReport('Steepest Descent Solver')
		do k=1,lmaxIts
			Ar = matmul(A,r)
			alpha = sum(r*r)/sum(Ar*r)
			x = x+alpha*r
			r = b-matmul(A,x)
			rss = sqrt(sum(r**2))
			
			call solverProgress(k,abs(rss/rss0),abs(ltol),'Steepest Descent Solver')
			if( abs(rss/rss0)<ltol ) exit
		end do
	end function steepestDescent

	function minimumResidual(A,b,x0,tol,maxIts) result(x)
		class(sparseZ_t),intent(in)::A
		complex(wp),dimension(:),intent(in)::b
		complex(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		complex(wp),dimension(:),allocatable::x
		
		complex(wp),dimension(:),allocatable::D,r,Ar
		complex(wp)::alpha
		integer::lmaxIts
		real(wp)::ltol
		complex(wp)::rss0,rss
		integer::k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		
		rss0 = sqrt(sum(r**2))
		
		call startReport('Minimum Residual Solver')
		do k=1,lmaxIts
			Ar = matmul(A,r)
			alpha = sum(Ar*r)/sum(Ar*Ar)
			x = x+alpha*r
			r = b-matmul(A,x)
			rss = sqrt(sum(r**2))
			
			call solverProgress(k,abs(rss/rss0),abs(ltol),'Minimum Residual Solver')
			if( abs(rss/rss0)<ltol ) exit
		end do
	end function minimumResidual

	function conjugateGradient(A,b,x0,tol,maxIts) result(x)
		class(sparseZ_t),intent(in)::A
		complex(wp),dimension(:),intent(in)::b
		complex(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		complex(wp),dimension(:),allocatable::x
		
		complex(wp),dimension(:),allocatable::D,r,Ap,p
		complex(wp)::alpha,beta,rTr,rTr_old
		integer::lmaxIts
		real(wp)::ltol
		complex(wp)::rss0,rss
		integer::k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
		D = A%getDiagonal()
		x = b/D
		if(present(x0)) x = x0
		r = b-matmul(A,x)
		
		p = r
		rTr = sum(r*r)
		
		rss0 = sqrt(sum(r**2))
		
		call startReport('Conjugate Gradient Solver')
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
			
			call solverProgress(k,abs(rss/rss0),abs(ltol),'Conjugate Gradient Solver')
			if( abs(rss/rss0)<ltol ) exit
		end do
	end function conjugateGradient

	function biConjugateGradientStabilized(A,b,x0,tol,maxIts) result(x)
		class(sparseZ_t),intent(in)::A
		complex(wp),dimension(:),intent(in)::b
		complex(wp),dimension(:),intent(in),optional::x0
		real(wp),intent(in),optional::tol
		integer,intent(in),optional::maxIts
		
		complex(wp),dimension(:),allocatable::x
		
		complex(wp),dimension(:),allocatable::D,r,Ap,p,r0,s,As
		complex(wp)::orho,rho,alpha,beta,omega
		integer::lmaxIts
		real(wp)::ltol
		complex(wp)::rss0,rss
		integer::k
		
		lmaxIts = 1000000
		ltol    = 1.0E-6_wp
		
		if(present(maxIts)) lmaxIts = maxIts
		if(present(tol   )) ltol    = tol
		
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
		
		call startReport('Bi-Conjugate Gradient (Stab) Solver')
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
			
			call solverProgress(k,abs(rss/rss0),abs(ltol),'Bi-Conjugate Gradient (Stab) Solver')
			if( abs(rss/rss0)<ltol ) exit
		end do
	end function biConjugateGradientStabilized

	subroutine startReport(nm)
		character(*),intent(in)::nm
		
		if(SO_TYPE==SO_FANCY) call showProgress(nm,0.0_wp)
	end subroutine startReport

	subroutine solverProgress(k,rr,tol,nm)
		integer,intent(in)::k
		real(wp),intent(in)::rr
		real(wp),intent(in)::tol
		character(*),intent(in)::nm
		
		real(wp)::progress
		
		select case(SO_TYPE)
		case(SO_QUIET)
		case(SO_SIMPLE)
			write(*,*) k,rr
		case(SO_FANCY)
			progress = log( rr )/log(tol)
			call showProgress(nm,progress)
		end select
	end subroutine solverProgress

end module basicSolversZ_mod
