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
		real(wp)::rss0,rss,p
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
		
		do k=1,lmaxIts
			r = b-matmul(A,x)
			x = x+r/D
			rss = sqrt(sum(r**2))
			
			select case(SO_TYPE)
			case(SO_QUIET)
			case(SO_SIMPLE)
				write(*,*) k,rss/rss0
			case(SO_FANCY)
				p = log( rss/rss0 )/log(ltol)
				call showProgress('Jacobi Solver',p)
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
		real(wp)::rss0,rss,p
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
				p = log( rss/rss0 )/log(ltol)
				call showProgress('Gauss-Seidel Solver',p)
			end select
			
			if(rss/rss0<ltol) exit
		end do
	end function gaussSeidel

end module solver_mod
