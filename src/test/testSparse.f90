program testSparse_prg
	use kinds_mod
	use sparse_mod
	use array_mod
	implicit none
	
! 	call testNewSparse
! 	call testSpvec
	call testJacobi
	
contains

	subroutine testNewSparse
		type(sparse_t)::A
		integer::N,M
		
		N = 3
		M = 5
		
		A = newSparse(N,M)
	end subroutine testNewSparse

	subroutine testSpvec
		type(spvec_t)::u,v,r
		
		u%i = [1,2,3]
		u%v = [1.0_wp,2.0_wp,3.0_wp]
		
		v%i = [2,3,4]
		v%v = [2.0_wp,3.0_wp,4.0_wp]
		
		write(*,*) u.o.v
		
		r = u+v
		write(*,*) r%i
		write(*,*) r%v
		
		r = 2.0_wp*u*2.0_wp
		write(*,*) r%i
		write(*,*) r%v
	end subroutine testSpvec

	subroutine testJacobi
		real(wp),parameter::Tl  = 0.0_wp
		real(wp),parameter::Tr  = 1.0_wp
		real(wp),parameter::k   = 1.0_wp
		real(wp),parameter::q0  = 10.0_wp
		real(wp),parameter::tol = 1.0E-8_wp
		
		real(wp)::Ap,Ae,Aw,dx,rss
		type(sparse_t)::A
		real(wp),dimension(:),allocatable::x,T,q,D,r
		real(wp),dimension(:,:),allocatable::Ad
		integer::N,i
		
		N = 20
		x = linspace(0.0_wp,1.0_wp,N)
		T = [( 0.0_wp , i=1,N )]
		q = [( q0     , i=1,N )]
		A = newSparse(N,N)
		
		dx = x(2)-x(1)
		
		i = 1
		call A%set(i,i,1.0_wp)
		q(i) = Tl
		do i=2,N-1
			Ae = k/dx**2
			Aw = k/dx**2
			Ap = Ae+Aw
			call A%set(i,i-1,-Aw)
			call A%set(i,i  , Ap)
			call A%set(i,i+1,-Ae)
		end do
		i = N
		call A%set(i,i,1.0_wp)
		q(i) = Tr
		
		Ad = A%toDense()
		D  = A%getDiagonal()
		
		do i=1,10000
			r = q-matmul(A,T)
			T = T+r/D
			rss = sqrt(sum(r**2))
			write(*,*) i,rss
			if(rss<tol) exit
		end do
		write(*,'(*(F6.3))') T
	end subroutine testJacobi

end program testSparse_prg
