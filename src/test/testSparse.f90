program testSparse_prg
	use kinds_mod
	use sparse_mod
	use solver_mod
	use array_mod
	use plplotlib_mod
	implicit none
	
! 	call testNewSparse
! 	call testSpvec
	call testSolvers
	
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

	subroutine testSolvers
		real(wp),parameter::Tl  = 0.0_wp
		real(wp),parameter::Tr  = 1.0_wp
		real(wp),parameter::k   = 1.0_wp
		real(wp),parameter::q0  = 10.0_wp
		real(wp),parameter::tol = 1.0E-8_wp
		
		real(wp)::Ap,Ae,Aw,dx
		type(sparse_t)::A
		real(wp),dimension(:),allocatable::x,Tj,Tg,q
		integer::N,i
		
		N  = 50
		x  = linspace(0.0_wp,1.0_wp,N)
		Tj = [( 0.0_wp , i=1,N )]
		Tg = [( 0.0_wp , i=1,N )]
		q  = [( q0     , i=1,N )]
		A  = newSparse(N,N)
		
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
		
		Tj = jacobi(A,q)
		Tg = gaussSeidel(A,q)
		
		call setup()
		call figure()
		call subplot(1,1,1)
		call xylim(mixval(x),mixval(Tj)+[0.0_wp,0.05_wp]*span(Tj))
		call plot(x,Tj,lineStyle='',markStyle='x',markColor='r')
		call plot(x,Tg,lineStyle='',markStyle='o',markColor='b')
		call ticks()
		call labels('Position #fix#fn','Temperature #fiT#fn','1D Heat Conduction with Generation')
		call show()
	end subroutine testSolvers

end program testSparse_prg
