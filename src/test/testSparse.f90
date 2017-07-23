program testSparse_prg
	use sparse_mod
	use basicSolvers_mod
	use solvers_mod
	use array_mod
	use plplotlib_mod
	implicit none
	
	call setup(fileName='testsSparse-%n.svg',figSize=[400,300])
	call testNewSparse
	call testSpvec
	call testBasicSolvers
	call testSolvers
	call show()
	
contains

	subroutine testNewSparse
		type(sparse_t)::A
		integer::N,M
		
		N = 3
		M = 5
		
		A = sparse_t(N,M)
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

	subroutine testBasicSolvers
		real(wp),parameter::Tl  = 0.0_wp
		real(wp),parameter::Tr  = 1.0_wp
		real(wp),parameter::k   = 1.0_wp
		real(wp),parameter::q0  = 10.0_wp
		real(wp),parameter::tol = 1.0E-8_wp
		
		real(wp)::Ap,Ae,Aw,dx
		type(sparse_t)::A
		real(wp),dimension(:),allocatable::x,q
		real(wp),dimension(:),allocatable::T1,T2,T3
		integer::N,i,s
		
		N  = 1000
		s  = N/30
		allocate(x(0:N+1))
		x  = linspace(0.0_wp,1.0_wp,N+2)
		q  = [( q0     , i=1,N )]
		A  = sparse_t(N,N)
		
		allocate(T1(0:N+1))
		allocate(T2(0:N+1))
		allocate(T3(0:N+1))
		
		T1(0)   = Tl
		T1(N+1) = Tr
		
		T2(0)   = Tl
		T2(N+1) = Tr
		
		T3(0)   = Tl
		T3(N+1) = Tr
		
		dx = x(2)-x(1)
		
		i = 1
		Ae = k/dx**2
		Aw = k/dx**2
		Ap = Ae+Aw
		call A%set(i,i  , Ap)
		call A%set(i,i+1,-Ae)
		q(i) = q(i)+Aw*Tl
		do i=2,N-1
			Ae = k/dx**2
			Aw = k/dx**2
			Ap = Ae+Aw
			call A%set(i,i-1,-Aw)
			call A%set(i,i  , Ap)
			call A%set(i,i+1,-Ae)
		end do
		i = N
		Ae = k/dx**2
		Aw = k/dx**2
		Ap = Ae+Aw
		call A%set(i,i-1,-Aw)
		call A%set(i,i  , Ap)
		q(i) = q(i)+Ae*Tr
		
		T1(1:N) = biConjugateGradientStabilized(A,q)
		T2(1:N) = conjugateGradient(A,q)
		T3(1:N) = successiveOverRelaxation(A,q,1.995_wp)
		
		
		call figure()
		call subplot(1,1,1)
		call xylim(mixval(x),mixval(T1)+[0.0_wp,0.05_wp]*span(T1))
		call plot(x(::s),T1(::s),lineStyle='-',lineColor='r',markStyle='x',markColor='r')
		call plot(x(::s),T2(::s),lineStyle='-',lineColor='b',markStyle='o',markColor='b')
		call plot(x(::s),T3(::s),lineStyle='-',lineColor='c',markStyle='s',markColor='c')
		call ticks()
		call labels('Position #fix#fn','Temperature #fiT#fn','1D Heat Conduction with Generation')
		
	end subroutine testBasicSolvers

	subroutine testSolvers
		real(wp),parameter::Tl  = 0.0_wp
		real(wp),parameter::Tr  = 1.0_wp
		real(wp),parameter::k   = 1.0_wp
		real(wp),parameter::q0  = 10.0_wp
		real(wp),parameter::tol = 1.0E-8_wp
		
		real(wp)::Ap,Ae,Aw,dx
		type(sparse_t)::A
		class(solver_t),allocatable::solver
		real(wp),dimension(:),allocatable::x,q
		real(wp),dimension(:),allocatable::T1
		integer::N,i,s
		
		N  = 100
		s  = N/30
		allocate(x(0:N+1))
		x  = linspace(0.0_wp,1.0_wp,N+2)
		q  = [( q0     , i=1,N )]
		A  = sparse_t(N,N)
		
		allocate(T1(0:N+1))
		
		T1(0)   = Tl
		T1(N+1) = Tr
		
		dx = x(2)-x(1)
		
		i = 1
		Ae = k/dx**2
		Aw = k/dx**2
		Ap = Ae+Aw
		call A%set(i,i  , Ap)
		call A%set(i,i+1,-Ae)
		q(i) = q(i)+Aw*Tl
		do i=2,N-1
			Ae = k/dx**2
			Aw = k/dx**2
			Ap = Ae+Aw
			call A%set(i,i-1,-Aw)
			call A%set(i,i  , Ap)
			call A%set(i,i+1,-Ae)
		end do
		i = N
		Ae = k/dx**2
		Aw = k/dx**2
		Ap = Ae+Aw
		call A%set(i,i-1,-Aw)
		call A%set(i,i  , Ap)
		q(i) = q(i)+Ae*Tr
		
		allocate(solver,source=jacobi_t())
		call solver%setup(A)
		T1(1:N) = solver%solve(A,q)
		deallocate(solver)
		
		allocate(solver,source=gaussSeidel_t())
		call solver%setup(A)
		T1(1:N) = solver%solve(A,q)
		deallocate(solver)
		
		allocate(solver,source=SOR_t(1.99_wp))
		call solver%setup(A)
		T1(1:N) = solver%solve(A,q)
		deallocate(solver)
		
		allocate(solver,source=conjugateGradient_t())
		call solver%setup(A)
		T1(1:N) = solver%solve(A,q)
		deallocate(solver)
		
		allocate(solver,source=biCGSTAB_t())
		call solver%setup(A)
		T1(1:N) = solver%solve(A,q)
		deallocate(solver)
		
		call figure()
		call subplot(1,1,1)
		call xylim(mixval(x),mixval(T1)+[0.0_wp,0.05_wp]*span(T1))
		call plot(x(::s),T1(::s),lineStyle='-',lineColor='k',markStyle='x',markColor='r')
		call ticks()
		call labels('Position #fix#fn','Temperature #fiT#fn','1D Heat Conduction with Generation')
		
	end subroutine testSolvers

end program testSparse_prg
