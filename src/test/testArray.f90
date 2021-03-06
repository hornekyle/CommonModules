program testArray_prg
	!! Test program for array_mod
	use array_mod
	implicit none
	
	call testMixval
	call testSpan
	call testFlatten
	
	call testDeDup
	
	call testLinspace
	call testMeshGrid
	
	call testLinearInterp
	
	call testTDMA
	call testLU
	
	call testPoly
	
contains

	subroutine testMixval
		!! Test mixval to verify operation
		logical,dimension(1)::results
		
		integer,parameter::N = 10
		real(wp),dimension(N)::x
		real(wp),dimension(2)::test,true
		
		call random_number(x)
		
		test = mixval(x)
		true = [minval(x),maxval(x)]
		
		results(1) = all(test==true)
		
		if( .not.all(results) ) error stop "Failed mixval check"
	end subroutine testMixval

	subroutine testSpan
		!! Test mixval to verify operation
		logical,dimension(1)::results
		
		integer,parameter::N = 10
		real(wp),dimension(N)::x
		real(wp)::test,true
		
		call random_number(x)
		
		test = span(x)
		true = maxval(x)-minval(x)
		
		results(1) = test==true
		
		if( .not.all(results) ) error stop "Failed span check"
	end subroutine testSpan

	subroutine testFlatten
		!! Test mixval to verify operation
		logical,dimension(1)::results
		
		integer,parameter::N = 3
		integer,parameter::M = 4
		real(wp),dimension(N,M)::x
		real(wp),dimension(N*M)::y
		
		call random_number(x)
		y = flatten(x)
		
		results(1) = all( abs(y-reshape(x,[N*M]))<2.0_wp**4*epsilon(1.0_wp) )
		
		if( .not.all(results) ) error stop "Failed flatten check"
	end subroutine testFlatten

	subroutine testDeDup
		!! Test deDup to verify operation
		logical,dimension(1)::results
		
		results(1) = all( deDup([1,1,2,3,2,4])==[1,2,3,4] )
		
		if( .not.all(results) ) error stop "Failed deDup check"
	end subroutine testDeDup

	subroutine testLinspace
		!! Test linspace to verify operation
		logical,dimension(1)::results
		
		real(wp),dimension(:),allocatable::x,y
		integer::N,k
		
		N = 100
		x = linspace(0.0_wp,1.0_wp,N)
		y = [( real(k-1,wp)/real(N-1,wp) , k=1,N )]
		
		results(1) = norm2(x-y)<1.0E-10_wp
		
		if( .not.all(results) ) error stop "Failed linspace check"
	end subroutine testLinspace

	subroutine testMeshGrid
		!! Test meshGridX and meshGridY to verify operation
		logical,dimension(6)::results
		
		real(wp),dimension(:),allocatable::x,y
		real(wp),dimension(:,:),allocatable::XX,YY
		integer::N,M
		
		N = 5
		M = 6
		
		x = linspace(0.0_wp,1.0_wp,N)
		y = linspace(0.0_wp,1.0_wp,M)
		
		XX = meshGridX(x,y)
		YY = meshGridY(x,y)
		
		results(1) = all(XX(:,1)==x)
		results(2) = all(YY(1,:)==y)
		results(3) = all(XX(:,1)==XX(:,M))
		results(4) = all(YY(1,:)==YY(N,:))
		results(5) = all(shape(XX)==[N,M])
		results(6) = all(shape(YY)==[N,M])
		
		if( .not.all(results) ) error stop "Failed linspace check"
	end subroutine testMeshGrid

	subroutine testLinearInterp
		!! Test linearInterp to verify operation
		logical,dimension(1)::results
		
		real(wp),dimension(:),allocatable::x1,x2,y
		integer::N,k
		
		N = 100
		
		x1 = linspace(0.0_wp,5.0_wp,N)
		x2 = linspace(0.0_wp,5.0_wp,N/4)
		
		allocate(y(N))
		do k=1,N
			y(k) = linearInterp(x1(k),x2,2.0_wp*x2)
		end do
		
		results(1) = norm2(y-2.0_wp*x1)<1.0E-10_wp
		
		if( .not.all(results) ) error stop "Failed linearInterp check"
	end subroutine testLinearInterp

	subroutine testTDMA
		!! Test TDMA to verify operation
		
		real(wp),dimension(:,:),allocatable::A
		real(wp),dimension(:),allocatable::b,x,xt
		
		integer::N,k
		
		N = 10
		
		allocate( A(N,-1:+1) , x(N) , b(N) )
		xt = linspace(0.0_wp,1.0_wp,N)
		
		A(1,-1:+1) = [0.0_wp,1.0_wp,0.0_wp]
		b(   1   ) = 0.0_wp
		
		do k=2,N-1
			A(k,-1) =  1.0_wp
			A(k, 0) = -2.0_wp
			A(k,+1) =  1.0_wp
			b(  k ) =  0.0_wp
		end do
		
		A(N,-1:+1) = [0.0_wp,1.0_wp,0.0_wp]
		b(   N   ) = 1.0_wp
		
		x = TDMA(A,b)
		
		do k=1,N
			write(*,*) xt(k),x(k)
		end do
		
		write(*,*) norm2(xt-x)
	end subroutine testTDMA

	subroutine testLU
		!! Test solveLU to verify operation
		
		real(wp),dimension(:,:),allocatable::A
		real(wp),dimension(:),allocatable::x,b,bc
		integer::N
		
		do N=2,100
			if(allocated(A)) deallocate(A)
			if(allocated(b)) deallocate(b)
			
			allocate( A(N,N) , b(N) )
			
			call random_number(A)
			call random_number(b)
			
			x  = solveLU(A,b)
			bc = matmul(A,x)
			
			if( norm2(bc-b)>1.0E-10_wp ) stop 'solveLU Failed'
		end do
	end subroutine testLU

	subroutine testPoly
		!! Test polyfit
		
		real(wp),dimension(:),allocatable::x,y,f
		real(wp),dimension(:),allocatable::p,t,d,g
		integer::N
		
		N = 10
		x = linspace(0.0_wp,3.0_wp,N)
		y = -1.0_wp+x**2
		
		p = polyfit(x,y,2)
		t = [-1.0_wp,0.0_wp,1.0_wp]
		write(*,*) t-p
		if( norm2(t-p)>1.0E-10_wp ) stop 'polyfit Failed'
		
		f = polyval(p,x)
		write(*,*) y-f
		if( norm2(y-f)>1.0E-10_wp ) stop 'polyval Failed'
		
		d = polyder(p)
		g = [0.0_wp,2.0_wp]
		write(*,*) g-d
		if( norm2(g-d)>1.0E-10_wp ) stop 'polyder Failed'
	end subroutine testPoly
	
end program testArray_prg
