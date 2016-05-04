program testArray_prg
	!! Test program for array_mod
	!! @todo
	!! Write test for TDMA
	use kinds_mod
	use array_mod
	implicit none
	
	call testMixval
	call testSpan
	call testFlatten
	
	call testLinspace
	call testMeshGrid
	
	call testLinearInterp
	
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

end program testArray_prg
