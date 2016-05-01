program testArray_prg
	use kinds_mod
	use array_mod
	implicit none
	
	call testMixval
	call testLinspace
	call testMeshGrid
	
contains

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

end program testArray_prg
