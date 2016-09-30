program testNetCDF_prg
	!! Test program for netCFD_mod
	use netCDF_mod
	use array_mod
	implicit none
	
	call testWrite
	call testRead
	
contains

	subroutine testWrite
		!! Test writing netCDF files
		real(wp),dimension(:),allocatable::x,y
		real(wp),dimension(:,:),allocatable::XX,YY,F
		
		x = linspace(0.0_wp,1.0_wp,100)
		y = linspace(0.0_wp,1.0_wp,101)
		
		XX = meshGridX(x,y)
		YY = meshGridY(x,y)
		
		F = XX*YY
		
		call writeGrid('data.nc',['F'],x,y)
		call writeStep('data.nc',0.0_wp,1,'F',F)
	end subroutine testWrite

	subroutine testRead
		!! Test reading netCDF files  
		!! May fail due to write failure
		logical,dimension(1)::results
		real(wp),dimension(:),allocatable::x,y,z,t
		real(wp),dimension(:,:),allocatable::XX,YY,F
		character(3),dimension(:),allocatable::vars
		
		call readGrid('data.nc',vars,x,y,z,t)
		allocate(F(size(x),size(y)))
		call readStep('data.nc',trim(adjustl(vars(1))),F,1)
		
		XX = meshGridX(x,y)
		YY = meshGridY(x,y)
		
		results(1) = norm2(F-XX*YY)<1.0E-10_wp
		
		if( .not.any(results) ) error stop "Failed testRead[Write] check"
	end subroutine testRead

end program testNetCDF_prg
