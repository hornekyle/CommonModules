program testVtkIO_prg
	!! Test program for vtkIO_mod
	use vtkIO_mod
	use array_mod
	implicit none
	
	call testWriteVTK
	
contains

	subroutine testWriteVTK
		!! Test writing VTK files
		
		real(wp),dimension(:),allocatable::x,y
		real(wp),dimension(:,:),allocatable::XX,YY
		real(wp),dimension(:,:),allocatable::F,U,V
		integer::N,M,iou
		
		N = 100
		M = 101
		
		x = linspace(0.0_wp,1.0_wp,N)
		y = linspace(0.0_wp,1.0_wp,M)
		
		XX = MeshGridX(x,y)
		YY = MeshGridY(x,y)
		
		F = XX*YY
		U = -YY
		V =  XX
		
		open(file='data.vtk',newunit=iou)
		call writeHeaderVTK(iou,'Test File')
		call writeGridVTK(iou,XX,YY)
		call writeScalarVTK(iou,'F',F)
		call writeVectorVTK(iou,'U',U,V)
		close(iou)
	end subroutine testWriteVTK

end program testVtkIO_prg
