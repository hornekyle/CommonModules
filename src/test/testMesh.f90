program testMesh_prg
	!! Test program for mesh_mod
	use mesh_mod
	use plplotlib_mod
	use array_mod
	implicit none
	
	type(mesh_t)::m
	real(wp),dimension(:),allocatable::s
	real(wp),dimension(:,:),allocatable::v
	
	call setup(fileName='testsMesh-%n.svg',figSize=[400,300])
	call testReadGmsh
	call testWriteVTK
	call testShapeFunctions
	call show()
	
contains

	subroutine testReadGmsh
		!! Verify operation of readGmsh
		
		call execute_command_line('gmsh -2 ./input/square.geo -o square.msh -format msh2')
		call m%readGmsh('square.msh')
	end subroutine testReadGmsh

	subroutine testWriteVTK
		!! Verify operation of writeVTK
		integer::N,k
		
		N = size(m%nodes)
		allocate( s(N) , v(N,3) )
		v(:,3) = 0.0_wp
		do k=1,N
			s(k) = norm2( m%nodes(k)%x )
			v(k,1:2) = m%nodes(k)%x
		end do
		
		call m%writeVTK('square.vtk')
		call m%appendScalarVTK('square.vtk',s,'s')
		call m%appendVectorVTK('square.vtk',v,'v')
	end subroutine testWriteVTK

	subroutine testShapeFunctions
		!! Verify operation of element shape functions
		
		real(wp),dimension(:),allocatable::x,y
		real(wp),dimension(:),allocatable::ex,ey
		real(wp),dimension(:),allocatable::lx,ly
		type(element_t)::e
		integer::k
		
		x = m%nodes(:)%x(1)
		y = m%nodes(:)%x(2)
		
		call figure()
		call subplot(1,1,1,aspect=span(y)/span(x))
		call xylim(mixval(x),mixval(y)+0.05_wp*[-1.0_wp,1.0_wp*span(y)])
		
		do k=1,size(m%elements)
			e = m%elements(k)
			if(e%etype/=ET_TRIANGLE_1) cycle
			ex = m%nodes(e%nodes)%x(1)
			ey = m%nodes(e%nodes)%x(2)
			
			lx = ex([1,2,3,1])
			ly = ey([1,2,3,1])
			
			call plot(lx,ly,lineStyle='-',lineColor='C0',lineWidth=2.0_wp)
		end do
		call plot(x,y,lineStyle='',markStyle='.',markColor='k',markSize=4.0_wp)
		
		call ticks()
		call labels('x','y','')
	end subroutine testShapeFunctions

end program testMesh_prg
