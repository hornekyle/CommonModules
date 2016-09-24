program testMesh_prg
	!! Test program for mesh_mod
	use kinds_mod
	use mesh_mod
	implicit none
	
	call testReadGmsh
	call testWriteVTK
	
contains

	subroutine testReadGmsh
		!! Verify operation of readGmsh
		type(mesh_t)::m
		
		call execute_command_line('gmsh -2 ./input/square.geo -o square.msh')
		
		call m%readGmsh('square.msh')
	end subroutine testReadGmsh

	subroutine testWriteVTK
		!! Verify operation of readGmsh
		type(mesh_t)::m
		real(wp),dimension(:),allocatable::s
		real(wp),dimension(:,:),allocatable::v
		integer::N,k
		
		call execute_command_line('gmsh -2 ./input/square.geo -o square.msh')
		
		call m%readGmsh('square.msh')
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

end program testMesh_prg
