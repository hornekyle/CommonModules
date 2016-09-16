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
		
		call execute_command_line('gmsh -2 ./input/square.geo -o square.msh')
		
		call m%readGmsh('square.msh')
		
		call m%writeVTK('square.vtk')
	end subroutine testWriteVTK

end program testMesh_prg
