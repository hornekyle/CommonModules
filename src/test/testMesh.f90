program testMesh_prg
	!! Test program for mesh_mod
	use kinds_mod
	use mesh_mod
	implicit none
	
	call testReadGmsh
	
contains

	subroutine testReadGmsh
		!! Verify operation of readGmsh
		type(mesh_t)::m
		
		call execute_command_line('gmsh -2 ./input/square.geo -o square.msh')
		
		call m%readGmsh('square.msh')
	end subroutine testReadGmsh

end program testMesh_prg
 
