program testSparse_prg
	use kinds_mod
	use sparse_mod
	implicit none
	
	call testNewSparse
	
contains

	subroutine testNewSparse
		type(sparse_t)::A
		integer::N,M
		
		N = 3
		M = 5
		
		A = newSparse(N,M)
	end subroutine testNewSparse

end program testSparse_prg
