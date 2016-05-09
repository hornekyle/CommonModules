program testSparse_prg
	use kinds_mod
	use sparse_mod
	implicit none
	
	call testNewSparse
	call testSpvec
	
contains

	subroutine testNewSparse
		type(sparse_t)::A
		integer::N,M
		
		N = 3
		M = 5
		
		A = newSparse(N,M)
	end subroutine testNewSparse

	subroutine testSpvec
		type(spvec_t)::u,v,r
		
		u%i = [1,2,3]
		u%v = [1.0_wp,2.0_wp,3.0_wp]
		
		v%i = [2,3,4]
		v%v = [2.0_wp,3.0_wp,4.0_wp]
		
		write(*,*) u.o.v
		
		r = u+v
		write(*,*) r%i
		write(*,*) r%v
		
		r = 2.0_wp*u*2.0_wp
		write(*,*) r%i
		write(*,*) r%v
	end subroutine testSpvec
	
end program testSparse_prg
