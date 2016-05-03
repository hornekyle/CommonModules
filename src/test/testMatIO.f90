program testMatIO_prg
	!! Test program for matIO_mod
	use kinds_mod
	use matIO_mod
	implicit none
	
	call testWriteMat
	
contains

	subroutine testWriteMat
		!! Verify operation of writeMat
		integer,parameter::N = 5
		integer,parameter::M = 4
		real(wp),dimension(N,M)::A
		
		integer::i,j
		
		forall(i=1:N,j=1:M) A(i,j) = real(i+j,wp)
		
		call writeMat('data.mat','A',A,.true.)
	end subroutine testWriteMat

end program testMatIO_prg
