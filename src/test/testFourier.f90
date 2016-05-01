program testFourier_prg
	!! Test program for fourier_mod  
	!! @todo
	!! Add tests for inverse transforms  
	!! Add tests for 2d transforms  
	!! Add tests for 2d inverse transforms
	use kinds_mod
	use fourier_mod
	implicit none
	
	call testFFT
	
contains

	subroutine testFFT
		!! Test FFT to verify operation
		logical,dimension(1)::results
		
		integer,parameter::N = 100
		real(wp),parameter::D = 5.0_wp
		real(wp),dimension(N)::t,s,f,A
		integer::k,kP
		
		forall(k=1:N) t(k) = D*(real(k-1,wp)/real(N-1,wp))
		s = cos( (2.0_wp*PI)*t )
		f = tFFT(t)
		A = FFT(s)
		kP = maxloc(A(:N/2),1)
		
		results(1) = abs(f(kP)-1.0_wp)<1.0E-3_wp
		
		if( .not.all(results) ) error stop "Failed FFT check"
	end subroutine testFFT

end program testFourier_prg
