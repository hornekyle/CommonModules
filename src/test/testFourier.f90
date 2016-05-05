program testFourier_prg
	!! Test program for fourier_mod  
	!! @todo
	!! Add tests for 2d transforms  
	!! Add tests for 2d inverse transforms
	use kinds_mod
	use fourier_mod
	implicit none
	
	call testFFT
	call testIFFT
	
contains

	subroutine testFFT
		!! Test FFT to verify operation
		logical,dimension(1)::results
		
		integer,parameter::N = 10000
		real(wp),parameter::D = 5.0_wp
		real(wp),dimension(N)::t,s,f,A
		integer::k,kP
		
		forall(k=1:N) t(k) = D*(real(k-1,wp)/real(N-1,wp))
		s = cos( (2.0_wp*PI)*t )
		f = FFT_freq(t)
		A = FFT(s)
		kP = maxloc(A(:N/2),1)
		
		results(1) = abs(f(kP)-1.0_wp)<1.0E-3_wp
		
		if( .not.all(results) ) error stop "Failed FFT check"
	end subroutine testFFT

	subroutine testIFFT
		!! Test FFT to verify operation
		logical,dimension(1)::results
		
		integer,parameter::N = 10000
		real(wp),parameter::D = 5.0_wp
		real(wp),dimension(N)::t,f
		complex(wp),dimension(N)::s,fs,ffs
		integer::k
		
		forall(k=1:N) t(k) = D*(real(k-1,wp)/real(N-1,wp))
		s   = cos( (2.0_wp*PI)*t )
		f   = FFT_freq(t)
		fs  = FFT(s)
		ffs = iFFT(fs)
		
		results(1) = norm2(abs(s-ffs))<1.0E-10_wp
		
		if( .not.all(results) ) error stop "Failed iFFT check"
	end subroutine testIFFT

end program testFourier_prg
