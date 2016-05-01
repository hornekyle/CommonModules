module fourier_mod
	!! Module for simplified access to the FFTW3 library
	use kinds_mod
	use iso_c_binding
	use fftw3_mod
	implicit none
	private
	
	interface FFT
		!! Compute the FFT of a dataset
		module procedure FFT_r1
		module procedure FFT_c1
	end interface
	
	interface iFFT
		!! Compute the inverse FFT of a dataset
		module procedure iFFT_r1
		module procedure iFFT_c1
	end interface
	
	public::FFT_freq
	public::FFT
	public::iFFT
	
contains

	!=============!
	!= Utilities =!
	!=============!

	function FFT_freq(t) result(f)
		!! Compute the frequencies from time for an FFT
		real(wp),dimension(:),intent(in)::t
			!! Sample times of input signal to FFT
		real(wp),dimension(:),allocatable::f
			!! Frequencies of FFT output
		
		integer::N,k
		
		N = size(t)
		
		allocate(f(N))
		
		do k=1,N
			f(k) = real(k-1,wp)/( t(N)-t(1) )
		end do
	end function FFT_freq

	!=================!
	!= 1D Transforms =!
	!=================!

	function FFT_r1(u) result(o)
		real(wp),dimension(:),intent(in)::u
		real(wp),dimension(:),allocatable::o
		
		integer(c_int),parameter::d = 1
		complex(c_double_complex),dimension(:),allocatable::in,out
		integer(c_int)::N
		type(c_ptr)::plan
		
		N = size(u)
		allocate(in(N),out(N),o(N))
		
		in = u
		plan = fftw_plan_dft(d,[N],in,out,FFTW_FORWARD,FFTW_ESTIMATE)
		call fftw_execute_dft(plan,in,out)
		call fftw_destroy_plan(plan)
		
		o = abs(out)/sqrt(real(N,wp))
	end function FFT_r1

	function FFT_c1(u) result(o)
		complex(wp),dimension(:),intent(in)::u
		complex(wp),dimension(:),allocatable::o
		
		integer(c_int),parameter::d = 1
		complex(c_double_complex),dimension(:),allocatable::in,out
		integer(c_int)::N
		type(c_ptr)::plan
		
		N = size(u)
		allocate(in(N),out(N),o(N))
		
		in = u
		plan = fftw_plan_dft(d,[N],in,out,FFTW_FORWARD,FFTW_ESTIMATE)
		call fftw_execute_dft(plan,in,out)
		call fftw_destroy_plan(plan)
		
		o = out/sqrt(real(N,wp))
	end function FFT_c1

	!=================!
	!= 2D Transforms =!
	!=================!

	function FFT_r2(u) result(o)
		real(wp),dimension(:,:),intent(in)::u
		real(wp),dimension(:,:),allocatable::o
		
		integer(c_int),parameter::d = 2
		complex(c_double_complex),dimension(:,:),allocatable::in,out
		integer(c_int)::N,M
		type(c_ptr)::plan
		
		N = size(u,1)
		M = size(u,2)
		allocate(in(N,M),out(N,M),o(N,M))
		
		in = u
		plan = fftw_plan_dft(d,[N,M],in,out,FFTW_FORWARD,FFTW_ESTIMATE)
		call fftw_execute_dft(plan,in,out)
		call fftw_destroy_plan(plan)
		
		o = abs(out)/sqrt(real(N*M,wp))
	end function FFT_r2

	function FFT_c2(u) result(o)
		complex(wp),dimension(:,:),intent(in)::u
		complex(wp),dimension(:,:),allocatable::o
		
		integer(c_int),parameter::d = 2
		complex(c_double_complex),dimension(:,:),allocatable::in,out
		integer(c_int)::N,M
		type(c_ptr)::plan
		
		N = size(u,1)
		M = size(u,2)
		allocate(in(N,M),out(N,M),o(N,M))
		
		in = u
		plan = fftw_plan_dft(d,[N,M],in,out,FFTW_FORWARD,FFTW_ESTIMATE)
		call fftw_execute_dft(plan,in,out)
		call fftw_destroy_plan(plan)
		
		o = out/sqrt(real(N*M,wp))
	end function FFT_c2

	!=========================!
	!= 1D Inverse Transforms =!
	!=========================!

	function iFFT_r1(u) result(o)
		real(wp),dimension(:),intent(in)::u
		real(wp),dimension(:),allocatable::o
		
		integer(c_int),parameter::d = 1
		complex(c_double_complex),dimension(:),allocatable::in,out
		integer(c_int)::N
		type(c_ptr)::plan
		
		N = size(u)
		allocate(in(N),out(N),o(N))
		
		in = u
		plan = fftw_plan_dft(d,[N],in,out,FFTW_BACKWARD,FFTW_ESTIMATE)
		call fftw_execute_dft(plan,in,out)
		call fftw_destroy_plan(plan)
		
		o = abs(out)/sqrt(real(N,wp))
	end function iFFT_r1

	function iFFT_c1(u) result(o)
		complex(wp),dimension(:),intent(in)::u
		complex(wp),dimension(:),allocatable::o
		
		integer(c_int),parameter::d = 1
		complex(c_double_complex),dimension(:),allocatable::in,out
		integer(c_int)::N
		type(c_ptr)::plan
		
		N = size(u)
		allocate(in(N),out(N),o(N))
		
		in = u
		plan = fftw_plan_dft(d,[N],in,out,FFTW_BACKWARD,FFTW_ESTIMATE)
		call fftw_execute_dft(plan,in,out)
		call fftw_destroy_plan(plan)
		
		o = out/sqrt(real(N,wp))
	end function iFFT_c1

	!=========================!
	!= 2D Inverse Transforms =!
	!=========================!

	function iFFT_r2(u) result(o)
		real(wp),dimension(:,:),intent(in)::u
		real(wp),dimension(:,:),allocatable::o
		
		integer(c_int),parameter::d = 2
		complex(c_double_complex),dimension(:,:),allocatable::in,out
		integer(c_int)::N,M
		type(c_ptr)::plan
		
		N = size(u,1)
		M = size(u,2)
		allocate(in(N,M),out(N,M),o(N,M))
		
		in = u
		plan = fftw_plan_dft(d,[N,M],in,out,FFTW_BACKWARD,FFTW_ESTIMATE)
		call fftw_execute_dft(plan,in,out)
		call fftw_destroy_plan(plan)
		
		o = abs(out)/sqrt(real(N*M,wp))
	end function iFFT_r2

	function iFFT_c2(u) result(o)
		complex(wp),dimension(:,:),intent(in)::u
		complex(wp),dimension(:,:),allocatable::o
		
		integer(c_int),parameter::d = 2
		complex(c_double_complex),dimension(:,:),allocatable::in,out
		integer(c_int)::N,M
		type(c_ptr)::plan
		
		N = size(u,1)
		M = size(u,2)
		allocate(in(N,M),out(N,M),o(N,M))
		
		in = u
		plan = fftw_plan_dft(d,[N,M],in,out,FFTW_BACKWARD,FFTW_ESTIMATE)
		call fftw_execute_dft(plan,in,out)
		call fftw_destroy_plan(plan)
		
		o = out/sqrt(real(N*M,wp))
	end function iFFT_c2

end module fourier_mod
