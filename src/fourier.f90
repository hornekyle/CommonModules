module fourier_mod
	!! Module for simplified access to the FFTW3 library
	use kinds_mod
	use iso_c_binding
	use fftw3_mod
	implicit none
	private
	
	!==============!
	!= Interfaces =!
	!==============!
	
	interface FFT
		!! Compute the FFT of a dataset
		module procedure FFT_r1
		module procedure FFT_r2
		
		module procedure FFT_c1
		module procedure FFT_c2
	end interface
	
	interface iFFT
		!! Compute the inverse FFT of a dataset
		module procedure iFFT_r1
		module procedure iFFT_r2
		
		module procedure iFFT_c1
		module procedure iFFT_c2
	end interface
	
	interface DFT
		!! Compute the DFT of a dataset
		module procedure DFT_r1
		module procedure DFT_c1

		module procedure DFT_r2
		module procedure DFT_c2
	end interface

	interface iDFT
		!! Compute the inverse DFT of a dataset
		module procedure iDFT_r1
		module procedure iDFT_c1

		module procedure iDFT_r2
		module procedure iDFT_c2
	end interface
	
	!===========!
	!= Exports =!
	!===========!
	
	public::FFT_freq
	public::FFT,iFFT
	public::DFT,iDFT
	
	! Types
	public::wp
	
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

	!==========!
	!= 1D FFT =!
	!==========!

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

	!==========!
	!= 2D FFT =!
	!==========!

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

	!===========!
	!= 1D iFFT =!
	!===========!

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

	!===========!
	!= 2D iFFT =!
	!===========!

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

	!============!
	!== 1D DFT ==!
	!============!

	function DFT_r1(input) result(output)
		real(wp),dimension(0:),intent(in)::input
		real(wp),dimension(:),allocatable::output
		
		complex(wp),dimension(:),allocatable::buf
		integer::n,k
		complex(wp)::z
		
		allocate(buf(0:size(input)-1))
		buf = cmplx(0.0,0.0_wp,dp)
		
		do k=0,size(input)-1
			buf(k) = cmplx(0.0_wp,0.0_wp,wp)
			do n=0,size(input)-1
				z = -cmplx(0.0_wp,2.0_wp*PI*real(k*n,wp)/real(size(input),wp),wp)
				buf(k) = buf(k)+cmplx(input(n),0.0_wp,wp)*exp(z)
			end do
		end do
		
		output = abs(buf)/sqrt(real(size(input),wp))
	end function DFT_r1

	function DFT_c1(input) result(output)
		complex(wp),dimension(0:),intent(in)::input
		complex(wp),dimension(:),allocatable::output
		
		complex(wp),dimension(:),allocatable::buf
		integer::n,k
		complex(wp)::z
		
		allocate(buf(0:size(input)-1))
		buf = cmplx(0.0,0.0_wp,dp)
		
		do k=0,size(input)-1
			buf(k) = cmplx(0.0_wp,0.0_wp,wp)
			do n=0,size(input)-1
				z = -cmplx(0.0_wp,2.0_wp*PI*real(k*n,wp)/real(size(input),wp),wp)
				buf(k) = buf(k)+input(n)*exp(z)
			end do
		end do
		
		output = buf/sqrt(real(size(input),wp))
	end function DFT_c1

	!============!
	!== 2D DFT ==!
	!============!

	function DFT_r2(input) result(output)
		real(wp),dimension(0:,0:),intent(in)::input
		real(wp),dimension(:,:),allocatable::output
		
		complex(wp),dimension(:,:),allocatable::buf
		integer,dimension(2)::BN
		integer,dimension(2)::k,n
		integer::k1,k2
		integer::n1,n2
		complex(wp)::z
		
		allocate(buf(0:size(input,1)-1,0:size(input,2)-1))
		buf = cmplx(0.0,0.0_wp,dp)
		BN = [size(input,1),size(input,2)]
		
		do k1=0,BN(1)-1
		do k2=0,BN(2)-1
			k = [k1,k2]
			buf(k1,k2) = cmplx(0.0_wp,0.0_wp,wp)
			do n1=0,BN(1)-1
			do n2=0,BN(2)-1
				n = [n1,n2]
				z = -cmplx(0.0_wp,2.0_wp*PI*dot_product(real(k,wp),real(n,wp)/real(BN,wp)),wp)
				buf(k1,k2) = buf(k1,k2) + cmplx(input(n1,n2),0.0_wp,wp)*exp(z)
			end do
			end do
		end do
		end do
		
		output = abs(buf)/sqrt(real(BN(1)*BN(2),wp))
	end function DFT_r2

	function DFT_c2(input) result(output)
		complex(wp),dimension(0:,0:),intent(in)::input
		complex(wp),dimension(:,:),allocatable::output
		
		complex(wp),dimension(:,:),allocatable::buf
		integer,dimension(2)::BN
		integer,dimension(2)::k,n
		integer::k1,k2
		integer::n1,n2
		complex(wp)::z
		
		allocate(buf(0:size(input,1)-1,0:size(input,2)-1))
		buf = cmplx(0.0,0.0_wp,dp)
		BN = [size(input,1),size(input,2)]
		
		do k1=0,BN(1)-1
		do k2=0,BN(2)-1
			k = [k1,k2]
			buf(k1,k2) = cmplx(0.0_wp,0.0_wp,wp)
			do n1=0,BN(1)-1
			do n2=0,BN(2)-1
				n = [n1,n2]
				z = -cmplx(0.0_wp,2.0_wp*PI*dot_product(real(k,wp),real(n,wp)/real(BN,wp)),wp)
				buf(k1,k2) = buf(k1,k2) + input(n1,n2)*exp(z)
			end do
			end do
		end do
		end do
		
		output = buf/sqrt(real(BN(1)*BN(2),wp))
	end function DFT_c2

	!=============!
	!== 1D iDFT ==!
	!=============!

	function iDFT_r1(input) result(output)
		real(wp),dimension(0:),intent(in)::input
		real(wp),dimension(:),allocatable::output
		
		complex(wp),dimension(:),allocatable::buf
		integer::n,k
		complex(wp)::z
		
		allocate(buf(0:size(input)-1))
		buf = cmplx(0.0,0.0_wp,dp)
		
		do k=0,size(input)-1
			buf(k) = cmplx(0.0_wp,0.0_wp,wp)
			do n=0,size(input)-1
				z = cmplx(0.0_wp,2.0_wp*PI*real(k*n,wp)/real(size(input),wp),wp)
				buf(k) = buf(k)+cmplx(input(n),0.0_wp,wp)*exp(z)
			end do
		end do
		
		output = abs(buf)/sqrt(real(size(input),wp))
	end function iDFT_r1

	function iDFT_c1(input) result(output)
		complex(wp),dimension(0:),intent(in)::input
		complex(wp),dimension(:),allocatable::output
		
		complex(wp),dimension(:),allocatable::buf
		integer::n,k
		complex(wp)::z
		
		allocate(buf(0:size(input)-1))
		buf = cmplx(0.0,0.0_wp,dp)
		
		do k=0,size(input)-1
			buf(k) = cmplx(0.0_wp,0.0_wp,wp)
			do n=0,size(input)-1
				z = cmplx(0.0_wp,2.0_wp*PI*real(k*n,wp)/real(size(input),wp),wp)
				buf(k) = buf(k)+input(n)*exp(z)
			end do
		end do
		
		output = buf/sqrt(real(size(input),wp))
	end function iDFT_c1

	!=============!
	!== 2D iDFT ==!
	!=============!

	function iDFT_r2(input) result(output)
		real(wp),dimension(0:,0:),intent(in)::input
		real(wp),dimension(:,:),allocatable::output
		
		complex(wp),dimension(:,:),allocatable::buf
		integer,dimension(2)::BN
		integer,dimension(2)::k,n
		integer::k1,k2
		integer::n1,n2
		complex(wp)::z
		
		allocate(buf(0:size(input,1)-1,0:size(input,2)-1))
		buf = cmplx(0.0,0.0_wp,dp)
		BN = [size(input,1),size(input,2)]
		
		do k1=0,BN(1)-1
		do k2=0,BN(2)-1
			k = [k1,k2]
			buf(k1,k2) = cmplx(0.0_wp,0.0_wp,wp)
			do n1=0,BN(1)-1
			do n2=0,BN(2)-1
				n = [n1,n2]
				z = cmplx(0.0_wp,2.0_wp*PI*dot_product(real(k,wp),real(n,wp)/real(BN,wp)),wp)
				buf(k1,k2) = buf(k1,k2) + cmplx(input(n1,n2),0.0_wp,wp)*exp(z)
			end do
			end do
		end do
		end do
		
		output = real(buf,wp)/sqrt(real(BN(1)*BN(2),wp))
	end function iDFT_r2

	function iDFT_c2(input) result(output)
		complex(wp),dimension(0:,0:),intent(in)::input
		complex(wp),dimension(:,:),allocatable::output
		
		complex(wp),dimension(:,:),allocatable::buf
		integer,dimension(2)::BN
		integer,dimension(2)::k,n
		integer::k1,k2
		integer::n1,n2
		complex(wp)::z
		
		allocate(buf(0:size(input,1)-1,0:size(input,2)-1))
		buf = cmplx(0.0,0.0_wp,dp)
		BN = [size(input,1),size(input,2)]
		
		do k1=0,BN(1)-1
		do k2=0,BN(2)-1
			k = [k1,k2]
			buf(k1,k2) = cmplx(0.0_wp,0.0_wp,wp)
			do n1=0,BN(1)-1
			do n2=0,BN(2)-1
				n = [n1,n2]
				z = cmplx(0.0_wp,2.0_wp*PI*dot_product(real(k,wp),real(n,wp)/real(BN,wp)),wp)
				buf(k1,k2) = buf(k1,k2) + input(n1,n2)*exp(z)
			end do
			end do
		end do
		end do
		
		output = buf/sqrt(real(BN(1)*BN(2),wp))
	end function iDFT_c2


end module fourier_mod
