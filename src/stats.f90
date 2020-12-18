module stats_mod
	!! Basic probability and statistics module
	use constants_mod
	use kinds_mod
	implicit none
	private
	
	!==============!
	!= Interfaces =!
	!==============!
	
	interface randomUniform
		!! Return sample(s) \(x\) from a uniform distribution such that \(x\in[-1,1]\)
		module procedure randomUniform_s
		module procedure randomUniform_a1
	end interface randomUniform
	
	interface randomNormal
		!! Return sample(s) \(x\) from an approximate normal distribution such that \(x\in[-6,6]\), \( \sigma \approx 1.0 \) and \( \mu \approx 0.0 \).
		module procedure randomNormal_s
		module procedure randomNormal_a1
	end interface
	
	!===========!
	!= Exports =!
	!===========!
	
	public::setRandomSeed
	
	public::randomUniform
	public::randomNormal
	public::randomInteger
	
	public::mean
	public::stDev
	
	public::KDE
	
	! Types
	public::wp
	
contains

	!========================!
	!= Probability Routines =!
	!========================!

	subroutine setRandomSeed(S)
		!! Set the pseudo-random number generator seed
		integer::S
		integer::k,N
		
		call random_seed(size=N)
		call random_seed(put=[(k-1,k=1,N)]*S)
	end subroutine setRandomSeed

	function randomUniform_s() result(o)
		!! Return a sample from a uniform distribution
		!! in the range \(x\in[-1,1]\).
		real(wp)::o
			!! Pseudo-random number
		
		call random_number(o)
		o = o*2.0_wp-1.0_wp
	end function randomUniform_s

	function randomUniform_a1(N) result(o)
		!! Return \(N\) samples from a uniform distribution
		!! in the range \(x\in[-1,1]\).
		integer,intent(in)::N
			!! Number of samples
		real(wp),dimension(:),allocatable::o
			!! Pseudo-random number array
		
		integer::k
		
		allocate(o(N))
		
		do k=1,N
			o(k) = randomUniform_s()
		end do
	end function randomUniform_a1

	function randomNormal_s() result(o)
		!! Return a sample from an approximate normal distribution
		!! with a mean of \( \mu \approx 0.0\) and a standard deviation of
		!! \( \sigma \approx 1.0 \). In this approximate distribution, \(x\in[-6,6]\).
		real(wp)::o
			!! Pseudo-random number
		
		real(wp),dimension(12)::x
		
		call random_number(x)
		o = sum(x)-6.0_wp
	end function randomNormal_s

	function randomNormal_a1(N) result(o)
		!! Return \(N\) samples from an approximate normal distribution
		!! with a mean of \(\mu=0\) and a standard deviation of
		!! \(\sigma=1\). In this approximate distribution, \(x\in[-6,6]\).
		integer,intent(in)::N
			!! Number of samples
		real(wp),dimension(:),allocatable::o
			!! Pseudo-random number array
		
		integer::k
		
		allocate(o(N))
		
		do k=1,N
			o(k) = randomNormal_s()
		end do
	end function randomNormal_a1

	function randomInteger(N) result(o)
		!! Return a random integer \(i\in[1,N]\)
		integer,intent(in)::N
			!! Upper limit of range
		integer::o
			!! Random integer
		
		real(wp)::x
		
		x = (randomUniform()+1.0_wp)/2.0_wp
		
		o = floor(x*real(N,wp)+1.0_wp)
	end function randomInteger
	
	!================================!
	!= Population Property Routines =!
	!================================!

	function mean(d) result(o)
		!! Compute the mean of an input array
		real(wp),dimension(:),intent(in)::d
			!! Data to process
		real(wp)::o
			!! Mean
		
		o = sum(d)/real(size(d),wp)
	end function mean

	function stDev(d) result(o)
		!! Compute the standard deviation of an input array
		real(wp),dimension(:),intent(in)::d
			!! Data to process
		real(wp)::o
			!! Standard deviation
		
		o = sqrt(sum((d-sum(d)/real(size(d),wp))**2)/real(size(d)-1,wp))
	end function stDev

	!============================!
	!= Kernel Density Estimator =!
	!============================!

	function KDE(sample,rng) result(p)
		real(wp),dimension(:),intent(in)::sample
			!! Set of samples for density estimation
		real(wp),dimension(:),intent(in)::rng
			!! Range over which to compute the density
		real(wp),dimension(:),allocatable::p
			!! Estimated density of samples over range
		
		real(wp),dimension(:),allocatable::x
		integer::Ns,Nr,k
		real(wp)::h
		
		Ns = size(sample)
		Nr = size(rng)
		
		h  = bandwidth(sample)
		allocate(p(Nr))
		p = 0.0_wp
		do k=1,Ns
			x = (rng-sample(k))/h
			p = p+(1.0_wp/(real(Ns,wp)*h))*kernel(x)
		end do
	
	contains
	
		elemental function kernel(x) result(p)
			real(wp),intent(in)::x
				!! Independent variable
			real(wp)::p
				!! Kernel value
			
			real(wp)::m,s
			
			m = 0.0_wp
			s = 1.0_wp
			p = (1/sqrt(2*PI*s**2))*exp(-(x-m)**2/(2.0_wp*s**2))
		end function kernel

		function bandwidth(sample) result(h)
			real(wp),dimension(:),intent(in)::sample
				!! Sample data
			real(wp)::h
				!! Bandwidth
			
			integer::N
			
			N = size(sample)
			h = 1.05_wp*stDev(sample)*real(N,wp)**(-1.0_wp/5.0_wp)
		end function bandwidth
	
	end function KDE

end module stats_mod
