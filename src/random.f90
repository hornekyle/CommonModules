module random_mod
	use kinds_mod
	implicit none
	
contains

	subroutine setRandomSeed(S)
		integer::S
		integer::k,N

		call random_seed(size=N)
		call random_seed(put=[(k-1,k=1,N)]*S)
	end subroutine setRandomSeed
	
	function randomNormal() result(o)
		!! Return a sample from an approximate normal distribution
		!! with a mean of \(\mu=0\) and a standard deviation of
		!! \(\sigma=1\). In this approximate distribution, \(x\in[-6,6]\).
		real(wp)::o
		real(wp),dimension(12)::x

		call random_number(x)
		o = sum(x)-6.0_wp
	end function randomNormal

	function randomUniform() result(o)
		!! Return a sample from a uniform distribution
		!! in the range \(x\in[-1,1]\).
		real(wp)::o

		call random_number(o)
		o = o*2.0_wp-1.0_wp
	end function randomUniform

end module random_mod
