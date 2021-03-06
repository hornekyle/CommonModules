program testStats_prg
	!! Test program for stats_mod
	use array_mod
	use constants_mod
	use stats_mod
	implicit none
	
	call testSetRandomSeed
	call testRandomUniform
	call testRandomNormal
	
	call testMean
	call testStDev
	
	call testKDE
	
contains

	subroutine testSetRandomSeed
		!! Test setRandomSeed to verify operation
		logical,dimension(2)::results
		
		integer,parameter::N = 10
		real(wp),dimension(N)::x,y,z
		
		call setRandomSeed(1)
		call random_number(x)
		
		call setRandomSeed(2)
		call random_number(y)
		
		call setRandomSeed(1)
		call random_number(z)
		
		results(1) = any(x==y)
		results(2) = .not.all(x==z)
		
		if( any(results) ) error stop "Failed setRandomSeed check"
	end subroutine testSetRandomSeed

	subroutine testRandomUniform
		!! Test randomUniform to verify approximate distribution properties
		logical,dimension(4)::results
		
		integer,parameter::N = 1000000
		real(wp),dimension(N)::x
		
		call setRandomSeed(1)
		x = randomUniform(N)
		
		results(1) = abs(mean(x))>5.0E-3_wp
		results(2) = abs(stDev(x)-sqrt(3.0_wp)**(-1))>5.0E-3_wp
		results(3) = any(x> 1.0_wp+epsilon(1.0_wp))
		results(4) = any(x<-1.0_wp-epsilon(1.0_wp))
		
		if( any(results) ) error stop "Failed randomUniform check"
	end subroutine testRandomUniform

	subroutine testRandomNormal
		!! Test randomNormal to verify approximate distribution properties
		logical,dimension(4)::results
		
		integer,parameter::N = 1000000
		real(wp),dimension(N)::x
		
		call setRandomSeed(1)
		x = randomNormal(N)
		
		results(1) = abs(mean(x))>5.0E-3_wp
		results(2) = abs(stdev(x)-1.0_wp)>5.0E-3_wp
		results(3) = any(x> 6.0_wp+12.0_wp*epsilon(1.0_wp))
		results(4) = any(x<-6.0_wp-12.0_wp*epsilon(1.0_wp))
		
		if( any(results) ) error stop "Failed randomNormal check"
	end subroutine testRandomNormal

	subroutine testMean
		!! Test mean to verify operation
		logical,dimension(1)::results
		
		results(1) = abs(mean([0.0_wp,1.0_wp,2.0_wp,3.0_wp,4.0_wp])-2.0_wp)<2.0_wp**4*epsilon(1.0_wp)
		
		if( .not.all(results) ) error stop "Failed mean check"
	end subroutine testMean

	subroutine testStDev
		!! Test stDev to verify operation
		logical,dimension(1)::results
		
		results(1) = abs(stDev([-1.0_wp,0.0_wp,1.0_wp])-1.0_wp)<2.0_wp**4*epsilon(1.0_wp)
		
		if( .not.all(results) ) error stop "Failed mean check"
	end subroutine testStDev

	subroutine testKDE
		!! Test KDE to verify operation
		
		real(wp),dimension(:),allocatable::s,x,y,t
		
		x = linspace(-6.0_wp,6.0_wp,100)
		s = randomNormal(1000*000)
		y = KDE(s,x)
		t = 1.0_wp/(2.0_wp*PI)*exp(-0.5_wp*x**2)
		
		if( norm2(t-y)>1.0_wp ) error stop "Failed KDE check"
	end subroutine testKDE

end program testStats_prg 
