program testTime_prg
	!! Test program for time_mod
	use kinds_mod
	use time_mod
	implicit none
	
	call testCpuTime
	call testWallTime
	
contains

	subroutine testCpuTime
		!! Test cpuTime to verify operation
		real(wp)::t0,t1
		
		t0 = cpuTime()
		call sleep(1)
		t1 = cpuTime()
		
		if( t1-t0>0.01_wp ) error stop "Failed cpuTime check"
	end subroutine testCpuTime 

	subroutine testWallTime
		!! Test wallTime to verify operation
		real(wp)::t0,t1
		
		t0 = wallTime()
		call sleep(1)
		t1 = wallTime()
		
		if( t1-t0<1.0_wp ) error stop "Failed wallTime check"
	end subroutine testWallTime 

end program testTime_prg