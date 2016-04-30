module time_mod
	!! Timing module
	use kinds_mod
	implicit none
	
contains

	function cpuTime() result(o)
		!! Return the cpu time to within an added constant (excludes sleep time)
		real(wp)::o
		
		call cpu_time(o)
	end function cpuTime

	function wallTime() result(o)
		!! Return the wall time to within an added constant (includes sleep time)
		real(wp)::o
		
		integer,parameter::ip = selected_int_kind(15)
		integer(ip)::ticks,tickRate,r
		
		call system_clock(ticks,tickRate)
		
		r = mod(ticks,tickRate)
		
		o = real(ticks/tickRate,wp)+real(r,wp)/real(tickRate,wp)
	end function wallTime

end module time_mod
