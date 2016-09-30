module time_mod
	!! Timing module
	use kinds_mod
	use iso_c_binding
	implicit none
	private
	
	public::cpuTime
	public::wallTime
	public::wait
	
	! Types
	public::wp
	
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

	subroutine wait(dt)
		!! Make the thread sleep
		real(wp),intent(in)::dt
			!! Time to sleep in seconds
		
		integer(c_int)::usec
		integer(c_int)::ret
		
		interface
			function doSleep(usec) result(e) bind(C,name='usleep')
				use iso_c_binding
				integer(c_int),value::usec
				integer(c_int)::e
			end function doSleep
		end interface
		
		usec = nint(dt*1.0E6)
		ret  = doSleep(usec)
	end subroutine wait

end module time_mod
