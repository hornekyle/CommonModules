program testIterate_prg
	!! Test program for iterate_mod
	use kinds_mod
	use iterate_mod
	use text_mod
	use time_mod
	implicit none
	
	call testNext
	
contains

	subroutine testNext
		!! Verify operation of next
		type(iterator_t)::iter
		
		iter = newIterator([5,5],[0.0_wp,0.0_wp],[1.0_wp,1.0_wp])
		do while(.not.iter%isDone)
			call showProgress('Testing',iter%getProgress())
			call wait(0.02_wp)
			call iter%next()
		end do
	end subroutine testNext

end program testIterate_prg 
