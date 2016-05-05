program testConfig_prg
	!! Test program for config_mod
	!! @todo
	!! Add tests for each datatype
	use kinds_mod
	use config_mod
	implicit none
	
	call testNewConfig
	
contains

	subroutine testNewConfig
		!! Verify the operation of newConfig
		logical,dimension(1)::results
		type(config_t)::cfg
		
		cfg = newConfig('./input/testConfig.cfg')
		
		results(1) = allocated(cfg%pairs)
		
		if( .not.all(results) ) error stop "Failed newConfig check"
	end subroutine testNewConfig

end program testConfig_prg