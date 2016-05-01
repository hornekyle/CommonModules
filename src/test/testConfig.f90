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
		type(config_t)::cfg
		
		cfg = newConfig('./config/testConfig.cfg')
	end subroutine testNewConfig

end program testConfig_prg