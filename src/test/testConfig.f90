program testConfig_prg
	!! Test program for config_mod
	!! @todo
	!! Add tests for each datatype
	use config_mod
	use text_mod
	implicit none
	
	call testNewConfig
	
contains

	subroutine testNewConfig
		!! Verify the operation of newConfig
		logical,dimension(1)::results
		type(config_t)::cfg
		
		logical::tLogical
		integer::tInteger
		real(wp)::tReal
		complex(wp)::tComplex
		real(wp),dimension(:),allocatable::tVector
		real(wp),dimension(:,:),allocatable::tMatrix
		character(:),allocatable::tString
		
		cfg = config_t('./input/testConfig.cfg')
		call cfg%writeContents(stdout)
		
		results(1) = allocated(cfg%pairs)
		
		tLogical = cfg%getLogical('logical')
		tInteger = cfg%getInteger('integer')
		tReal    = cfg%getReal('real')
		tComplex = cfg%getComplex('complex')
		tVector  = cfg%getVector('vector')
		tMatrix  = cfg%getMatrix('matrix')
		tString  = cfg%getString('string')
		
		if( .not.all(results) ) error stop "Failed newConfig check"
	end subroutine testNewConfig

end program testConfig_prg
