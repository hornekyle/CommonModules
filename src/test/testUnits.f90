program testUnits_prg
	!! Test program for units_mod
	!! @todo
	!! Needs serious improvements
	use kinds_mod, only: wp
	use units_mod
	implicit none
	
	call testUnits
	
contains

	subroutine testUnits
		type(quantity_t)::dP
		type(quantity_t)::rho
		type(quantity_t)::V
		
		rho = quantity_t(1.225_wp,'kg')/quantity_t(1.0_wp,'m')**3
		dP  = quantity_t(7.25E-2_wp,'psi')
		dP = dP%convert( quantity_t(1.0_wp,'Pa') )
		
		V = sqrt(2.0_wp*dP/rho)
		
		write(*,*) V%value,V%getChar()
	end subroutine testUnits

end program testUnits_prg 
 
