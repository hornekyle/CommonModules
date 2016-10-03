program testUnits_prg
	!! Test program for units_mod
	!! @todo
	!! Needs serious improvements
	use units_mod
	use constants_mod
	implicit none
	
	call testUnits
	
contains

	subroutine testUnits
		type(quantity_t)::dP
		type(quantity_t)::rho
		type(quantity_t)::V
		type(quantity_t)::A
		type(quantity_t)::D
		type(quantity_t)::F
		type(quantity_t)::P
		
		rho = quantity_t(1.225_wp,'kg')/quantity_t(1.0_wp,'m')**3
		dP  = quantity_t(7.25E-2_wp,'psi')
		V = sqrt(2.0_wp*dP/rho)
		
		D   = quantity_t(3.5_wp,'in')
		A   = PI*D**2/4.0_wp
		F   = A*dP
		P   = V*F
		write(*,*) P%value,P%getChar()
		P   = P%convert( quantity_t(1.0_wp,'W') )
		write(*,*) P%value,P%getChar()
	end subroutine testUnits

end program testUnits_prg 
 
