module treeValue_mod
	use node_mod
	implicit none
	public
	
	!========================================!
	!= Evaluation Tree Types and Interfaces =!
	!========================================!
	
	! real_t
	type,extends(node_t)::real_t
		real(wp)::value
	contains
		procedure::evalR => evalR_real
		procedure::evalZ => evalZ_real
	end type
	
	interface real_t
		module procedure newReal
	end interface
	
	! imag_t
	type,extends(node_t)::imag_t
		real(wp)::value
	contains
		procedure::evalR => evalR_imag
		procedure::evalZ => evalZ_imag
	end type
	
	interface imag_t
		module procedure newImag
	end interface
	
	! var_t
	type,extends(node_t)::var_t
		integer::idx
	contains
		procedure::evalR => evalR_var
		procedure::evalZ => evalZ_var
	end type
	
	interface var_t
		module procedure newVar
	end interface

contains

	!============================!
	!= Evaluation Tree Routines =!
	!============================!

	! real_t
	function newReal(value) result(self)
		real(wp),intent(in)::value
		type(real_t)::self
		
		self%value = value
	end function newReal

	function evalR_real(self,args) result(o)
		class(real_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = self%value
	end function evalR_real

	function evalZ_real(self,args) result(o)
		class(real_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = self%value
	end function evalZ_real

	! imag_t
	function newImag(value) result(self)
		real(wp),intent(in)::value
		type(imag_t)::self
		
		self%value = value
	end function newImag

	function evalR_imag(self,args) result(o)
		class(imag_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		stop 'Imaginary number encountered in real evaluation'
	end function evalR_imag

	function evalZ_imag(self,args) result(o)
		class(imag_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = self%value
	end function evalZ_imag

	! var_t
	function newVar(idx) result(self)
		integer,intent(in)::idx
		type(var_t)::self
		
		self%idx = idx
	end function newVar

	function evalR_var(self,args) result(o)
		class(var_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		integer::N
		
		N = size(args)
		
		if(self%idx>N) then
			write(*,*) 'Invalid argument index: '//intToChar(self%idx)
			stop 'Error in eval_var'
		end if
		
		o = args(self%idx)
	end function evalR_var

	function evalZ_var(self,args) result(o)
		class(var_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		integer::N
		
		N = size(args)
		
		if(self%idx>N) then
			write(*,*) 'Invalid argument index: '//intToChar(self%idx)
			stop 'Error in eval_var'
		end if
		
		o = args(self%idx)
	end function evalZ_var

end module treeValue_mod
