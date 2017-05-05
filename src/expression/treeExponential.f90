module treeExponential_mod
	use node_mod
	implicit none
	public
	
	!========================================!
	!= Evaluation Tree Types and Interfaces =!
	!========================================!
	
	! exp_t
	type,extends(node_t)::exp_t
		class(node_t),allocatable::a
	contains
		procedure::evalR => evalR_exp
		procedure::evalZ => evalZ_exp
	end type
	
	interface exp_t
		module procedure newExp
	end interface
	
	! log_t
	type,extends(node_t)::log_t
		class(node_t),allocatable::a
	contains
		procedure::evalR => evalR_log
		procedure::evalZ => evalZ_log
	end type
	
	interface log_t
		module procedure newLog
	end interface
	
	! log10_t
	type,extends(node_t)::log10_t
		class(node_t),allocatable::a
	contains
		procedure::evalR => evalR_log10
		procedure::evalZ => evalZ_log10
	end type
	
	interface log10_t
		module procedure newLog10
	end interface

contains

	!============================!
	!= Evaluation Tree Routines =!
	!============================!

	! exp_t
	function newExp(a) result(self)
		class(node_t),intent(in)::a
		type(exp_t)::self
		
		allocate(self%a,source=a)
	end function newExp

	function evalR_exp(self,args) result(o)
		class(exp_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = exp( self%a%eval(args) )
	end function evalR_exp

	function evalZ_exp(self,args) result(o)
		class(exp_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = exp( self%a%eval(args) )
	end function evalZ_exp

	! log_t
	function newLog(a) result(self)
		class(node_t),intent(in)::a
		type(log_t)::self
		
		allocate(self%a,source=a)
	end function newLog

	function evalR_log(self,args) result(o)
		class(log_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = log( self%a%eval(args) )
	end function evalR_log

	function evalZ_log(self,args) result(o)
		class(log_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = log( self%a%eval(args) )
	end function evalZ_log

	! log10_t
	function newLog10(a) result(self)
		class(node_t),intent(in)::a
		type(log10_t)::self
		
		allocate(self%a,source=a)
	end function newLog10

	function evalR_log10(self,args) result(o)
		class(log10_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = log10( self%a%eval(args) )
	end function evalR_log10

	function evalZ_log10(self,args) result(o)
		use ieee_arithmetic
		class(log10_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = ieee_value(0.0_wp,IEEE_QUIET_NAN)
		stop 'Log of complex argument not supported'
	end function evalZ_log10

end module treeExponential_mod
