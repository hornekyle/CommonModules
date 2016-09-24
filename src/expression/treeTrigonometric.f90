module treeTrigonometric_mod
	use kinds_mod
	use node_mod
	implicit none
	
	!========================================!
	!= Evaluation Tree Types and Interfaces =!
	!========================================!
	
	! sin_t
	type,extends(node_t)::sin_t
		class(node_t),allocatable::a
	contains
		procedure::evalR => evalR_sin
		procedure::evalZ => evalZ_sin
	end type
	
	interface sin_t
		module procedure newSin
	end interface
	
	! cos_t
	type,extends(node_t)::cos_t
		class(node_t),allocatable::a
	contains
		procedure::evalR => evalR_cos
		procedure::evalZ => evalZ_cos
	end type
	
	interface cos_t
		module procedure newCos
	end interface
	
	! tan_t
	type,extends(node_t)::tan_t
		class(node_t),allocatable::a
	contains
		procedure::evalR => evalR_tan
		procedure::evalZ => evalZ_tan
	end type
	
	interface tan_t
		module procedure newTan
	end interface
	
	! asin_t
	type,extends(node_t)::asin_t
		class(node_t),allocatable::a
	contains
		procedure::evalR => evalR_asin
		procedure::evalZ => evalZ_asin
	end type
	
	interface asin_t
		module procedure newAsin
	end interface
	
	! acos_t
	type,extends(node_t)::acos_t
		class(node_t),allocatable::a
	contains
		procedure::evalR => evalR_acos
		procedure::evalZ => evalZ_acos
	end type
	
	interface acos_t
		module procedure newAcos
	end interface
	
	! atan_t
	type,extends(node_t)::atan_t
		class(node_t),allocatable::a
	contains
		procedure::evalR => evalR_atan
		procedure::evalZ => evalZ_atan
	end type
	
	interface atan_t
		module procedure newAtan
	end interface
	
contains

	!============================!
	!= Evaluation Tree Routines =!
	!============================!

	! sin_t
	function newSin(a) result(self)
		class(node_t),intent(in)::a
		type(sin_t)::self
		
		allocate(self%a,source=a)
	end function newSin

	function evalR_sin(self,args) result(o)
		class(sin_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = sin( self%a%eval(args) )
	end function evalR_sin

	function evalZ_sin(self,args) result(o)
		class(sin_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = sin( self%a%eval(args) )
	end function evalZ_sin

	! cos_t
	function newCos(a) result(self)
		class(node_t),intent(in)::a
		type(cos_t)::self
		
		allocate(self%a,source=a)
	end function newCos

	function evalR_cos(self,args) result(o)
		class(cos_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = cos( self%a%eval(args) )
	end function evalR_cos

	function evalZ_cos(self,args) result(o)
		class(cos_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = cos( self%a%eval(args) )
	end function evalZ_cos

	! tan_t
	function newTan(a) result(self)
		class(node_t),intent(in)::a
		type(tan_t)::self
		
		allocate(self%a,source=a)
	end function newTan

	function evalR_tan(self,args) result(o)
		class(tan_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = tan( self%a%eval(args) )
	end function evalR_tan

	function evalZ_tan(self,args) result(o)
		class(tan_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = tan( self%a%eval(args) )
	end function evalZ_tan

	! asin_t
	function newAsin(a) result(self)
		class(node_t),intent(in)::a
		type(asin_t)::self
		
		allocate(self%a,source=a)
	end function newAsin

	function evalR_asin(self,args) result(o)
		class(asin_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = asin( self%a%eval(args) )
	end function evalR_asin

	function evalZ_asin(self,args) result(o)
		class(asin_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = asin( self%a%eval(args) )
	end function evalZ_asin

	! acos_t
	function newAcos(a) result(self)
		class(node_t),intent(in)::a
		type(acos_t)::self
		
		allocate(self%a,source=a)
	end function newAcos

	function evalR_acos(self,args) result(o)
		class(acos_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = acos( self%a%eval(args) )
	end function evalR_acos

	function evalZ_acos(self,args) result(o)
		class(acos_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = acos( self%a%eval(args) )
	end function evalZ_acos

	! atan_t
	function newAtan(a) result(self)
		class(node_t),intent(in)::a
		type(atan_t)::self
		
		allocate(self%a,source=a)
	end function newAtan

	function evalR_atan(self,args) result(o)
		class(atan_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = atan( self%a%eval(args) )
	end function evalR_atan

	function evalZ_atan(self,args) result(o)
		class(atan_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = atan( self%a%eval(args) )
	end function evalZ_atan

end module treeTrigonometric_mod
