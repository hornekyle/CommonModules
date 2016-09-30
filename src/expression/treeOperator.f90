module treeOperator_mod
	use node_mod
	implicit none
	public
	
	!========================================!
	!= Evaluation Tree Types and Interfaces =!
	!========================================!
	
	! add_t
	type,extends(node_t)::add_t
		class(node_t),allocatable::a
		class(node_t),allocatable::b
	contains
		procedure::evalR => evalR_add
		procedure::evalZ => evalZ_add
	end type
	
	interface add_t
		module procedure newAdd
	end interface
	
	! sub_t
	type,extends(node_t)::sub_t
		class(node_t),allocatable::a
		class(node_t),allocatable::b
	contains
		procedure::evalR => evalR_sub
		procedure::evalZ => evalZ_sub
	end type
	
	interface sub_t
		module procedure newSub
	end interface
	
	! mul_t
	type,extends(node_t)::mul_t
		class(node_t),allocatable::a
		class(node_t),allocatable::b
	contains
		procedure::evalR => evalR_mul
		procedure::evalZ => evalZ_mul
	end type
	
	interface mul_t
		module procedure newMul
	end interface
	
	! div_t
	type,extends(node_t)::div_t
		class(node_t),allocatable::a
		class(node_t),allocatable::b
	contains
		procedure::evalR => evalR_div
		procedure::evalZ => evalZ_div
	end type
	
	interface div_t
		module procedure newDiv
	end interface
	
	! pow_t
	type,extends(node_t)::pow_t
		class(node_t),allocatable::a
		class(node_t),allocatable::b
	contains
		procedure::evalR => evalR_pow
		procedure::evalZ => evalZ_pow
	end type
	
	interface pow_t
		module procedure newPow
	end interface
	
	! neg_t
	type,extends(node_t)::neg_t
		class(node_t),allocatable::a
	contains
		procedure::evalR => evalR_neg
		procedure::evalZ => evalZ_neg
	end type
	
	interface neg_t
		module procedure newNeg
	end interface
	
	! sqrt_t
	type,extends(node_t)::sqrt_t
		class(node_t),allocatable::a
	contains
		procedure::evalR => evalR_sqrt
		procedure::evalZ => evalZ_sqrt
	end type
	
	interface sqrt_t
		module procedure newSqrt
	end interface
	
	! abs_t
	type,extends(node_t)::abs_t
		class(node_t),allocatable::a
	contains
		procedure::evalR => evalR_abs
		procedure::evalZ => evalZ_abs
	end type
	
	interface abs_t
		module procedure newAbs
	end interface
	
contains

	!============================!
	!= Evaluation Tree Routines =!
	!============================!

	! add_t
	function newAdd(a,b) result(self)
		class(node_t),intent(in)::a
		class(node_t),intent(in)::b
		type(add_t)::self
		
		allocate(self%a,source=a)
		allocate(self%b,source=b)
	end function newAdd

	function evalR_add(self,args) result(o)
		class(add_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = self%a%eval(args)+self%b%eval(args)
	end function evalR_add

	function evalZ_add(self,args) result(o)
		class(add_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = self%a%eval(args)+self%b%eval(args)
	end function evalZ_add

	! sub_t
	function newSub(a,b) result(self)
		class(node_t),intent(in)::a
		class(node_t),intent(in)::b
		type(sub_t)::self
		
		allocate(self%a,source=a)
		allocate(self%b,source=b)
	end function newSub

	function evalR_sub(self,args) result(o)
		class(sub_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = self%a%eval(args)-self%b%eval(args)
	end function evalR_sub

	function evalZ_sub(self,args) result(o)
		class(sub_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = self%a%eval(args)-self%b%eval(args)
	end function evalZ_sub

	! mul_t
	function newMul(a,b) result(self)
		class(node_t),intent(in)::a
		class(node_t),intent(in)::b
		type(mul_t)::self
		
		allocate(self%a,source=a)
		allocate(self%b,source=b)
	end function newMul

	function evalR_mul(self,args) result(o)
		class(mul_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = self%a%eval(args)*self%b%eval(args)
	end function evalR_mul

	function evalZ_mul(self,args) result(o)
		class(mul_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = self%a%eval(args)*self%b%eval(args)
	end function evalZ_mul

	! div_t
	function newDiv(a,b) result(self)
		class(node_t),intent(in)::a
		class(node_t),intent(in)::b
		type(div_t)::self
		
		allocate(self%a,source=a)
		allocate(self%b,source=b)
	end function newDiv

	function evalR_div(self,args) result(o)
		class(div_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = self%a%eval(args)/self%b%eval(args)
	end function evalR_div

	function evalZ_div(self,args) result(o)
		class(div_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = self%a%eval(args)/self%b%eval(args)
	end function evalZ_div

	! pow_t
	function newPow(a,b) result(self)
		class(node_t),intent(in)::a
		class(node_t),intent(in)::b
		type(pow_t)::self
		
		allocate(self%a,source=a)
		allocate(self%b,source=b)
	end function newPow

	function evalR_pow(self,args) result(o)
		class(pow_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = self%a%eval(args)**self%b%eval(args)
	end function evalR_pow

	function evalZ_pow(self,args) result(o)
		class(pow_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = self%a%eval(args)**self%b%eval(args)
	end function evalZ_pow

	! neg_t
	function newNeg(a) result(self)
		class(node_t),intent(in)::a
		type(neg_t)::self
		
		allocate(self%a,source=a)
	end function newNeg

	function evalR_neg(self,args) result(o)
		class(neg_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = -self%a%eval(args)
	end function evalR_neg

	function evalZ_neg(self,args) result(o)
		class(neg_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = -self%a%eval(args)
	end function evalZ_neg

	! sqrt_t
	function newSqrt(a) result(self)
		class(node_t),intent(in)::a
		type(sqrt_t)::self
		
		allocate(self%a,source=a)
	end function newSqrt

	function evalR_sqrt(self,args) result(o)
		class(sqrt_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = sqrt( self%a%eval(args) )
	end function evalR_sqrt

	function evalZ_sqrt(self,args) result(o)
		class(sqrt_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = sqrt( self%a%eval(args) )
	end function evalZ_sqrt

	! abs_t
	function newAbs(a) result(self)
		class(node_t),intent(in)::a
		type(abs_t)::self
		
		allocate(self%a,source=a)
	end function newAbs

	function evalR_abs(self,args) result(o)
		class(abs_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = abs( self%a%eval(args) )
	end function evalR_abs

	function evalZ_abs(self,args) result(o)
		class(abs_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		o = abs( self%a%eval(args) )
	end function evalZ_abs

end module treeOperator_mod
