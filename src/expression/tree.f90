module tree_mod
	use kinds_mod
	use node_mod
	implicit none
	
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
		class(log10_t),intent(in)::self
		complex(wp),dimension(:),intent(in)::args
		complex(wp)::o
		
		stop 'Log of complex argument not supported'
	end function evalZ_log10

end module tree_mod
