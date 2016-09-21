module eval_mod
	!! Module for dynamic evaluation of function expressions
	!! @todo
	!! Extend to complex numbers
	!! * Two versions of eval, one for R and one for Z
	!! Add ability to take derivative
	use kinds_mod
	use text_mod
	implicit none
	private
	
	!==============================!
	!= node_t Type and Interfaces =!
	!==============================!
	
	type,abstract::node_t
	contains
		procedure(eval_p),deferred::eval
	end type
	
	interface
		function eval_p(self,args) result(o)
			import
			class(node_t),intent(in)::self
			real(wp),dimension(:),intent(in)::args
			real(wp)::o
		end function eval_p
	end interface
	
	!==================================!
	!= function_t Type and Interfaces =!
	!==================================!
	
	type::function_t
		!! Type to store and evaluate parsed expressions
		character(:),allocatable::str
		class(node_t),allocatable::root
	contains
		procedure::eval
	end type
	
	interface function_t
		!! Constructors for function_t
		module procedure newFunction
	end interface
	
	!===================================!
	!= nodeStack_t Type and Interfaces =!
	!===================================!
	
	type::genericNode_t
		class(node_t),allocatable::node
	end type
	
	type::nodeStack_t
		integer,private::N = 0
		integer,private::D = 0
		type(genericNode_t),dimension(:),allocatable::levels
	contains
		procedure::pop
		procedure::push
	end type
	
	interface nodeStack_t
		module procedure newNodeStack
	end interface
	
	!======================!
	!= token_t Parameters =!
	!======================!
	
	character(8),parameter::ops = ',+-*/^()'
	
	integer,parameter::R_SPAN = 99
	
	integer,parameter::T_NONE = -1
	integer,parameter::T_REAL = -2
	integer,parameter::T_IMAG = -3
	integer,parameter::T_VAR  = -4
	
	integer,parameter::T_CMA  = 001
	integer,parameter::T_LPR  = 002
	integer,parameter::T_RPR  = 003
	
	integer,parameter::T_OPERATOR = 200
	integer,parameter::T_ADD  = 201
	integer,parameter::T_SUB  = 202
	integer,parameter::T_MUL  = 203
	integer,parameter::T_DIV  = 204
	integer,parameter::T_POW  = 205
	
	integer,parameter::T_FUNCTION = 100
	integer,parameter::T_NEG   = 101
	integer,parameter::T_SQRT  = 102
	integer,parameter::T_EXP   = 103
	integer,parameter::T_LOG   = 104
	integer,parameter::T_ABS   = 105
	integer,parameter::T_SIN   = 106
	integer,parameter::T_COS   = 107
	integer,parameter::T_TAN   = 108
	integer,parameter::T_ASIN  = 109
	integer,parameter::T_ACOS  = 110
	integer,parameter::T_ATAN  = 111
	integer,parameter::T_LOG10 = 112
	
	!===============================!
	!= token_t Type and Interfaces =!
	!===============================!
	
	type::token_t
		!! Type for a single mathematical token
		integer::t  = T_NONE
			!! Token type
		real(wp)::a = 0.0_wp
			!! Token real value (if any)
		character(8)::s = ''
			!! Token label (if any)
	end type
	
	interface token_t
		!! Constructors for token_t
		module procedure newToken
	end interface
	
	!========================================!
	!= Evaluation Tree Types and Interfaces =!
	!========================================!
	
	! real_t
	type,extends(node_t)::real_t
		real(wp)::value
	contains
		procedure::eval => eval_real
	end type
	
	interface real_t
		module procedure newReal
	end interface
	
	! imag_t
	type,extends(node_t)::imag_t
		real(wp)::value
	contains
		procedure::eval => eval_imag
	end type
	
	interface imag_t
		module procedure newImag
	end interface
	
	! var_t
	type,extends(node_t)::var_t
		integer::idx
	contains
		procedure::eval => eval_var
	end type
	
	interface var_t
		module procedure newVar
	end interface
	
	! add_t
	type,extends(node_t)::add_t
		class(node_t),allocatable::a
		class(node_t),allocatable::b
	contains
		procedure::eval => eval_add
	end type
	
	interface add_t
		module procedure newAdd
	end interface
	
	! sub_t
	type,extends(node_t)::sub_t
		class(node_t),allocatable::a
		class(node_t),allocatable::b
	contains
		procedure::eval => eval_sub
	end type
	
	interface sub_t
		module procedure newSub
	end interface
	
	
	! mul_t
	type,extends(node_t)::mul_t
		class(node_t),allocatable::a
		class(node_t),allocatable::b
	contains
		procedure::eval => eval_mul
	end type
	
	interface mul_t
		module procedure newMul
	end interface
	
	! div_t
	type,extends(node_t)::div_t
		class(node_t),allocatable::a
		class(node_t),allocatable::b
	contains
		procedure::eval => eval_div
	end type
	
	interface div_t
		module procedure newDiv
	end interface
	
	! pow_t
	type,extends(node_t)::pow_t
		class(node_t),allocatable::a
		class(node_t),allocatable::b
	contains
		procedure::eval => eval_pow
	end type
	
	interface pow_t
		module procedure newPow
	end interface
	
	! neg_t
	type,extends(node_t)::neg_t
		class(node_t),allocatable::a
	contains
		procedure::eval => eval_neg
	end type
	
	interface neg_t
		module procedure newNeg
	end interface
	
	! sqrt_t
	type,extends(node_t)::sqrt_t
		class(node_t),allocatable::a
	contains
		procedure::eval => eval_sqrt
	end type
	
	interface sqrt_t
		module procedure newSqrt
	end interface
	
	! exp_t
	type,extends(node_t)::exp_t
		class(node_t),allocatable::a
	contains
		procedure::eval => eval_exp
	end type
	
	interface exp_t
		module procedure newExp
	end interface
	
	! log_t
	type,extends(node_t)::log_t
		class(node_t),allocatable::a
	contains
		procedure::eval => eval_log
	end type
	
	interface log_t
		module procedure newLog
	end interface
	
	! abs_t
	type,extends(node_t)::abs_t
		class(node_t),allocatable::a
	contains
		procedure::eval => eval_abs
	end type
	
	interface abs_t
		module procedure newAbs
	end interface
	
	! sin_t
	type,extends(node_t)::sin_t
		class(node_t),allocatable::a
	contains
		procedure::eval => eval_sin
	end type
	
	interface sin_t
		module procedure newSin
	end interface
	
	! cos_t
	type,extends(node_t)::cos_t
		class(node_t),allocatable::a
	contains
		procedure::eval => eval_cos
	end type
	
	interface cos_t
		module procedure newCos
	end interface
	
	! tan_t
	type,extends(node_t)::tan_t
		class(node_t),allocatable::a
	contains
		procedure::eval => eval_tan
	end type
	
	interface tan_t
		module procedure newTan
	end interface
	
	! asin_t
	type,extends(node_t)::asin_t
		class(node_t),allocatable::a
	contains
		procedure::eval => eval_asin
	end type
	
	interface asin_t
		module procedure newAsin
	end interface
	
	! acos_t
	type,extends(node_t)::acos_t
		class(node_t),allocatable::a
	contains
		procedure::eval => eval_acos
	end type
	
	interface acos_t
		module procedure newAcos
	end interface
	
	! atan_t
	type,extends(node_t)::atan_t
		class(node_t),allocatable::a
	contains
		procedure::eval => eval_atan
	end type
	
	interface atan_t
		module procedure newAtan
	end interface
	
	! log10_t
	type,extends(node_t)::log10_t
		class(node_t),allocatable::a
	contains
		procedure::eval => eval_log10
	end type
	
	interface log10_t
		module procedure newLog10
	end interface
	
	!===========!
	!= Exports =!
	!===========!
	
	public::function_t
	
contains

	!=======================!
	!= function_t Routines =!
	!=======================!

	function newFunction(str) result(self)
		!! Constructor for function_t
		character(*),intent(in)::str
			!! Character to parse into function
		type(function_t)::self
			!! New function_t
		
		type(token_t),dimension(:),allocatable::ar
		type(token_t),dimension(:),allocatable::ex
		integer::ek
		
		self%str = removeSpaces(str)
		
		ek = scan(self%str,'=')
		ar = toRPN(tokenize(self%str(:ek-1)))
		ex = toRPN(tokenize(self%str(ek+1:)))
		
		allocate(self%root,source=toTree( ex , ar%s ))
	end function newFunction

	function eval(self,a) result(o)
		!! Evaluate a function with given arguments
		class(function_t),intent(inout)::self
			!! Function to evaluate
		real(wp),dimension(:),intent(in)::a
			!! Argument values
		real(wp)::o
			!! Resultant value
		
		o = self%root%eval(a)
	end function eval

	!========================!
	!= nodeStack_t Routines =!
	!========================!

	function newNodeStack(N) result(self)
		integer,intent(in)::N
		type(nodeStack_t)::self
		
		self%N = N
		self%D = 0
		allocate(self%levels(N))
	end function newNodeStack

	function pop(self) result(o)
		class(nodeStack_t),intent(inout)::self
		class(node_t),allocatable::o
		integer::k
		
		if(self%D==0) then
			stop 'Tried to pop with empty stack'
		end if
		
		allocate(o,source=self%levels(1)%node)
		do k=1,min(self%D,self%N-1)
			self%levels(k) = self%levels(k+1)
		end do
		
		if(self%N==self%D) then
			if(allocated(self%levels(self%N)%node)) deallocate(self%levels(self%N)%node)
		end if
		
		self%D = self%D-1
	end function pop

	subroutine push(self,a)
		class(nodeStack_t),intent(inout)::self
		class(node_t),intent(in)::a
		integer::k
		
		if(self%D==self%N) then
			stop 'Tried to push a full stack'
		end if
		
		do k=self%D+1,2,-1
			self%levels(k) = self%levels(k-1)
		end do
		if(allocated(self%levels(1)%node)) deallocate(self%levels(1)%node)
		allocate(self%levels(1)%node,source=a)
		
		self%D = self%D+1
	end subroutine push

	!============================!
	!= Evaluation Tree Routines =!
	!============================!

	! real_t
	function newReal(value) result(self)
		real(wp),intent(in)::value
		type(real_t)::self
		
		self%value = value
	end function newReal

	function eval_real(self,args) result(o)
		class(real_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = self%value
	end function eval_real

	! imag_t
	function newImag(value) result(self)
		real(wp),intent(in)::value
		type(imag_t)::self
		
		self%value = value
	end function newImag

	function eval_imag(self,args) result(o)
		class(imag_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		stop 'Imaginary number encountered in real evaluation'
	end function eval_imag

	! var_t
	function newVar(idx) result(self)
		integer,intent(in)::idx
		type(var_t)::self
		
		self%idx = idx
	end function newVar

	function eval_var(self,args) result(o)
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
	end function eval_var

	! add_t
	function newAdd(a,b) result(self)
		class(node_t),intent(in)::a
		class(node_t),intent(in)::b
		type(add_t)::self
		
		allocate(self%a,source=a)
		allocate(self%b,source=b)
	end function newAdd

	function eval_add(self,args) result(o)
		class(add_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = self%a%eval(args)+self%b%eval(args)
	end function eval_add


	! sub_t
	function newSub(a,b) result(self)
		class(node_t),intent(in)::a
		class(node_t),intent(in)::b
		type(sub_t)::self
		
		allocate(self%a,source=a)
		allocate(self%b,source=b)
	end function newSub

	function eval_sub(self,args) result(o)
		class(sub_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = self%a%eval(args)-self%b%eval(args)
	end function eval_sub

	! mul_t
	function newMul(a,b) result(self)
		class(node_t),intent(in)::a
		class(node_t),intent(in)::b
		type(mul_t)::self
		
		allocate(self%a,source=a)
		allocate(self%b,source=b)
	end function newMul

	function eval_mul(self,args) result(o)
		class(mul_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = self%a%eval(args)*self%b%eval(args)
	end function eval_mul

	! div_t
	function newDiv(a,b) result(self)
		class(node_t),intent(in)::a
		class(node_t),intent(in)::b
		type(div_t)::self
		
		allocate(self%a,source=a)
		allocate(self%b,source=b)
	end function newDiv

	function eval_div(self,args) result(o)
		class(div_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = self%a%eval(args)/self%b%eval(args)
	end function eval_div

	! pow_t
	function newPow(a,b) result(self)
		class(node_t),intent(in)::a
		class(node_t),intent(in)::b
		type(pow_t)::self
		
		allocate(self%a,source=a)
		allocate(self%b,source=b)
	end function newPow

	function eval_pow(self,args) result(o)
		class(pow_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = self%a%eval(args)**self%b%eval(args)
	end function eval_pow

	! neg_t
	function newNeg(a) result(self)
		class(node_t),intent(in)::a
		type(neg_t)::self
		
		allocate(self%a,source=a)
	end function newNeg

	function eval_neg(self,args) result(o)
		class(neg_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = -self%a%eval(args)
	end function eval_neg

	! sqrt_t
	function newSqrt(a) result(self)
		class(node_t),intent(in)::a
		type(sqrt_t)::self
		
		allocate(self%a,source=a)
	end function newSqrt

	function eval_sqrt(self,args) result(o)
		class(sqrt_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = sqrt( self%a%eval(args) )
	end function eval_sqrt

	! exp_t
	function newExp(a) result(self)
		class(node_t),intent(in)::a
		type(exp_t)::self
		
		allocate(self%a,source=a)
	end function newExp

	function eval_exp(self,args) result(o)
		class(exp_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = exp( self%a%eval(args) )
	end function eval_exp

	! log_t
	function newLog(a) result(self)
		class(node_t),intent(in)::a
		type(log_t)::self
		
		allocate(self%a,source=a)
	end function newLog

	function eval_log(self,args) result(o)
		class(log_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = log( self%a%eval(args) )
	end function eval_log

	! abs_t
	function newAbs(a) result(self)
		class(node_t),intent(in)::a
		type(abs_t)::self
		
		allocate(self%a,source=a)
	end function newAbs

	function eval_abs(self,args) result(o)
		class(abs_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = abs( self%a%eval(args) )
	end function eval_abs

	! sin_t
	function newSin(a) result(self)
		class(node_t),intent(in)::a
		type(sin_t)::self
		
		allocate(self%a,source=a)
	end function newSin

	function eval_sin(self,args) result(o)
		class(sin_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = sin( self%a%eval(args) )
	end function eval_sin

	! cos_t
	function newCos(a) result(self)
		class(node_t),intent(in)::a
		type(cos_t)::self
		
		allocate(self%a,source=a)
	end function newCos

	function eval_cos(self,args) result(o)
		class(cos_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = cos( self%a%eval(args) )
	end function eval_cos

	! tan_t
	function newTan(a) result(self)
		class(node_t),intent(in)::a
		type(tan_t)::self
		
		allocate(self%a,source=a)
	end function newTan

	function eval_tan(self,args) result(o)
		class(tan_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = tan( self%a%eval(args) )
	end function eval_tan

	! asin_t
	function newAsin(a) result(self)
		class(node_t),intent(in)::a
		type(asin_t)::self
		
		allocate(self%a,source=a)
	end function newAsin

	function eval_asin(self,args) result(o)
		class(asin_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = asin( self%a%eval(args) )
	end function eval_asin

	! acos_t
	function newAcos(a) result(self)
		class(node_t),intent(in)::a
		type(acos_t)::self
		
		allocate(self%a,source=a)
	end function newAcos

	function eval_acos(self,args) result(o)
		class(acos_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = acos( self%a%eval(args) )
	end function eval_acos

	! atan_t
	function newAtan(a) result(self)
		class(node_t),intent(in)::a
		type(atan_t)::self
		
		allocate(self%a,source=a)
	end function newAtan

	function eval_atan(self,args) result(o)
		class(atan_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = atan( self%a%eval(args) )
	end function eval_atan

	! log10_t
	function newLog10(a) result(self)
		class(node_t),intent(in)::a
		type(log10_t)::self
		
		allocate(self%a,source=a)
	end function newLog10

	function eval_log10(self,args) result(o)
		class(log10_t),intent(in)::self
		real(wp),dimension(:),intent(in)::args
		real(wp)::o
		
		o = log10( self%a%eval(args) )
	end function eval_log10

	!====================!
	!= token_t Routines =!
	!====================!

	function newToken(str) result(self)
		!! Constructor for token_t
		character(*),intent(in)::str
			!! String to parse
		type(token_t)::self
			!! New token_t
		
		character(:),allocatable::buf
		
		self%s = str
		if(verify(str,ops)==0) then
			select case(str)
			case(',')
				self%t = T_CMA
			case('(')
				self%t = T_LPR
			case(')')
				self%t = T_RPR
			case('+')
				self%t = T_ADD
			case('-')
				self%t = T_SUB
			case('*')
				self%t = T_MUL
			case('/')
				self%t = T_DIV
			case('^')
				self%t = T_POW
			end select
		else if(verify(str,' .+-0123456789E')==0) then
			self%t = T_REAL
			read(str,*) self%a
		else if(verify(str,' .+-0123456789EJj')==0) then
			self%t = T_IMAG
			buf = removeJ(str)
			read(buf,*) self%a
		else if(str=='sqrt') then
			self%t = T_SQRT
		else if(str=='exp') then
			self%t = T_EXP
		else if(str=='log') then
			self%t = T_LOG
		else if(str=='abs') then
			self%t = T_ABS
		else if(str=='sin') then
			self%t = T_SIN
		else if(str=='cos') then
			self%t = T_COS
		else if(str=='tan') then
			self%t = T_TAN
		else if(str=='asin') then
			self%t = T_ASIN
		else if(str=='acos') then
			self%t = T_ACOS
		else if(str=='atan') then
			self%t = T_ATAN
		else if(str=='log10') then
			self%t = T_LOG10
		else
			self%t = T_VAR
		end if
	
	contains
	
		function removeJ(str) result(o)
			character(*),intent(in)::str
			character(:),allocatable::o
			
			character(1),dimension(:),allocatable::a,b
			
			a = charToArray(str)
			b = pack(a, a/='j' .and. a/='J' )
			o = arrayToChar(b)
		end function removeJ
	
	end function newToken

	!===================!
	!= Helper Routines =!
	!===================!

	function toRPN(tks) result(o)
		!! Convert a list of tokens from read order into RPN
		!!
		!! Uses the shunting-yard algorithm
		type(token_t),dimension(:),intent(in)::tks
			!! Input tokens in read order
		type(token_t),dimension(:),allocatable::o
			!! Output list in RPN
		
		type(token_t),dimension(:),allocatable::s
		integer::ok,sk,k
		
		allocate(o(size(tks)))
		allocate(s(size(tks)))
		ok = 0
		sk = 0
		
		do k=1,size(tks)
			select case(tks(k)%t)
			case(T_REAL,T_IMAG,T_VAR)
				ok = ok+1
				o(ok) = tks(k)
			case(T_FUNCTION:T_FUNCTION+R_SPAN)
				sk = sk+1
				s(sk) = tks(k)
			case(T_LPR)
				sk = sk+1
				s(sk) = tks(k)
			case(T_RPR)
				do while(s(sk)%t/=T_LPR)
					ok = ok+1
					o(ok) = s(sk)
					sk = sk-1
				end do
				sk = sk-1
				if(sk/=0) then
					if(s(sk)%t>=T_FUNCTION .and. s(sk)%t<T_FUNCTION+R_SPAN) then
						ok = ok+1
						o(ok) = s(sk)
						sk = sk-1
					end if
				end if
			case(T_OPERATOR:T_OPERATOR+R_SPAN)
				if(sk/=0) then
					do while( (s(sk)%t>T_OPERATOR .and. s(sk)%t<T_OPERATOR+R_SPAN) .and. &
							 &    ( (s(sk)%t==T_POW .and. tks(k)%t==T_POW) .or. &
							 &      (tks(k)%t<s(sk)%t) ) )
						ok = ok+1
						o(ok) = s(sk)
						sk = sk-1
						if(sk==0) exit
					end do
				end if
				sk = sk+1
				s(sk) = tks(k)
			end select
		end do
		do while(sk>0)
			ok = ok+1
			o(ok) = s(sk)
			sk = sk-1
		end do
		
		o = pack(o,o%t/=T_NONE)
	end function toRPN

	function toTree(tks,args) result(o)
		!! Convert an RPN list into an evaluation tree
		type(token_t),dimension(:),intent(in)::tks
			!! List of tokens in RPN order
		character(*),dimension(:),intent(in)::args
			!! Names of variables in proper order
		class(node_t),allocatable::o
			!! Evaluation tree
		
		type(nodeStack_t)::stk
		class(node_t),allocatable::l1,l2
		integer::N,M,i,k,idx
		
		N = size(tks)
		M = size(args)
		
		stk = nodeStack_t(N)
		
		do k=1,N
			select case( tks(k)%t )
			case(T_VAR)
				do i=1,M
					if( tks(k)%s/=args(i) ) cycle
					idx = i
				end do
				call stk%push( newVar(idx) )
			case(T_REAL)
				call stk%push( newReal(tks(k)%a) )
			case(T_IMAG)
				call stk%push( newImag(tks(k)%a) )
			case(T_ADD)
				allocate(l1,source=stk%pop())
				allocate(l2,source=stk%pop())
				call stk%push( newAdd(l2,l1) )
			case(T_SUB)
				allocate(l1,source=stk%pop())
				allocate(l2,source=stk%pop())
				call stk%push( newSub(l2,l1) )
			case(T_MUL)
				allocate(l1,source=stk%pop())
				allocate(l2,source=stk%pop())
				call stk%push( newMul(l2,l1) )
			case(T_DIV)
				allocate(l1,source=stk%pop())
				allocate(l2,source=stk%pop())
				call stk%push( newDiv(l2,l1) )
			case(T_POW)
				allocate(l1,source=stk%pop())
				allocate(l2,source=stk%pop())
				call stk%push( newPow(l2,l1) )
			case(T_NEG)
				call stk%push( newNeg( stk%pop() ) )
			case(T_SQRT)
				call stk%push( newSqrt( stk%pop() ) )
			case(T_EXP)
				call stk%push( newExp( stk%pop() ) )
			case(T_LOG)
				call stk%push( newLog( stk%pop() ) )
			case(T_ABS)
				call stk%push( newAbs( stk%pop() ) )
			case(T_SIN)
				call stk%push( newSin( stk%pop() ) )
			case(T_COS)
				call stk%push( newCos( stk%pop() ) )
			case(T_TAN)
				call stk%push( newTan( stk%pop() ) )
			case(T_ASIN)
				call stk%push( newAsin( stk%pop() ) )
			case(T_ACOS)
				call stk%push( newAcos( stk%pop() ) )
			case(T_ATAN)
				call stk%push( newAtan( stk%pop() ) )
			case(T_LOG10)
				call stk%push( newLog10( stk%pop() ) )
			end select
			
			if(allocated(l1)) deallocate(l1)
			if(allocated(l2)) deallocate(l2)
		end do
		
		allocate( o,source=stk%pop() )
	end function toTree

	function tokenize(str) result(o)
		!! Split a character into tokens
		character(*),intent(in)::str
			!! Character to split
		type(token_t),dimension(:),allocatable::o
			!! Resulting list of tokens
		
		character(64)::t
		integer::s,n,k
		
		allocate(o(0))
		s = 1
		
		do while(s<len(str))
			n = scan(str(s:),ops)
			if(n==0) then
				exit
			else if(n/=1) then
				t = str(s:s+n-2)
				s = s+n-1
			else
				t = str(s:s)
				s = s+n
			end if
			o = [o,token_t(trim(t))]
		end do
		t = str(s:)
		o = [o,token_t(trim(t))]
		
		! Correct for unary (-)
		if(o(1)%t==T_SUB) o(1)%t = T_NEG
		do k=2,size(o)
			if(o(k)%t/=T_SUB) cycle
			
			if(o(k-1)%t>T_OPERATOR .and. o(k-1)%t<T_OPERATOR+R_SPAN) o(k)%t = T_NEG
			if(o(k-1)%t==T_LPR) o(k)%t = T_NEG
		end do
		do k=1,size(o)
			if(o(k)%t==T_NEG) o(k)%s = '_'
		end do
		
		! Support functions
		do k=1,size(o)-1
			if(o(k)%t/=T_VAR) cycle
			if(o(k+1)%t==T_LPR) o(k)%t = T_FUNCTION
		end do
	end function tokenize

end module eval_mod
