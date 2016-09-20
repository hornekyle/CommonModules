module eval_mod
	!! Module for dynamic evaluation of function expressions
	!! @todo
	!! Extend to complex numbers
	!! * Two versions of eval, one for R and one for Z
	!! Change s in token_t to allocatable
	!! Convert evaluation mode to object tree
	!! Add ability to take derivative
	use kinds_mod
	use text_mod
	implicit none
	private
	
	!==============!
	!= Parameters =!
	!==============!
	
	character(8),parameter::ops = ',+-*/^()'
	
	integer,parameter::R_SPAN = 99
	
	integer,parameter::T_NONE = -1
	integer,parameter::T_REAL = -2
	integer,parameter::T_VAR  = -3
	
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
	
	!==================================!
	!= function_t Type and Interfaces =!
	!==================================!
	
	type::function_t
		!! Type to store and evaluate parsed expressions
		type(token_t),dimension(:),allocatable::ar
			!! Tokens of function arguments
		type(token_t),dimension(:),allocatable::ex
			!! Tokens of function expression
	contains
		procedure::eval
	end type
	
	interface function_t
		!! Constructors for function_t
		module procedure newFunction
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
		
		character(:),allocatable::buf
		integer::ek
		
		buf = removeSpaces(str)
		
		ek = scan(buf,'=')
		
		self%ar = toRPN(tokenize(buf(:ek-1)))
		self%ex = toRPN(tokenize(buf(ek+1:)))
	end function newFunction

	function eval(self,a) result(o)
		!! Evaluate a function with given arguments
		class(function_t),intent(inout)::self
			!! Function to evaluate
		real(wp),dimension(:),intent(in)::a
			!! Argument values
		real(wp)::o
			!! Resultant value
		
		real(wp),dimension(:),allocatable::stk
		integer::ek,sk
		
		allocate(stk(size(self%ex)))
		stk = 0.0_wp
		sk = 0
		do ek=1,size(self%ex)
			select case(self%ex(ek)%t)
			case(T_VAR)
				sk = sk+1
				stk(sk) = sum(pack(a,self%ex(ek)%s==self%ar(:size(self%ar)-1)%s))
			case(T_REAL)
				sk = sk+1
				stk(sk) = self%ex(ek)%a
			case(T_ADD)
				stk(sk-1) = stk(sk-1)+stk(sk)
				stk(sk)   = 0.0_wp
				sk = sk-1
			case(T_SUB)
				stk(sk-1) = stk(sk-1)-stk(sk)
				stk(sk)   = 0.0_wp
				sk = sk-1
			case(T_MUL)
				stk(sk-1) = stk(sk-1)*stk(sk)
				stk(sk)   = 0.0_wp
				sk = sk-1
			case(T_DIV)
				stk(sk-1) = stk(sk-1)/stk(sk)
				stk(sk)   = 0.0_wp
				sk = sk-1
			case(T_POW)
				stk(sk-1) = stk(sk-1)**stk(sk)
				stk(sk)   = 0.0_wp
				sk = sk-1
			case(T_NEG)
				stk(sk) = -stk(sk)
			case(T_SQRT)
				stk(sk) = sqrt(stk(sk))
			case(T_EXP)
				stk(sk) = exp(stk(sk))
			case(T_LOG)
				stk(sk) = log(stk(sk))
			case(T_ABS)
				stk(sk) = abs(stk(sk))
			case(T_SIN)
				stk(sk) = sin(stk(sk))
			case(T_COS)
				stk(sk) = cos(stk(sk))
			case(T_TAN)
				stk(sk) = tan(stk(sk))
			case(T_ASIN)
				stk(sk) = asin(stk(sk))
			case(T_ACOS)
				stk(sk) = acos(stk(sk))
			case(T_ATAN)
				stk(sk) = atan(stk(sk))
			case(T_LOG10)
				stk(sk) = log10(stk(sk))
			end select
		end do
		
		o = stk(sk)
	end function eval

	!====================!
	!= token_t Routines =!
	!====================!

	function newToken(str) result(self)
		!! Constructor for token_t
		character(*),intent(in)::str
			!! String to parse
		type(token_t)::self
			!! New token_t
		
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
			case(T_REAL,T_VAR)
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
