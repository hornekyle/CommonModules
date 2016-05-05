module eval_mod
	!! Module for dynamic evaluation of function expressions
	use kinds_mod
	use text_mod
	implicit none
	private
	
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
	
	type::token_t
		integer::t = T_NONE
		real(wp)::a = 0.0_wp
		character(8)::s = ''
	end type
	
	type::function_t
		type(token_t),dimension(:),allocatable::ex,ar
	contains
		procedure::eval
	end type

	public::function_t
	public::newFunction
	
contains

	function newFunction(str) result(o)
		character(*),intent(in)::str
		type(function_t)::o
		
		character(:),allocatable::buf
		integer::ek
		
		buf = removeSpaces(str)
		
		ek = scan(buf,'=')
		
		o%ar = toRPN(tokenize(buf(:ek-1)))
		o%ex = toRPN(tokenize(buf(ek+1:)))
	end function newFunction

	function eval(self,a) result(o)
		class(function_t),intent(inout)::self
		real(wp),dimension(:),intent(in)::a
		real(wp)::o
		
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

	function toRPN(tks) result(o)
		type(token_t),dimension(:),intent(in)::tks
		type(token_t),dimension(:),allocatable::o
		
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
		character(*),intent(in)::str
		type(token_t),dimension(:),allocatable::o
		
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
			o = [o,token(trim(t))]
		end do
		t = str(s:)
		o = [o,token(trim(t))]
		
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

	function token(str) result(o)
		character(*),intent(in)::str
		type(token_t)::o
		
		o%s = str
		if(verify(str,ops)==0) then
			select case(str)
			case(',')
				o%t = T_CMA
			case('(')
				o%t = T_LPR
			case(')')
				o%t = T_RPR
			case('+')
				o%t = T_ADD
			case('-')
				o%t = T_SUB
			case('*')
				o%t = T_MUL
			case('/')
				o%t = T_DIV
			case('^')
				o%t = T_POW
			end select
		else if(verify(str,' .+-0123456789E')==0) then
			o%t = T_REAL
			read(str,*) o%a
		else if(str=='sqrt') then
			o%t = T_SQRT
		else if(str=='exp') then
			o%t = T_EXP
		else if(str=='log') then
			o%t = T_LOG
		else if(str=='abs') then
			o%t = T_ABS
		else if(str=='sin') then
			o%t = T_SIN
		else if(str=='cos') then
			o%t = T_COS
		else if(str=='tan') then
			o%t = T_TAN
		else if(str=='asin') then
			o%t = T_ASIN
		else if(str=='acos') then
			o%t = T_ACOS
		else if(str=='atan') then
			o%t = T_ATAN
		else if(str=='log10') then
			o%t = T_LOG10
		else
			o%t = T_VAR
		end if
	end function token

end module eval_mod
