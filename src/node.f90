module node_mod
	use kinds_mod
	use text_mod
	implicit none
	
	!==============================!
	!= node_t Type and Interfaces =!
	!==============================!
	
	type,abstract::node_t
	contains
		procedure(evalR_p),deferred,private::evalR
		procedure(evalZ_p),deferred,private::evalZ
		generic::eval => evalR,evalZ
	end type
	
	interface
		function evalR_p(self,args) result(o)
			import
			class(node_t),intent(in)::self
			real(wp),dimension(:),intent(in)::args
			real(wp)::o
		end function evalR_p
		
		function evalZ_p(self,args) result(o)
			import
			class(node_t),intent(in)::self
			complex(wp),dimension(:),intent(in)::args
			complex(wp)::o
		end function evalZ_p
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
	
contains


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

end module node_mod
