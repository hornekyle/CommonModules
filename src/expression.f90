module expression_mod
	!! Module for dynamic evaluation of function expressions
	!! @todo
	!! Add ability to take derivative
	use kinds_mod
	use node_mod
	use treeValue_mod
	use treeOperator_mod
	use treeExponential_mod
	use treeTrigonometric_mod
	implicit none
	private
	
	!==================================!
	!= function_t Type and Interfaces =!
	!==================================!
	
	type::function_t
		!! Type to store and evaluate parsed expressions
		character(:),allocatable::str
		class(node_t),allocatable::root
	contains
		procedure,private::evalR
		procedure,private::evalZ
		generic::eval => evalR, evalZ
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
		
		type(token_t),dimension(:),allocatable::ar
		type(token_t),dimension(:),allocatable::ex
		integer::ek
		
		self%str = removeSpaces(str)
		
		ek = scan(self%str,'=')
		ar = toRPN(tokenize(self%str(:ek-1)))
		ex = toRPN(tokenize(self%str(ek+1:)))
		
		allocate(self%root,source=toTree( ex , ar%s ))
	end function newFunction

	function evalR(self,a) result(o)
		!! Evaluate a function with given arguments
		class(function_t),intent(inout)::self
			!! Function to evaluate
		real(wp),dimension(:),intent(in)::a
			!! Argument values
		real(wp)::o
			!! Resultant value
		
		o = self%root%eval(a)
	end function evalR

	function evalZ(self,a) result(o)
		!! Evaluate a function with given arguments
		class(function_t),intent(inout)::self
			!! Function to evaluate
		complex(wp),dimension(:),intent(in)::a
			!! Argument values
		complex(wp)::o
			!! Resultant value
		
		o = self%root%eval(a)
	end function evalZ

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

end module expression_mod
