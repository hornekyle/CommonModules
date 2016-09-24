module iterate_mod
	!! Module for creating and using iteration arrays
	use kinds_mod
	implicit none
	private
	
	type::iterator_t
		integer,dimension(:),allocatable::idx
			!! Current iteration index
		integer,dimension(:),allocatable::N
			!! Iteration limits
		real(wp),dimension(:),allocatable::lB
			!! Lower bounds for real conversion
		real(wp),dimension(:),allocatable::hB
			!! Upper bounds for real conversion
		logical::isDone = .false.
			!! Completion state flag
	contains
		procedure::next
		procedure::getReals
		procedure::getInt
		procedure::setInt
		procedure::getProgress
	end type
	
	public::iterator_t
	public::newIterator
	
contains

	function newIterator(N,lB,hB) result(o)
		!! Create a new iterator_t object
		integer,dimension(:),intent(in)::N
			!! Number of steps in each dimension
		real(wp),dimension(:),intent(in)::lB
			!! Lower bounds in each dimension for real conversion
		real(wp),dimension(:),intent(in)::hB
			!! Upper bounds in each dimension for real conversion
		type(iterator_t)::o
			!! New iterator object
		
		integer::k
		
		o%idx = [( 0 , k=1,size(N) )]
		o%N   = N-1
		o%lB  = lB
		o%hB  = hB
		
		o%isDone = .false.
	end function newIterator

	subroutine next(self)
		!! Increment the iterator's index
		class(iterator_t),intent(inout)::self
			!! Self object
		
		integer::N,k
		
		N = size(self%idx)
		
		self%idx(1) = self%idx(1)+1
		do k=1,N-1
			if( self%idx(k)/=self%N(k) ) cycle
			self%idx(k) = 0
			self%idx(k+1) = self%idx(k+1)+1
		end do
		self%isDone = (self%idx(N)==self%N(N))
	end subroutine next

	function getReals(self) result(r)
		!! Convert the index into the real ranges defined by lB and hB
		class(iterator_t),intent(in)::self
			!! Self object
		real(wp),dimension(:),allocatable::r
			!! Real values in B-space
		
		integer::N,k
		
		N = size(self%idx)
		r = [( 0.0_wp , k=1,N )]
		
		where(self%N/=1) 
			r = real(self%idx,wp)/real(self%N-1,wp)*(self%hB-self%lB)+self%lB
		else where
			r = self%lB
		end where
	end function getReals

	function getInt(self) result(i)
		!! Convert the index array into a single count comparable to product(N)
		class(iterator_t),intent(in)::self
			!! Self object
		integer::i
			!! Single index equivalent
		
		integer::N,k
		
		N = size(self%idx)
		
		i = self%idx(1)
		do k=2,N
			i = i+self%idx(k)*product(self%N(1:k-1))
		end do
	end function getInt

	subroutine setInt(self,i)
		!! Set the index array to the equivalent of a single value comparable to product(N)
		class(iterator_t),intent(inout)::self
			!! Self object
		integer,intent(in)::i
			!! Single index equivalent
		
		integer::N,c,k
		
		N = size(self%idx)
		c = i
		
		self%idx(1) = mod(c,self%N(1))
		c = c-self%idx(1)
		do k=2,N
			self%idx(k) = mod(c,product(self%N(1:k)))/product(self%N(1:k-1))
			c = c-self%idx(k)
		end do
	end subroutine setInt

	function getProgress(self) result(p)
		!! Get the progress of the iterator such that \(p\in[0,1]\)
		class(iterator_t),intent(in)::self
			!! Self object
		real(wp)::p
			!! Progress
		
		p = real(self%getInt(),wp)/real(product(self%N)-1,wp)
	end function getProgress

end module iterate_mod
