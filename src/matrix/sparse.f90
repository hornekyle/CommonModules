module sparse_mod
	use kinds_mod
	use array_mod
	implicit none
	
	type::spvec_t
		integer,dimension(:),allocatable::i
		real(wp),dimension(:),allocatable::v
	contains
		procedure::get => get_v
		procedure::set => set_v
		procedure::add => add_v
	end type
	
	type::sparse_t
		integer::N
		integer::M
		type(spvec_t),dimension(:),allocatable::rows
	end type
	
	interface operator(+)
		module procedure add_vv
	end interface
	
	interface operator(*)
		module procedure mul_rv
		module procedure mul_vr
	end interface
	
	interface operator(.o.)
		module procedure dot_vv
	end interface
	
contains

	!===========!
	!= Utility =!
	!===========!

	function newSparse(N,M) result(o)
		integer,intent(in)::N
		integer,intent(in)::M
		type(sparse_t)::o
		
		integer::k
		
		o%N = N
		o%M = M
		allocate(o%rows(N))
		
		do k=1,N
			allocate(o%rows(k)%i(0))
			allocate(o%rows(k)%v(0))
		end do
	end function newSparse

	!========================!
	!= Routines for spvec_t =!
	!========================!

	function get_v(self,i) result(o)
		class(spvec_t),intent(in)::self
		integer,intent(in)::i
		real(wp)::o
		
		o = sum(self%v,self%i==i)
	end function get_v

	subroutine set_v(self,i,v)
		class(spvec_t),intent(inout)::self
		integer,intent(in)::i
		real(wp),intent(in)::v
		
		logical::doAppend
		integer::k
		
		doAppend = .false.
		
		do k=1,size(self%i)
			if(self%i(k)/=i) cycle
			self%v(k) = v
		end do
		
		if(doAppend) then
			self%i = [self%i,i]
			self%v = [self%v,v]
		end if
	end subroutine set_v

	subroutine add_v(self,i,v)
		class(spvec_t),intent(inout)::self
		integer,intent(in)::i
		real(wp),intent(in)::v
		
		logical::doAppend
		integer::k
		
		doAppend = .false.
		
		do k=1,size(self%i)
			if(self%i(k)/=i) cycle
			self%v(k) = self%v(k)+v
		end do
		
		if(doAppend) then
			self%i = [self%i,i]
			self%v = [self%v,v]
		end if
	end subroutine add_v

	function add_vv(u,v) result(o)
		type(spvec_t),intent(in)::u
		type(spvec_t),intent(in)::v
		type(spvec_t)::o
		
		integer::i,k
		
		o%i = deDup([u%i,v%i])
		allocate(o%v(size(o%i)))
		
		do k=1,size(o%i)
			i = o%i(k)
			o%v(k) = u%get(i)+v%get(i)
		end do
	end function add_vv

	function mul_rv(r,v) result(o)
		real(wp),intent(in)::r
		type(spvec_t),intent(in)::v
		type(spvec_t)::o
		
		o%i = v%i
		o%v = r*v%v
	end function mul_rv

	function mul_vr(v,r) result(o)
		type(spvec_t),intent(in)::v
		real(wp),intent(in)::r
		type(spvec_t)::o
		
		o%i = v%i
		o%v = r*v%v
	end function mul_vr

	function dot_vv(u,v) result(o)
		type(spvec_t),intent(in)::u
		type(spvec_t),intent(in)::v
		real(wp)::o
		
		integer::i,k
		
		o = 0.0_wp
		do k=1,size(u%i)
			i = u%i(k)
			o = o+u%v(k)*v%get(i)
		end do
	end function dot_vv

end module sparse_mod