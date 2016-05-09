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
		procedure::toDense => toDense_v
	end type
	
	type::sparse_t
		integer::N
		integer::M
		type(spvec_t),dimension(:),allocatable::rows
	contains
		procedure::get => get_m
		procedure::set => set_m
		procedure::add => add_m
	end type
	
	interface operator(+)
		module procedure add_uv
		module procedure add_vu
		module procedure add_vv
	end interface
	
	interface operator(*)
		module procedure mul_rv
		module procedure mul_vr
	end interface
	
	interface operator(.o.)
		module procedure dot_uv
		module procedure dot_vu
		module procedure dot_vv
		
		module procedure dot_um
		module procedure dot_mu
		module procedure dot_vm
		module procedure dot_mv
	end interface
	
	interface matmul
		module procedure dot_mu
		module procedure dot_mv
	end interface
	
	interface sum
		module procedure sum_v
	end interface
	
! 	interface minval
! 		module procedure minval_v
! 		module procedure minval_m
! 	end interface
	
! 	interface maxval
! 		module procedure maxval_v
! 		module procedure maxval_m
! 	end interface
	
contains

	!===========!
	!= Utility =!
	!===========!

	function newSpvec() result(o)
		type(spvec_t)::o
		
		allocate(o%i(0))
		allocate(o%v(0))
	end function newSpvec

	function newSparse(N,M) result(o)
		integer,intent(in)::N
		integer,intent(in)::M
		type(sparse_t)::o
		
		integer::k
		
		o%N = N
		o%M = M
		allocate(o%rows(N))
		
		do k=1,N
			o%rows(k) = newSpvec()
		end do
	end function newSparse

	!=======================!
	!= Methods for spvec_t =!
	!=======================!

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

	function toDense_v(self,N) result(o)
		class(spvec_t),intent(in)::self
		integer,intent(in)::N
		real(wp),dimension(:),allocatable::o
		
		integer::i,k
		
		allocate(o(N))
		do k=1,size(self%i)
			i = self%i(k)
			o(i) = self%v(k)
		end do
	end function toDense_v

	!=========================!
	!= Overloads for spvec_t =!
	!=========================!

	function add_uv(u,v) result(o)
		real(wp),dimension(:),intent(in)::u
		type(spvec_t),intent(in)::v
		real(wp),dimension(:),allocatable::o
		
		integer::i,k
		
		o = u
		do k=1,size(v%i)
			i = v%i(k)
			o(i) = o(i)+v%v(k)
		end do
	end function add_uv

	function add_vu(v,u) result(o)
		type(spvec_t),intent(in)::v
		real(wp),dimension(:),intent(in)::u
		real(wp),dimension(:),allocatable::o
		
		integer::i,k
		
		o = u
		do k=1,size(v%i)
			i = v%i(k)
			o(i) = o(i)+v%v(k)
		end do
	end function add_vu

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

	function dot_uv(u,v) result(o)
		real(wp),dimension(:),intent(in)::u
		type(spvec_t),intent(in)::v
		real(wp)::o
		
		o = sum(u(v%i)*v%v)
	end function dot_uv

	function dot_vu(v,u) result(o)
		type(spvec_t),intent(in)::v
		real(wp),dimension(:),intent(in)::u
		real(wp)::o
		
		o = sum(u(v%i)*v%v)
	end function dot_vu

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

	function sum_v(l) result(o)
		type(spvec_t),dimension(:),intent(in)::l
		type(spvec_t)::o
		
		integer::i,j,k
		real(wp)::v
		
		o%i = deDup( [( l(k)%i , k=1,size(l) )] )
		allocate(o%v(size(o%i)))
		
		do k=1,size(l)
			do j=1,size(l(k)%i)
				i = l(k)%i(j)
				v = l(k)%v(j)
				call o%add(i,v)
			end do
		end do
	end function sum_v

	!========================!
	!= Methods for sparse_t =!
	!========================!

	function get_m(self,i,j) result(o)
		class(sparse_t),intent(in)::self
		integer,intent(in)::i
		integer,intent(in)::j
		real(wp)::o
		
		o = self%rows(i)%get(j)
	end function get_m

	subroutine set_m(self,i,j,v)
		class(sparse_t),intent(inout)::self
		integer,intent(in)::i
		integer,intent(in)::j
		real(wp),intent(in)::v
		
		call self%rows(i)%set(j,v)
	end subroutine set_m

	subroutine add_m(self,i,j,v)
		class(sparse_t),intent(inout)::self
		integer,intent(in)::i
		integer,intent(in)::j
		real(wp),intent(in)::v
		
		call self%rows(i)%add(j,v)
	end subroutine add_m

	!==========================!
	!= Overloads for sparse_t =!
	!==========================!

	function dot_um(u,A) result(o)
		real(wp),dimension(:),intent(in)::u
		type(sparse_t),intent(in)::A
		real(wp),dimension(:),allocatable::o
		
		integer::i
		
		allocate(o(A%M))
		o = 0.0_wp
		do i=1,A%N
			o = o+u(i)*A%rows(i)%toDense(A%M)
		end do
	end function dot_um

	function dot_mu(A,u) result(o)
		type(sparse_t),intent(in)::A
		real(wp),dimension(:),intent(in)::u
		real(wp),dimension(:),allocatable::o
		
		integer::k
		
		allocate(o(A%N))
		do k=1,size(o)
			o(k) = A%rows(k).o.u
		end do
	end function dot_mu

	function dot_vm(v,A) result(o)
		type(spvec_t),intent(in)::v
		type(sparse_t),intent(in)::A
		type(spvec_t)::o
		
		integer::i,k
		
		o = newSpvec()
		
		do k=1,size(v%i)
			i = v%i(k)
			o = o+v%v(i)*A%rows(i)
		end do
	end function dot_vm

	function dot_mv(A,v) result(o)
		type(sparse_t),intent(in)::A
		type(spvec_t),intent(in)::v
		type(spvec_t)::o
		
		real(wp)::tol,r
		integer::i
		
		tol = (2.0_wp**5)*epsilon(1.0_wp)
		o = newSpvec()
		
		do i=1,A%N
			r = A%rows(i).o.v
			if(abs(r)>tol) call o%set(i,r)
		end do
	end function dot_mv

	function dot_mm(A,B) result(o)
		type(sparse_t),intent(in)::A
		type(sparse_t),intent(in)::B
		type(sparse_t)::o
		
		integer::i
		
		o = newSparse(A%N,B%M)
		do i=1,o%N
			o%rows(i) = A%rows(i).o.B
		end do
	end function dot_mm

end module sparse_mod