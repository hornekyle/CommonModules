module sparseZ_mod
	use kinds_mod
	use array_mod
	implicit none
	private
	
	!=========!
	!= Types =!
	!=========!
	
	type::spvecZ_t
		integer,dimension(:),allocatable::i
		complex(wp),dimension(:),allocatable::v
	contains
		procedure::get => get_v
		procedure::set => set_v
		procedure::add => add_v
		procedure::toDense => toDense_v
	end type
	
	type::sparseZ_t
		integer::N
		integer::M
		type(spvecZ_t),dimension(:),allocatable::rows
	contains
		procedure::get => get_m
		procedure::set => set_m
		procedure::add => add_m
		procedure::expand
		procedure::getDiagonal
		procedure::toDense => toDense_m
		procedure::toText => toText_m
	end type
	
	!==============!
	!= Interfaces =!
	!==============!
	
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
		
		module procedure dot_mm
	end interface
	
	interface matmul
		module procedure dot_mu
		module procedure dot_mv
		module procedure dot_mm
	end interface
	
	interface sum
		module procedure sum_v
	end interface
	
	interface transpose
		module procedure transpose_m
	end interface
	
	!===========!
	!= Exports =!
	!===========!
	
	public::spvecZ_t
	public::sparseZ_t
	
	public::newSpvecZ
	public::newSparseZ
	
	public::operator(+)
	public::operator(*)
	public::operator(.o.)
	public::matmul
	public::sum
	public::transpose
	
	! Kinds
	public::wp
	
contains

	!===========!
	!= Utility =!
	!===========!

	function newSpvecZ() result(o)
		type(spvecZ_t)::o
		
		allocate(o%i(0))
		allocate(o%v(0))
	end function newSpvecZ

	function newSparseZ(N,M) result(o)
		integer,intent(in)::N
		integer,intent(in)::M
		type(sparseZ_t)::o
		
		integer::k
		
		o%N = N
		o%M = M
		allocate(o%rows(N))
		
		do k=1,N
			o%rows(k) = newSpvecZ()
		end do
	end function newSparseZ

	!=======================!
	!= Methods for spvec_t =!
	!=======================!

	function get_v(self,i) result(o)
		class(spvecZ_t),intent(in)::self
		integer,intent(in)::i
		complex(wp)::o
		
		o = sum(self%v,self%i==i)
	end function get_v

	subroutine set_v(self,i,v)
		class(spvecZ_t),intent(inout)::self
		integer,intent(in)::i
		complex(wp),intent(in)::v
		
		logical::doAppend
		integer::k
		
		doAppend = .true.
		
		do k=1,size(self%i)
			if(self%i(k)/=i) cycle
			self%v(k) = v
			doAppend = .false.
		end do
		
		if(doAppend) then
			self%i = [self%i,i]
			self%v = [self%v,v]
		end if
	end subroutine set_v

	subroutine add_v(self,i,v)
		class(spvecZ_t),intent(inout)::self
		integer,intent(in)::i
		complex(wp),intent(in)::v
		
		logical::doAppend
		integer::k
		
		doAppend = .true.
		
		do k=1,size(self%i)
			if(self%i(k)/=i) cycle
			self%v(k) = self%v(k)+v
			doAppend = .false.
		end do
		
		if(doAppend) then
			self%i = [self%i,i]
			self%v = [self%v,v]
		end if
	end subroutine add_v

	function toDense_v(self,N) result(o)
		class(spvecZ_t),intent(in)::self
		integer,intent(in)::N
		complex(wp),dimension(:),allocatable::o
		
		integer::i,k
		
		allocate(o(N))
		o = 0.0_wp
		do k=1,size(self%i)
			i = self%i(k)
			o(i) = self%v(k)
		end do
	end function toDense_v

	!=========================!
	!= Overloads for spvec_t =!
	!=========================!

	function add_uv(u,v) result(o)
		complex(wp),dimension(:),intent(in)::u
		type(spvecZ_t),intent(in)::v
		complex(wp),dimension(:),allocatable::o
		
		integer::i,k
		
		o = u
		do k=1,size(v%i)
			i = v%i(k)
			o(i) = o(i)+v%v(k)
		end do
	end function add_uv

	function add_vu(v,u) result(o)
		type(spvecZ_t),intent(in)::v
		complex(wp),dimension(:),intent(in)::u
		complex(wp),dimension(:),allocatable::o
		
		integer::i,k
		
		o = u
		do k=1,size(v%i)
			i = v%i(k)
			o(i) = o(i)+v%v(k)
		end do
	end function add_vu

	function add_vv(u,v) result(o)
		type(spvecZ_t),intent(in)::u
		type(spvecZ_t),intent(in)::v
		type(spvecZ_t)::o
		
		integer::i,k
		
		o%i = deDup([u%i,v%i])
		allocate(o%v(size(o%i)))
		
		do k=1,size(o%i)
			i = o%i(k)
			o%v(k) = u%get(i)+v%get(i)
		end do
	end function add_vv

	function mul_rv(r,v) result(o)
		complex(wp),intent(in)::r
		type(spvecZ_t),intent(in)::v
		type(spvecZ_t)::o
		
		o%i = v%i
		o%v = r*v%v
	end function mul_rv

	function mul_vr(v,r) result(o)
		type(spvecZ_t),intent(in)::v
		complex(wp),intent(in)::r
		type(spvecZ_t)::o
		
		o%i = v%i
		o%v = r*v%v
	end function mul_vr

	function dot_uv(u,v) result(o)
		complex(wp),dimension(:),intent(in)::u
		type(spvecZ_t),intent(in)::v
		complex(wp)::o
		
		o = sum(u(v%i)*v%v)
	end function dot_uv

	function dot_vu(v,u) result(o)
		type(spvecZ_t),intent(in)::v
		complex(wp),dimension(:),intent(in)::u
		complex(wp)::o
		
		o = sum(u(v%i)*v%v)
	end function dot_vu

	function dot_vv(u,v) result(o)
		type(spvecZ_t),intent(in)::u
		type(spvecZ_t),intent(in)::v
		complex(wp)::o
		
		integer::i,k
		
		o = 0.0_wp
		do k=1,size(u%i)
			i = u%i(k)
			o = o+u%v(k)*v%get(i)
		end do
	end function dot_vv

	function sum_v(l) result(o)
		type(spvecZ_t),dimension(:),intent(in)::l
		type(spvecZ_t)::o
		
		integer::i,j,k
		complex(wp)::v
		
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
		class(sparseZ_t),intent(in)::self
		integer,intent(in)::i
		integer,intent(in)::j
		complex(wp)::o
		
		o = self%rows(i)%get(j)
	end function get_m

	subroutine set_m(self,i,j,v)
		class(sparseZ_t),intent(inout)::self
		integer,intent(in)::i
		integer,intent(in)::j
		complex(wp),intent(in)::v
		
		call self%rows(i)%set(j,v)
	end subroutine set_m

	subroutine add_m(self,i,j,v)
		class(sparseZ_t),intent(inout)::self
		integer,intent(in)::i
		integer,intent(in)::j
		complex(wp),intent(in)::v
		
		call self%rows(i)%add(j,v)
	end subroutine add_m

	subroutine expand(self,r,c,A)
		class(sparseZ_t),intent(inout)::self
		integer,dimension(:),intent(in)::r,c
		complex(wp),dimension(size(r),size(c)),intent(in)::A
		
		integer::i,j,k,l
		
		do k=1,size(r)
			i = r(k)
			if(i>self%N) cycle
			do l=1,size(c)
				j = c(l)
				if(j>self%M) cycle
				
				call self%add(i,j,A(k,l))
			end do
		end do
	end subroutine expand

	function getDiagonal(self) result(o)
		class(sparseZ_t),intent(in)::self
		complex(wp),dimension(:),allocatable::o
		
		integer::i
		
		allocate(o(self%N))
		do i=1,self%N
			o(i) = self%get(i,i)
		end do
	end function getDiagonal

	function toDense_m(self) result(o)
		class(sparseZ_t),intent(in)::self
		complex(wp),dimension(:,:),allocatable::o
		
		integer::i
		
		allocate(o(self%N,self%M))
		o = 0.0_wp
		do i=1,self%N
			o(i,1:self%M) = self%rows(i)%toDense(self%M)
		end do
	end function toDense_m

	subroutine toText_m(self,iou)
		class(sparseZ_t),intent(in)::self
		integer,intent(in)::iou
		
		integer::i,j,k
		complex(wp)::v
		
		do i=1,self%N
			do k=1,size(self%rows(i)%i)
				j = self%rows(i)%i(k)
				v = self%rows(i)%v(k)
				write(iou,*) i,j,v
			end do
		end do
	end subroutine toText_m

	!==========================!
	!= Overloads for sparse_t =!
	!==========================!

	function dot_um(u,A) result(o)
		complex(wp),dimension(:),intent(in)::u
		type(sparseZ_t),intent(in)::A
		complex(wp),dimension(:),allocatable::o
		
		integer::i
		
		allocate(o(A%M))
		o = 0.0_wp
		do i=1,A%N
			o = o+u(i)*A%rows(i)%toDense(A%M)
		end do
	end function dot_um

	function dot_mu(A,u) result(o)
		type(sparseZ_t),intent(in)::A
		complex(wp),dimension(:),intent(in)::u
		complex(wp),dimension(:),allocatable::o
		
		integer::k
		
		allocate(o(A%N))
		do k=1,size(o)
			o(k) = A%rows(k).o.u
		end do
	end function dot_mu

	function dot_vm(v,A) result(o)
		type(spvecZ_t),intent(in)::v
		type(sparseZ_t),intent(in)::A
		type(spvecZ_t)::o
		
		integer::i,k
		
		o = newSpvecZ()
		
		do k=1,size(v%i)
			i = v%i(k)
			o = o+v%v(i)*A%rows(i)
		end do
	end function dot_vm

	function dot_mv(A,v) result(o)
		type(sparseZ_t),intent(in)::A
		type(spvecZ_t),intent(in)::v
		type(spvecZ_t)::o
		
		real(wp)::tol
		complex(wp)::r
		integer::i
		
		tol = (2.0_wp**5)*epsilon(1.0_wp)
		o = newSpvecZ()
		
		do i=1,A%N
			r = A%rows(i).o.v
			if(abs(r)>tol) call o%set(i,r)
		end do
	end function dot_mv

	function dot_mm(A,B) result(o)
		type(sparseZ_t),intent(in)::A
		type(sparseZ_t),intent(in)::B
		type(sparseZ_t)::o
		
		integer::i
		
		o = newSparseZ(A%N,B%M)
		do i=1,o%N
			o%rows(i) = A%rows(i).o.B
		end do
	end function dot_mm

	function transpose_m(A) result(o)
		type(sparseZ_t),intent(in)::A
		type(sparseZ_t)::o
		
		integer::i,j,k
		complex(wp)::v
		
		o = newSparseZ(A%M,A%N)
		
		do i=1,A%N
			do k=1,size(A%rows(i)%i)
				j = A%rows(i)%i(k)
				v = A%rows(i)%v(k)
				call o%set(j,i,v)
			end do
		end do
	end function transpose_m

end module sparseZ_mod
