module autoDiff_mod
	!! Module implementing automatic differentiation
	!! @todo
	!! Split into managable sizes
	!! Full documentation must be written  
	!! Truly rigourous testing of this module is needed  
	!! Less-commonly used routines need to be implemented  
	!! A full implementation of matmul is needed
	!! Loop bounds in matmul need work
	!! Extend to complex numbers
	use kinds_mod
	implicit none
	
	private
	
	!==============!
	!= Parameters =!
	!==============!
	
	integer,parameter::Sl = 10
	integer,parameter::Sf = 2
	integer,parameter::Si = 64
	integer,parameter::Sj = 64
	
	integer::Nl = 2
	integer::Nf = 2
	integer::Ni = 2
	integer::Nj = 2
	
	!=========!
	!= Types =!
	!=========!
	
	type::ad3_t
		real(wp)::x = 0.0_wp
		real(wp),dimension(Sl)::d = 0.0_wp
		real(wp),dimension(Si,Sj,Sf)::I = 0.0_wp
	end type
	
	type::ad1_t
		real(wp)::x = 0.0_wp
		real(wp),dimension(Sl)::d = 0.0_wp
	end type
	
	!==============!
	!= Interfaces =!
	!==============!
	
	interface diff3
		module procedure diff_3s
		module procedure diff_3I
	end interface
	
	interface diff1
		module procedure diff_1s
	end interface
	
	interface assignment(=)
		module procedure assign_03
		module procedure assign_30
		module procedure assign_01
		module procedure assign_10
		module procedure assign_13
		module procedure assign_31
	end interface
	
	interface real
		module procedure real_3a
		module procedure real_1a
	end interface
	
	interface der
		module procedure der_3a0
		module procedure der_3a1
		module procedure der_3a2
		module procedure der_3a3
		module procedure der_1a0
		module procedure der_1a1
		module procedure der_1a2
		module procedure der_1a3
	end interface
	
	interface matmul
		module procedure matmul_03
		module procedure matmul_33
		module procedure matmul_01
		module procedure matmul_11
		module procedure matmul_13
		module procedure matmul_31
	end interface

	interface sum
		module procedure sum_3a1
		module procedure sum_3a2
		module procedure sum_3a3
		module procedure sum_3a4
		module procedure sum_1a1
		module procedure sum_1a2
		module procedure sum_1a3
		module procedure sum_1a4
	end interface
	
	interface sin
		module procedure sin_3a
		module procedure sin_1a
	end interface
	
	interface cos
		module procedure cos_3a
		module procedure cos_1a
	end interface
	
	interface log
		module procedure log_3a
		module procedure log_1a
	end interface
	
	interface exp
		module procedure exp_3a
		module procedure exp_1a
	end interface
	
	interface sqrt
		module procedure sqrt_3a
		module procedure sqrt_1a
	end interface
	
	interface operator(+)
		module procedure add_03
		module procedure add_30
		module procedure add_33
		module procedure add_01
		module procedure add_10
		module procedure add_11
		module procedure add_13
		module procedure add_31
	end interface
	
	interface operator(-)
		module procedure neg_3a
		module procedure sub_03
		module procedure sub_30
		module procedure sub_33
		module procedure neg_1a
		module procedure sub_01
		module procedure sub_10
		module procedure sub_11
		module procedure sub_31
		module procedure sub_13
	end interface
	
	interface operator(*)
		module procedure mul_03
		module procedure mul_30
		module procedure mul_33
		module procedure mul_01
		module procedure mul_10
		module procedure mul_11
		module procedure mul_31
		module procedure mul_13
	end interface
	
	interface operator(/)
		module procedure div_03
		module procedure div_30
		module procedure div_33
		module procedure div_01
		module procedure div_10
		module procedure div_11
		module procedure div_31
		module procedure div_13
	end interface
	
	interface operator(**)
		module procedure pow_03
		module procedure pow_30
		module procedure pow_33i
		module procedure pow_33
		module procedure pow_01
		module procedure pow_10
		module procedure pow_11i
		module procedure pow_11
		module procedure pow_31
		module procedure pow_13
	end interface
	
	!===========!
	!= Exports =!
	!===========!
	
	public::ad3_t
	public::ad1_t
	public::diff3
	public::diff1
	public::real
	public::der
	public::matmul
	public::sum
	
	public::sin
	public::cos
	public::log
	public::exp
	public::sqrt

	public::assignment(=)
	public::operator(+)
	public::operator(-)
	public::operator(*)
	public::operator(/)
	public::operator(**)
	
	public::set_adN
	public::get_adN
	
contains

	subroutine set_adN(iNl,iNi,iNj,iNf)
		integer,intent(in),optional::iNl
		integer,intent(in),optional::iNi
		integer,intent(in),optional::iNj
		integer,intent(in),optional::iNf
		
		if(present(iNl)) call guardedSet(iNl,Nl,Sl)
		if(present(iNi)) call guardedSet(iNi,Ni,Si)
		if(present(iNj)) call guardedSet(iNj,Nj,Sj)
		if(present(iNf)) call guardedSet(iNf,Nf,Sf)
		
	contains
		
		subroutine guardedSet(s,d,l)
			integer,intent(in)::s
			integer,intent(out)::d
			integer,intent(in)::l
			
			if(s<=l) then
				d  = s
			else
				write(*,*) 'Cannot set ad3_t size:',s,'>',l
				stop 1
			end if
		end subroutine guardedSet
		
	end subroutine set_adN

	function get_adN() result(o)
		integer,dimension(4)::o
		
		o = [Nl,Ni,Nj,Nf]
	end function get_adN

	!================!
	!= Constructors =!
	!================!

	pure function diff_3s(v,di) result(o)
		real(wp),intent(in)::v
		integer,intent(in)::di
		type(ad3_t)::o
		
		o%x = v
		o%d = 0.0_wp
		o%d(di) = 1.0_wp
		o%I = 0.0_wp
	end function diff_3s

	pure function diff_3I(v,di,dj,df) result(o)
		real(wp),intent(in)::v
		integer,intent(in)::di,dj,df
		type(ad3_t)::o
		
		o%x = v
		o%d = 0.0_wp
		o%I = 0.0_wp
		o%I(di,dj,df) = 1.0_wp
	end function diff_3I

	pure function diff_1s(v,di) result(o)
		real(wp),intent(in)::v
		integer,intent(in)::di
		type(ad1_t)::o
		
		o%x = v
		o%d = 0.0_wp
		o%d(di) = 1.0_wp
	end function diff_1s

	!=====================!
	!= Utility Functions =!
	!=====================!
	
	elemental function real_3a(a) result(o)
		type(ad3_t),intent(in)::a
		real(wp)::o
		
		o = a%x
	end function real_3a

	elemental function real_1a(a) result(o)
		type(ad1_t),intent(in)::a
		real(wp)::o
		
		o = a%x
	end function real_1a

	pure function der_3a0(a,i) result(o)
		type(ad3_t),intent(in)::a
		integer,intent(in)::i
		real(wp)::o
		
		o = a%d(i)
	end function der_3a0

	pure function der_3a1(a,i) result(o)
		type(ad3_t),dimension(:),intent(in)::a
		integer,intent(in)::i
		real(wp),dimension(:),allocatable::o
		
		o = a%d(i)
	end function der_3a1

	pure function der_3a2(a,i) result(o)
		type(ad3_t),dimension(:,:),intent(in)::a
		integer,intent(in)::i
		real(wp),dimension(:,:),allocatable::o
		
		o = a%d(i)
	end function der_3a2

	pure function der_3a3(a,i) result(o)
		type(ad3_t),dimension(:,:,:),intent(in)::a
		integer,intent(in)::i
		real(wp),dimension(:,:,:),allocatable::o
		
		o = a%d(i)
	end function der_3a3

	pure function der_1a0(a,i) result(o)
		type(ad1_t),intent(in)::a
		integer,intent(in)::i
		real(wp)::o
		
		o = a%d(i)
	end function der_1a0

	pure function der_1a1(a,i) result(o)
		type(ad1_t),dimension(:),intent(in)::a
		integer,intent(in)::i
		real(wp),dimension(:),allocatable::o
		
		o = a%d(i)
	end function der_1a1

	pure function der_1a2(a,i) result(o)
		type(ad1_t),dimension(:,:),intent(in)::a
		integer,intent(in)::i
		real(wp),dimension(:,:),allocatable::o
		
		o = a%d(i)
	end function der_1a2

	pure function der_1a3(a,i) result(o)
		type(ad1_t),dimension(:,:,:),intent(in)::a
		integer,intent(in)::i
		real(wp),dimension(:,:,:),allocatable::o
		
		o = a%d(i)
	end function der_1a3

	pure function sum_3a1(a) result(o)
		type(ad3_t),dimension(:),intent(in)::a
		type(ad3_t)::o
		integer::i,j,f,k
		
		o%x = sum(a%x)
		forall(k=1:Nl) o%d(k) = sum(a%d(k))
		forall(i=1:Ni,j=1:Nj,f=1:Nf) o%I(i,j,f) = sum(a%I(i,j,f))
	end function sum_3a1

	pure function sum_3a2(a) result(o)
		type(ad3_t),dimension(:,:),intent(in)::a
		type(ad3_t)::o
		integer::i,j,f,k
		
		o%x = sum(a%x)
		forall(k=1:Nl) o%d(k) = sum(a%d(k))
		forall(i=1:Ni,j=1:Nj,f=1:Nf) o%I(i,j,f) = sum(a%I(i,j,f))
	end function sum_3a2

	pure function sum_3a3(a) result(o)
		type(ad3_t),dimension(:,:,:),intent(in)::a
		type(ad3_t)::o
		integer::i,j,f,k
		
		o%x = sum(a%x)
		forall(k=1:Nl) o%d(k) = sum(a%d(k))
		forall(i=1:Ni,j=1:Nj,f=1:Nf) o%I(i,j,f) = sum(a%I(i,j,f))
	end function sum_3a3

	pure function sum_3a4(a) result(o)
		type(ad3_t),dimension(:,:,:,:),intent(in)::a
		type(ad3_t)::o
		integer::i,j,f,k
		
		o%x = sum(a%x)
		forall(k=1:Nl) o%d(k) = sum(a%d(k))
		forall(i=1:Ni,j=1:Nj,f=1:Nf) o%I(i,j,f) = sum(a%I(i,j,f))
	end function sum_3a4

	pure function sum_1a1(a) result(o)
		type(ad1_t),dimension(:),intent(in)::a
		type(ad1_t)::o
		integer::k
		
		o%x = sum(a%x)
		do k=1,Nl
			o%d(k) = sum(a%d(k))
		end do
	end function sum_1a1

	pure function sum_1a2(a) result(o)
		type(ad1_t),dimension(:,:),intent(in)::a
		type(ad1_t)::o
		integer::k
		
		o%x = sum(a%x)
		do k=1,Nl
			o%d(k) = sum(a%d(k))
		end do
	end function sum_1a2

	pure function sum_1a3(a) result(o)
		type(ad1_t),dimension(:,:,:),intent(in)::a
		type(ad1_t)::o
		integer::k
		
		o%x = sum(a%x)
		do k=1,Nl
			o%d(k) = sum(a%d(k))
		end do
	end function sum_1a3

	pure function sum_1a4(a) result(o)
		type(ad1_t),dimension(:,:,:,:),intent(in)::a
		type(ad1_t)::o
		integer::k
		
		o%x = sum(a%x)
		do k=1,Nl
			o%d(k) = sum(a%d(k))
		end do
	end function sum_1a4

	!==========!
	!= Matmul =!
	!==========!

	function matmul_03(A,x) result(o)
		real(wp),dimension(:,:),intent(in)::A
		type(ad3_t),dimension(size(A,2)),intent(in)::x
		type(ad3_t),dimension(:),allocatable::o
		
		integer::k
		
		allocate(o(size(x)))
		
		do k=1,size(A,1)
			o(k) = sum_3a1(A(k,:)*x(:))
		end do
	end function matmul_03

	function matmul_33(A,x) result(o)
		type(ad3_t),dimension(:,:),intent(in)::A
		type(ad3_t),dimension(size(A,2)),intent(in)::x
		type(ad3_t),dimension(:),allocatable::o
		
		integer::k
		
		allocate(o(size(x)))
		
		do k=1,size(A,1)
			o(k) = sum_3a1(A(k,:)*x(:))
		end do
	end function matmul_33

	function matmul_01(A,x) result(o)
		real(wp),dimension(:,:),intent(in)::A
		type(ad1_t),dimension(size(A,2)),intent(in)::x
		type(ad1_t),dimension(:),allocatable::o
		
		integer::k
		
		allocate(o(size(x)))
		
		do k=1,size(A,1)
			o(k) = sum_1a1(A(k,:)*x(:))
		end do
	end function matmul_01

	function matmul_11(A,x) result(o)
		type(ad1_t),dimension(:,:),intent(in)::A
		type(ad1_t),dimension(size(A,2)),intent(in)::x
		type(ad1_t),dimension(:),allocatable::o
		
		integer::k
		
		allocate(o(size(x)))
		
		do k=1,size(A,1)
			o(k) = sum_1a1(A(k,:)*x(:))
		end do
	end function matmul_11

	function matmul_13(A,x) result(o)
		type(ad1_t),dimension(:,:),intent(in)::A
		type(ad3_t),dimension(size(A,2)),intent(in)::x
		type(ad3_t),dimension(:),allocatable::o
		
		integer::k,w,i,j,f
		
		allocate(o(size(x)))
		
		do k=1,size(A,1)
			o(k)%x = sum(A(k,:)%x*x(:)%x)
		end do
		
		do w=1,size(x(1)%d)
			do k=1,size(A,1)
				o(k)%d(w) = sum(A(k,:)%d(w)*x(:)%d(w))
			end do
		end do
		
		do j=1,size(x(1)%I(1,:,1))
			do i=1,size(x(1)%I(:,1,1))
				do f=1,size(x(1)%I(1,1,:))
				
					do k=1,size(A,1)
						o(k)%I(i,j,f) = 0.0_wp
					end do
					
				end do
			end do
		end do
	end function matmul_13

	function matmul_31(A,x) result(o)
		type(ad3_t),dimension(:,:),intent(in)::A
		type(ad1_t),dimension(size(A,2)),intent(in)::x
		type(ad3_t),dimension(:),allocatable::o
		
		integer::k,w,i,j,f
		
		allocate(o(size(x)))
		
		do k=1,size(A,1)
			o(k)%x = sum(A(k,:)%x*x(:)%x)
		end do
		
		do w=1,size(x(1)%d)
			do k=1,size(A,1)
				o(k)%d(w) = sum(A(k,:)%d(w)*x(:)%d(w))
			end do
		end do
		
		do j=1,size(A(1,1)%I(1,:,1))
			do i=1,size(A(1,1)%I(:,1,1))
				do f=1,size(A(1,1)%I(1,1,:))
				
					do k=1,size(A,1)
						o(k)%I(i,j,f) = 0.0_wp
					end do
					
				end do
			end do
		end do
		
		
	end function matmul_31

	!============================!
	!= Transcendental Functions =!
	!============================!

	elemental function sin_3a(u) result(o)
		type(ad3_t),intent(in)::u
		type(ad3_t)::o
		
		o%x = sin(u%x)
		o%d(1:Nl) = cos(u%x)*u%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = cos(u%x)*u%I(1:Ni,1:Nj,1:Nf)
	end function sin_3a

	elemental function sin_1a(u) result(o)
		type(ad1_t),intent(in)::u
		type(ad1_t)::o
		
		o%x = sin(u%x)
		o%d(1:Nl) = cos(u%x)*u%d(1:Nl)
	end function sin_1a

	elemental function cos_3a(u) result(o)
		type(ad3_t),intent(in)::u
		type(ad3_t)::o
		
		o%x = cos(u%x)
		o%d(1:Nl) = -sin(u%x)*u%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = -sin(u%x)*u%I(1:Ni,1:Nj,1:Nf)
	end function cos_3a

	elemental function cos_1a(u) result(o)
		type(ad1_t),intent(in)::u
		type(ad1_t)::o
		
		o%x = cos(u%x)
		o%d(1:Nl) = -sin(u%x)*u%d(1:Nl)
	end function cos_1a

	elemental function log_3a(u) result(o)
		type(ad3_t),intent(in)::u
		type(ad3_t)::o
		
		o = log(u%x)
		o%d(1:Nl) = u%d(1:Nl)/u%x
		o%I(1:Ni,1:Nj,1:Nf) = u%I(1:Ni,1:Nj,1:Nf)/u%x
	end function log_3a

	elemental function log_1a(u) result(o)
		type(ad1_t),intent(in)::u
		type(ad1_t)::o
		
		o = log(u%x)
		o%d(1:Nl) = u%d(1:Nl)/u%x
	end function log_1a

	elemental function exp_3a(u) result(o)
		type(ad3_t),intent(in)::u
		type(ad3_t)::o
		
		o = exp(u%x)
		o%d(1:Nl) = exp(u%x)*u%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = exp(u%x)*u%I(1:Ni,1:Nj,1:Nf)
	end function exp_3a

	elemental function exp_1a(u) result(o)
		type(ad1_t),intent(in)::u
		type(ad1_t)::o
		
		o = exp(u%x)
		o%d(1:Nl) = exp(u%x)*u%d(1:Nl)
	end function exp_1a

	!==============!
	!= Assignment =!
	!==============!

	elemental subroutine assign_03(u,v)
		real(wp),intent(out)::u
		type(ad3_t),intent(in)::v
		
		u = v%x
	end subroutine assign_03

	elemental subroutine assign_30(u,v)
		type(ad3_t),intent(out)::u
		real(wp),intent(in)::v
		
		u%x = v
		u%d(1:Nl) = 0.0_wp
		u%I(1:Ni,1:Nj,1:Nf) = 0.0_wp
	end subroutine assign_30

	elemental subroutine assign_01(u,v)
		real(wp),intent(out)::u
		type(ad1_t),intent(in)::v
		
		u = v%x
	end subroutine assign_01

	elemental subroutine assign_10(u,v)
		type(ad1_t),intent(out)::u
		real(wp),intent(in)::v
		
		u%x = v
		u%d(1:Nl) = 0.0_wp
	end subroutine assign_10

	elemental subroutine assign_13(u,v)
		type(ad1_t),intent(out)::u
		type(ad3_t),intent(in)::v
		
		u%x = v%x
		u%d(1:Nl) = v%d(1:Nl)
	end subroutine assign_13

	elemental subroutine assign_31(u,v)
		type(ad3_t),intent(out)::u
		type(ad1_t),intent(in)::v
		
		u%x = v%x
		u%d(1:Nl) = v%d(1:Nl)
		u%I(1:Ni,1:Nj,1:Nf) = 0.0_wp
	end subroutine assign_31

	!============!
	!= Addition =!
	!============!

	elemental function add_03(u,v) result(o)
		real(wp),intent(in)::u
		type(ad3_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u+v%x
		o%d(1:Nl) = v%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = v%I(1:Ni,1:Nj,1:Nf)
	end function add_03

	elemental function add_30(u,v) result(o)
		type(ad3_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x+v
		o%d(1:Nl) = u%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = u%I(1:Ni,1:Nj,1:Nf)
	end function add_30

	elemental function add_33(u,v) result(o)
		type(ad3_t),intent(in)::u
		type(ad3_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x+v%x
		o%d(1:Nl) = u%d(1:Nl)+v%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = u%I(1:Ni,1:Nj,1:Nf)+v%I(1:Ni,1:Nj,1:Nf)
	end function add_33

	elemental function add_01(u,v) result(o)
		real(wp),intent(in)::u
		type(ad1_t),intent(in)::v
		type(ad1_t)::o
		
		o%x = u+v%x
		o%d(1:Nl) = v%d(1:Nl)
	end function add_01

	elemental function add_10(u,v) result(o)
		type(ad1_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad1_t)::o
		
		o%x = u%x+v
		o%d(1:Nl) = u%d(1:Nl)
	end function add_10

	elemental function add_11(u,v) result(o)
		type(ad1_t),intent(in)::u
		type(ad1_t),intent(in)::v
		type(ad1_t)::o
		
		o%x = u%x+v%x
		o%d(1:Nl) = u%d(1:Nl)+v%d(1:Nl)
	end function add_11

	elemental function add_31(u,v) result(o)
		type(ad3_t),intent(in)::u
		type(ad1_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x+v%x
		o%d(1:Nl) = u%d(1:Nl)+v%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = u%I(1:Ni,1:Nj,1:Nf)
	end function add_31

	elemental function add_13(u,v) result(o)
		type(ad1_t),intent(in)::u
		type(ad3_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x+v%x
		o%d(1:Nl) = u%d(1:Nl)+v%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = v%I(1:Ni,1:Nj,1:Nf)
	end function add_13

	!============!
	!= Negation =!
	!============!

	elemental function neg_3a(u) result(o)
		type(ad3_t),intent(in)::u
		type(ad3_t)::o
		
		o%x = -u%x
		o%d(1:Nl) = -u%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = -u%I(1:Ni,1:Nj,1:Nf)
	end function neg_3a

	elemental function neg_1a(u) result(o)
		type(ad1_t),intent(in)::u
		type(ad1_t)::o
		
		o%x = -u%x
		o%d(1:Nl) = -u%d(1:Nl)
	end function neg_1a

	!===============!
	!= Subtraction =!
	!===============!

	elemental function sub_03(u,v) result(o)
		real(wp),intent(in)::u
		type(ad3_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u-v%x
		o%d(1:Nl) = -v%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = -v%I(1:Ni,1:Nj,1:Nf)
	end function sub_03

	elemental function sub_30(u,v) result(o)
		type(ad3_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x-v
		o%d(1:Nl) = u%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = u%I(1:Ni,1:Nj,1:Nf)
	end function sub_30

	elemental function sub_33(u,v) result(o)
		type(ad3_t),intent(in)::u
		type(ad3_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x-v%x
		o%d(1:Nl) = u%d(1:Nl)-v%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = u%I(1:Ni,1:Nj,1:Nf)-v%I(1:Ni,1:Nj,1:Nf)
	end function sub_33

	elemental function sub_01(u,v) result(o)
		real(wp),intent(in)::u
		type(ad1_t),intent(in)::v
		type(ad1_t)::o
		
		o%x = u-v%x
		o%d(1:Nl) = -v%d(1:Nl)
	end function sub_01

	elemental function sub_10(u,v) result(o)
		type(ad1_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad1_t)::o
		
		o%x = u%x-v
		o%d(1:Nl) = u%d(1:Nl)
	end function sub_10

	elemental function sub_11(u,v) result(o)
		type(ad1_t),intent(in)::u
		type(ad1_t),intent(in)::v
		type(ad1_t)::o
		
		o%x = u%x-v%x
		o%d(1:Nl) = u%d(1:Nl)-v%d(1:Nl)
	end function sub_11

	elemental function sub_31(u,v) result(o)
		type(ad3_t),intent(in)::u
		type(ad1_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x-v%x
		o%d(1:Nl) = u%d(1:Nl)-v%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = u%I(1:Ni,1:Nj,1:Nf)
	end function sub_31

	elemental function sub_13(u,v) result(o)
		type(ad1_t),intent(in)::u
		type(ad3_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x-v%x
		o%d(1:Nl) = u%d(1:Nl)-v%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = -v%I(1:Ni,1:Nj,1:Nf)
	end function sub_13

	!==================!
	!= Multiplication =!
	!==================!

	elemental function mul_03(u,v) result(o)
		real(wp),intent(in)::u
		type(ad3_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u*v%x
		o%d(1:Nl) = u*v%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = u*v%I(1:Ni,1:Nj,1:Nf)
	end function mul_03

	elemental function mul_30(u,v) result(o)
		type(ad3_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x*v
		o%d(1:Nl) = u%d(1:Nl)*v
		o%I(1:Ni,1:Nj,1:Nf) = u%I(1:Ni,1:Nj,1:Nf)
	end function mul_30

	elemental function mul_33(u,v) result(o)
		type(ad3_t),intent(in)::u
		type(ad3_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x*v%x
		o%d(1:Nl) = u%x*v%d(1:Nl)+v%x*u%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = u%x*v%I(1:Ni,1:Nj,1:Nf)+v%x*u%I(1:Ni,1:Nj,1:Nf)
	end function mul_33

	elemental function mul_01(u,v) result(o)
		real(wp),intent(in)::u
		type(ad1_t),intent(in)::v
		type(ad1_t)::o
		
		o%x = u*v%x
		o%d(1:Nl) = u*v%d(1:Nl)
	end function mul_01

	elemental function mul_10(u,v) result(o)
		type(ad1_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad1_t)::o
		
		o%x = u%x*v
		o%d(1:Nl) = u%d(1:Nl)*v
	end function mul_10

	elemental function mul_11(u,v) result(o)
		type(ad1_t),intent(in)::u
		type(ad1_t),intent(in)::v
		type(ad1_t)::o
		
		o%x = u%x*v%x
		o%d(1:Nl) = u%x*v%d(1:Nl)+v%x*u%d(1:Nl)
	end function mul_11

	elemental function mul_31(u,v) result(o)
		type(ad3_t),intent(in)::u
		type(ad1_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x*v%x
		o%d(1:Nl) = u%x*v%d(1:Nl)+v%x*u%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = v%x*u%I(1:Ni,1:Nj,1:Nf)
	end function mul_31

	elemental function mul_13(u,v) result(o)
		type(ad1_t),intent(in)::u 
		type(ad3_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x*v%x
		o%d(1:Nl) = u%x*v%d(1:Nl)+v%x*u%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = u%x*v%I(1:Ni,1:Nj,1:Nf)
	end function mul_13

	!============!
	!= Division =!
	!============!

	elemental function div_03(u,v) result(o)
		real(wp),intent(in)::u
		type(ad3_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u/v%x
		o%d(1:Nl) = (-u*v%d(1:Nl))/(v%x**2)
		o%I(1:Ni,1:Nj,1:Nf) = (-u*v%I(1:Ni,1:Nj,1:Nf))/(v%x**2)
	end function div_03

	elemental function div_30(u,v) result(o)
		type(ad3_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x/v
		o%d(1:Nl) = (v*u%d(1:Nl))/(v**2)
		o%I(1:Ni,1:Nj,1:Nf) = (v*u%I(1:Ni,1:Nj,1:Nf))/(v**2)
	end function div_30

	elemental function div_33(u,v) result(o)
		type(ad3_t),intent(in)::u
		type(ad3_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x/v%x
		o%d(1:Nl) = (v%x*u%d(1:Nl)-u%x*v%d(1:Nl))/(v%x**2)
		o%I(1:Ni,1:Nj,1:Nf) = (v%x*u%I(1:Ni,1:Nj,1:Nf)-u%x*v%I(1:Ni,1:Nj,1:Nf))/(v%x**2)
	end function div_33

	elemental function div_01(u,v) result(o)
		real(wp),intent(in)::u
		type(ad1_t),intent(in)::v
		type(ad1_t)::o
		
		o%x = u/v%x
		o%d(1:Nl) = (-u*v%d(1:Nl))/(v%x**2)
	end function div_01

	elemental function div_10(u,v) result(o)
		type(ad1_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad1_t)::o
		
		o%x = u%x/v
		o%d(1:Nl) = (v*u%d(1:Nl))/(v**2)
	end function div_10

	elemental function div_11(u,v) result(o)
		type(ad1_t),intent(in)::u
		type(ad1_t),intent(in)::v
		type(ad1_t)::o
		
		o%x = u%x/v%x
		o%d(1:Nl) = (v%x*u%d(1:Nl)-u%x*v%d(1:Nl))/(v%x**2)
	end function div_11

	elemental function div_31(u,v) result(o)
		type(ad3_t),intent(in)::u
		type(ad1_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x/v%x
		o%d(1:Nl) = (v%x*u%d(1:Nl)-u%x*v%d(1:Nl))/(v%x**2)
		o%I(1:Ni,1:Nj,1:Nf) = (v%x*u%I(1:Ni,1:Nj,1:Nf))/(v%x**2)
	end function div_31

	elemental function div_13(u,v) result(o)
		type(ad1_t),intent(in)::u
		type(ad3_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x/v%x
		o%d(1:Nl) = (v%x*u%d(1:Nl)-u%x*v%d(1:Nl))/(v%x**2)
		o%I(1:Ni,1:Nj,1:Nf) = (-u%x*v%I(1:Ni,1:Nj,1:Nf))/(v%x**2)
	end function div_13

	!=========!
	!= Power =!
	!=========!
	
	elemental function pow_03(u,v) result(o)
		real(wp),intent(in)::u
		type(ad3_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u**v%x
		o%d(1:Nl) = (u**v%x*log(u))*v%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = (u**v%x*log(u))*v%I(1:Ni,1:Nj,1:Nf)
	end function pow_03

	elemental function pow_30(u,v) result(o)
		type(ad3_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x**v
		o%d(1:Nl) = (u%x**(v-1.0_wp)*v)*u%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = (u%x**(v-1.0_wp)*v)*u%I(1:Ni,1:Nj,1:Nf)
	end function pow_30

	elemental function pow_33i(u,v) result(o)
		type(ad3_t),intent(in)::u
		integer,intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x**v
		o%d(1:Nl) = (u%x**real(v-1,wp)*v)*u%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = (u%x**real(v-1,wp)*v)*u%I(1:Ni,1:Nj,1:Nf)
	end function pow_33i

	elemental function pow_33(u,v) result(o)
		type(ad3_t),intent(in)::u
		type(ad3_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x**v%x
		o%d(1:Nl) = (u%x**(v%x-1.0_wp)*v%x)*u%d(1:Nl)+(u%x**v%x*log(u%x))*v%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = (u%x**(v%x-1.0_wp)*v%x)*u%I(1:Ni,1:Nj,1:Nf)+(u%x**v%x*log(u%x))*v%I(1:Ni,1:Nj,1:Nf)
	end function pow_33

	elemental function pow_01(u,v) result(o)
		real(wp),intent(in)::u
		type(ad1_t),intent(in)::v
		type(ad1_t)::o
		
		o%x = u**v%x
		o%d(1:Nl) = (u**v%x*log(u))*v%d(1:Nl)
	end function pow_01

	elemental function pow_10(u,v) result(o)
		type(ad1_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad1_t)::o
		
		o%x = u%x**v
		o%d(1:Nl) = (u%x**(v-1.0_wp)*v)*u%d(1:Nl)
	end function pow_10

	elemental function pow_11i(u,v) result(o)
		type(ad1_t),intent(in)::u
		integer,intent(in)::v
		type(ad1_t)::o
		
		o%x = u%x**v
		o%d(1:Nl) = (u%x**real(v-1,wp)*v)*u%d(1:Nl)
	end function pow_11i

	elemental function pow_11(u,v) result(o)
		type(ad1_t),intent(in)::u
		type(ad1_t),intent(in)::v
		type(ad1_t)::o
		
		o%x = u%x**v%x
		o%d(1:Nl) = (u%x**(v%x-1.0_wp)*v%x)*u%d(1:Nl)+(u%x**v%x*log(u%x))*v%d(1:Nl)
	end function pow_11

	elemental function pow_31(u,v) result(o)
		type(ad3_t),intent(in)::u
		type(ad1_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x**v%x
		o%d(1:Nl) = (u%x**(v%x-1.0_wp)*v%x)*u%d(1:Nl)+(u%x**v%x*log(u%x))*v%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = (u%x**(v%x-1.0_wp)*v%x)*u%I(1:Ni,1:Nj,1:Nf)
	end function pow_31

	elemental function pow_13(u,v) result(o)
		type(ad1_t),intent(in)::u
		type(ad3_t),intent(in)::v
		type(ad3_t)::o
		
		o%x = u%x**v%x
		o%d(1:Nl) = (u%x**(v%x-1.0_wp)*v%x)*u%d(1:Nl)+(u%x**v%x*log(u%x))*v%d(1:Nl)
		o%I(1:Ni,1:Nj,1:Nf) = ((u%x**v%x*log(u%x))*v%I(1:Ni,1:Nj,1:Nf))
	end function pow_13

	elemental function sqrt_3a(u) result(o)
		type(ad3_t),intent(in)::u
		type(ad3_t)::o
		
		o = u**(0.5_wp)
	end function sqrt_3a

	elemental function sqrt_1a(u) result(o)
		type(ad1_t),intent(in)::u
		type(ad1_t)::o
		
		o = u**(0.5_wp)
	end function sqrt_1a

end module autoDiff_mod
