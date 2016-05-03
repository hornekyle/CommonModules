module autoDiff_mod
	!! Module implementing automatic differentiation
	!! @todo
	!! Full documentation must be written  
	!! Truly rigourous testing of this module is needed  
	!! Less-commonly used routines need to be implemented  
	!! A full implementation of matmul is needed
	use kinds_mod
	implicit none
	private
	
	!==============!
	!= Parameters =!
	!==============!
	
	integer,parameter::S  = 1
	integer,parameter::Sf = 1
	integer,parameter::Si = 1
	integer,parameter::Sj = 1
	
	integer::N  = 1
	integer::Nf = 1
	integer::Ni = 1
	integer::Nj = 1
	
	!=========!
	!= Types =!
	!=========!
	
	type::ad_t
		real(wp)::x = 0.0_wp
			!! Real part of dual number
		real(wp),dimension(S)::d = 0.0_wp
			!! Vector of first derivatives
		real(wp),dimension(Si,Sj,Sf)::I = 0.0_wp
			!! Matrix of first derivatives
	end type
	
	!==============!
	!= Interfaces =!
	!==============!
	
	interface diff
		module procedure diff_s
		module procedure diff_I
	end interface
	
	interface assignment(=)
		module procedure assign_ra
		module procedure assign_ar
	end interface
	
	interface real
		module procedure real_a
	end interface
	
	interface der
		module procedure der_a0
		module procedure der_a1
		module procedure der_a2
		module procedure der_a3
	end interface
	
	interface matmul
		module procedure matmul_ra
		module procedure matmul_aa
	end interface
	
	interface sum
		module procedure sum_a1
		module procedure sum_a2
		module procedure sum_a3
		module procedure sum_a4
	end interface
	
	interface sin
		module procedure sin_a
	end interface
	
	interface cos
		module procedure cos_a
	end interface
	
	interface log
		module procedure log_a
	end interface
	
	interface exp
		module procedure exp_a
	end interface
	
	interface sqrt
		module procedure sqrt_a
	end interface
	
	interface operator(+)
		module procedure add_ra
		module procedure add_ar
		module procedure add_aa
	end interface
	
	interface operator(-)
		module procedure neg_a
		module procedure sub_ra
		module procedure sub_ar
		module procedure sub_aa
	end interface
	
	interface operator(*)
		module procedure mul_ra
		module procedure mul_ar
		module procedure mul_aa
	end interface
	
	interface operator(/)
		module procedure div_ra
		module procedure div_ar
		module procedure div_aa
	end interface
	
	interface operator(**)
		module procedure pow_ra
		module procedure pow_ar
		module procedure pow_ai
		module procedure pow_aa
	end interface
	
	!===========!
	!= Exports =!
	!===========!
	
	public::ad_t
	public::diff
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

	subroutine set_adN(iN,iNi,iNj,iNf)
		integer,intent(in),optional::iN
		integer,intent(in),optional::iNi
		integer,intent(in),optional::iNj
		integer,intent(in),optional::iNf
		
		if(present(iN )) call guardedSet(iN ,N ,S )
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
				write(*,*) 'Cannot set ad_t size:',s,'>',l
				stop 1
			end if
		end subroutine guardedSet
		
	end subroutine set_adN

	function get_adN() result(o)
		integer,dimension(4)::o
		o = [N,Ni,Nj,Nf]
	end function get_adN

	!================!
	!= Constructors =!
	!================!
	
	pure function diff_s(v,di) result(o)
		real(wp),intent(in)::v
		integer,intent(in)::di
		type(ad_t)::o
		
		o%x = v
		o%d = 0.0_wp
		o%d(di) = 1.0_wp
		o%I = 0.0_wp
	end function diff_s
	
	pure function diff_I(v,di,dj,df) result(o)
		real(wp),intent(in)::v
		integer,intent(in)::di,dj,df
		type(ad_t)::o
		
		o%x = v
		o%d = 0.0_wp
		o%I = 0.0_wp
		o%I(di,dj,df) = 1.0_wp
	end function diff_I
	
	!=====================!
	!= Utility Functions =!
	!=====================!
	
	elemental function real_a(a) result(o)
		type(ad_t),intent(in)::a
		real(wp)::o
		
		o = a%x
	end function real_a
	
	pure function der_a0(a,i) result(o)
		type(ad_t),intent(in)::a
		integer,intent(in)::i
		real(wp)::o
		
		o = a%d(i)
	end function der_a0
	
	pure function der_a1(a,i) result(o)
		type(ad_t),dimension(:),intent(in)::a
		integer,intent(in)::i
		real(wp),dimension(:),allocatable::o
		
		o = a%d(i)
	end function der_a1
	
	pure function der_a2(a,i) result(o)
		type(ad_t),dimension(:,:),intent(in)::a
		integer,intent(in)::i
		real(wp),dimension(:,:),allocatable::o
		
		o = a%d(i)
	end function der_a2
	
	pure function der_a3(a,i) result(o)
		type(ad_t),dimension(:,:,:),intent(in)::a
		integer,intent(in)::i
		real(wp),dimension(:,:,:),allocatable::o
		
		o = a%d(i)
	end function der_a3
	
	pure function sum_a1(a) result(o)
		type(ad_t),dimension(:),intent(in)::a
		type(ad_t)::o
		integer::i,j,f,k
		
		o%x = sum(a%x)
		forall(k=1:N) o%d(k) = sum(a%d(k))
		forall(i=1:Ni,j=1:Nj,f=1:Nf) o%I(i,j,f) = sum(a%I(i,j,f))
	end function sum_a1

	pure function sum_a2(a) result(o)
		type(ad_t),dimension(:,:),intent(in)::a
		type(ad_t)::o
		integer::i,j,f,k
		
		o%x = sum(a%x)
		forall(k=1:N) o%d(k) = sum(a%d(k))
		forall(i=1:Ni,j=1:Nj,f=1:Nf) o%I(i,j,f) = sum(a%I(i,j,f))
	end function sum_a2

	pure function sum_a3(a) result(o)
		type(ad_t),dimension(:,:,:),intent(in)::a
		type(ad_t)::o
		integer::i,j,f,k
		
		o%x = sum(a%x)
		forall(k=1:N) o%d(k) = sum(a%d(k))
		forall(i=1:Ni,j=1:Nj,f=1:Nf) o%I(i,j,f) = sum(a%I(i,j,f))
	end function sum_a3

	pure function sum_a4(a) result(o)
		type(ad_t),dimension(:,:,:,:),intent(in)::a
		type(ad_t)::o
		integer::i,j,f,k
		
		o%x = sum(a%x)
		forall(k=1:N) o%d(k) = sum(a%d(k))
		forall(i=1:Ni,j=1:Nj,f=1:Nf) o%I(i,j,f) = sum(a%I(i,j,f))
	end function sum_a4

	!==========!
	!= Matmul =!
	!==========!

	function matmul_ra(A,x) result(o)
		real(wp),dimension(:,:),intent(in)::A
		type(ad_t),dimension(size(A,2)),intent(in)::x
		type(ad_t),dimension(:),allocatable::o
		
		integer::k
		
		allocate(o(size(x)))
		
		do k=1,size(A,1)
			o(k) = sum_a1(A(k,:)*x(:))
		end do
	end function matmul_ra

	function matmul_aa(A,x) result(o)
		type(ad_t),dimension(:,:),intent(in)::A
		type(ad_t),dimension(size(A,2)),intent(in)::x
		type(ad_t),dimension(:),allocatable::o
		
		integer::k
		
		allocate(o(size(x)))
		
		do k=1,size(A,1)
			o(k) = sum_a1(A(k,:)*x(:))
		end do
	end function matmul_aa

	!============================!
	!= Transcendental Functions =!
	!============================!

	elemental function sin_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o%x = sin(u%x)
		o%d(1:N) = cos(u%x)*u%d(1:N)
		o%I(1:Ni,1:Nj,1:Nf) = cos(u%x)*u%I(1:Ni,1:Nj,1:Nf)
	end function sin_a

	elemental function cos_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o%x = cos(u%x)
		o%I(1:Ni,1:Nj,1:Nf) = -sin(u%x)*u%I(1:Ni,1:Nj,1:Nf)
	end function cos_a

	elemental function log_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o = log(u%x)
		o%d(1:N) = u%d(1:N)/u%x
		o%I(1:Ni,1:Nj,1:Nf) = u%I(1:Ni,1:Nj,1:Nf)/u%x
	end function log_a

	elemental function exp_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o = exp(u%x)
		o%d(1:N) = exp(u%x)*u%d(1:N)
		o%I(1:Ni,1:Nj,1:Nf) = exp(u%x)*u%I(1:Ni,1:Nj,1:Nf)
	end function exp_a

	!==============!
	!= Assignment =!
	!==============!
	
	elemental subroutine assign_ra(u,v)
		real(wp),intent(out)::u
		type(ad_t),intent(in)::v
		
		u = v%x
	end subroutine assign_ra

	elemental subroutine assign_ar(u,v)
		type(ad_t),intent(out)::u
		real(wp),intent(in)::v
		
		u%x = v
		u%d(1:N) = 0.0_wp
		u%I(1:Ni,1:Nj,1:Nf) = 0.0_wp
	end subroutine assign_ar

	!============!
	!= Addition =!
	!============!

	elemental function add_ra(u,v) result(o)
		real(wp),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o%x = u+v%x
		o%d(1:N) = v%d(1:N)
		o%I(1:Ni,1:Nj,1:Nf) = v%I(1:Ni,1:Nj,1:Nf)
	end function add_ra

	elemental function add_ar(u,v) result(o)
		type(ad_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad_t)::o
		
		o%x = u%x+v
		o%d(1:N) = u%d(1:N)
		o%I(1:Ni,1:Nj,1:Nf) = u%I(1:Ni,1:Nj,1:Nf)
	end function add_ar

	elemental function add_aa(u,v) result(o)
		type(ad_t),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o%x = u%x+v%x
		o%d(1:N) = u%d(1:N)+v%d(1:N)
		o%I(1:Ni,1:Nj,1:Nf) = u%I(1:Ni,1:Nj,1:Nf)+v%I(1:Ni,1:Nj,1:Nf)
	end function add_aa

	!============!
	!= Negation =!
	!============!

	elemental function neg_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o%x = -u%x
		o%d(1:N) = -u%d(1:N)
		o%I(1:Ni,1:Nj,1:Nf) = -u%I(1:Ni,1:Nj,1:Nf)
	end function neg_a

	!===============!
	!= Subtraction =!
	!===============!

	elemental function sub_ra(u,v) result(o)
		real(wp),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o%x = u-v%x
		o%d(1:N) = -v%d(1:N)
		o%I(1:Ni,1:Nj,1:Nf) = -v%I(1:Ni,1:Nj,1:Nf)
	end function sub_ra

	elemental function sub_ar(u,v) result(o)
		type(ad_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad_t)::o
		
		o%x = u%x-v
		o%d(1:N) = u%d(1:N)
		o%I(1:Ni,1:Nj,1:Nf) = u%I(1:Ni,1:Nj,1:Nf)
	end function sub_ar

	elemental function sub_aa(u,v) result(o)
		type(ad_t),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o%x = u%x-v%x
		o%d(1:N) = u%d(1:N)-v%d(1:N)
		o%I(1:Ni,1:Nj,1:Nf) = u%I(1:Ni,1:Nj,1:Nf)-v%I(1:Ni,1:Nj,1:Nf)
	end function sub_aa

	!==================!
	!= Multiplication =!
	!==================!

	elemental function mul_ra(u,v) result(o)
		real(wp),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o%x = u*v%x
		o%d(1:N) = u*v%d(1:N)
		o%I(1:Ni,1:Nj,1:Nf) = u*v%I(1:Ni,1:Nj,1:Nf)
	end function mul_ra

	elemental function mul_ar(u,v) result(o)
		type(ad_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad_t)::o
		
		o%x = u%x*v
		o%d(1:N) = u%d(1:N)*v
		o%I(1:Ni,1:Nj,1:Nf) = u%I(1:Ni,1:Nj,1:Nf)
	end function mul_ar

	elemental function mul_aa(u,v) result(o)
		type(ad_t),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o%x = u%x*v%x
		o%d(1:N) = u%x*v%d(1:N)+v%x*u%d(1:N)
		o%I(1:Ni,1:Nj,1:Nf) = u%x*v%I(1:Ni,1:Nj,1:Nf)+v%x*u%I(1:Ni,1:Nj,1:Nf)
	end function mul_aa

	!============!
	!= Division =!
	!============!

	elemental function div_ra(u,v) result(o)
		real(wp),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o%x = u/v%x
		o%d(1:N) = (-u*v%d(1:N))/(v%x**2)
		o%I(1:Ni,1:Nj,1:Nf) = (-u*v%I(1:Ni,1:Nj,1:Nf))/(v%x**2)
	end function div_ra

	elemental function div_ar(u,v) result(o)
		type(ad_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad_t)::o
		
		o%x = u%x/v
		o%d(1:N) = (v*u%d(1:N))/(v**2)
		o%I(1:Ni,1:Nj,1:Nf) = (v*u%I(1:Ni,1:Nj,1:Nf))/(v**2)
	end function div_ar

	elemental function div_aa(u,v) result(o)
		type(ad_t),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o%x = u%x/v%x
		o%d(1:N) = (v%x*u%d(1:N)-u%x*v%d(1:N))/(v%x**2)
		o%I(1:Ni,1:Nj,1:Nf) = (v%x*u%I(1:Ni,1:Nj,1:Nf)-u%x*v%I(1:Ni,1:Nj,1:Nf))/(v%x**2)
	end function div_aa

	!=========!
	!= Power =!
	!=========!
	
	elemental function pow_ra(u,v) result(o)
		real(wp),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o
		
		o%x = u**v%x
		o%d(1:N) = (u**v%x*log(u))*v%d(1:N)
		o%I(1:Ni,1:Nj,1:Nf) = (u**v%x*log(u))*v%I(1:Ni,1:Nj,1:Nf)
	end function pow_ra

	elemental function pow_ar(u,v) result(o)
		type(ad_t),intent(in)::u
		real(wp),intent(in)::v
		type(ad_t)::o
		
		o%x = u%x**v
		o%d(1:N) = (u%x**(v-1.0_wp)*v)*u%d(1:N)
		o%I(1:Ni,1:Nj,1:Nf) = (u%x**(v-1.0_wp)*v)*u%I(1:Ni,1:Nj,1:Nf)
	end function pow_ar

	elemental function pow_ai(u,v) result(o)
		type(ad_t),intent(in)::u
		integer,intent(in)::v
		type(ad_t)::o
		
		o%x = u%x**v
		o%d(1:N) = (u%x**real(v-1,wp)*v)*u%d(1:N)
		o%I(1:Ni,1:Nj,1:Nf) = (u%x**real(v-1,wp)*v)*u%I(1:Ni,1:Nj,1:Nf)
	end function pow_ai

	elemental function pow_aa(u,v) result(o)
		type(ad_t),intent(in)::u
		type(ad_t),intent(in)::v
		type(ad_t)::o

		o%x = u%x**v%x
		o%d(1:N) = (u%x**(v%x-1.0_wp)*v%x)*u%d(1:N)+(u%x**v%x*log(u%x))*v%d(1:N)
		o%I(1:Ni,1:Nj,1:Nf) = (u%x**(v%x-1.0_wp)*v%x)*u%I(1:Ni,1:Nj,1:Nf)+(u%x**v%x*log(u%x))*v%I(1:Ni,1:Nj,1:Nf)
	end function pow_aa

	elemental function sqrt_a(u) result(o)
		type(ad_t),intent(in)::u
		type(ad_t)::o
		
		o = u**(0.5_wp)
	end function sqrt_a

end module autoDiff_mod
