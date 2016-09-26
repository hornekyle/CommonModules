module autoDiffArray_mod
	use kinds_mod
	use autoDiffType_mod
	use autoDiffOperator_mod
	implicit none
	private
	
	! Maximum Location
	interface maxloc
		module procedure maxloc_a1
	end interface
	
	! Summation
	interface sum
		module procedure sum_a1
		module procedure sum_a2
		module procedure sum_a3
	end interface
	public::sum
	
	! L2 Norm
	interface norm2
		module procedure norm2_a1
		module procedure norm2_a2
		module procedure norm2_a3
	end interface
	public::norm2
	
	! Matrix Multiply
	interface matmul
		module procedure matmul_a21
		module procedure matmul_a22
	end interface
	public::matmul
	
	! TDMA
	interface TDMA
		module procedure TDMA_sA
		module procedure TDMA_mA
	end interface
	public::TDMA
	
	! solveLU
	interface solveLU
		module procedure solveLU_s
		module procedure solveLU_m
	end interface
	public::solveLU
	
contains

	!====================!
	!= Maximum Location =!
	!====================!

	function maxloc_a1(u,idx) result(o)
		type(ad_t),dimension(:),intent(in)::u
		integer,intent(in)::idx
		integer::o
		
		integer::N,i
		
		N = size(u)
		o = maxloc([( u(i)%val() , i=1,N )],1)
	end function maxloc_a1

	!=============!
	!= Summation =!
	!=============!

	pure function sum_a1(u) result(o)
		type(ad_t),dimension(:),intent(in)::u
		type(ad_t)::o
		
		integer,dimension(1)::N
		integer::i
		
		N = shape(u)
		
		o = ad_t( 0.0_wp , size(u(1)%grad()) )
		
		do i=1,N(1)
			o = o+u(i)
		end do
	end function sum_a1

	pure function sum_a2(u) result(o)
		type(ad_t),dimension(:,:),intent(in)::u
		type(ad_t)::o
		
		integer,dimension(2)::N
		integer::i,j
		
		N = shape(u)
		
		o = ad_t( 0.0_wp , size(u(1,1)%grad()) )
		
		do j=1,N(2)
			do i=1,N(1)
				o = o+u(i,j)
			end do
		end do
	end function sum_a2

	pure function sum_a3(u) result(o)
		type(ad_t),dimension(:,:,:),intent(in)::u
		type(ad_t)::o
		
		integer,dimension(3)::N
		integer::i,j,k
		
		N = shape(u)
		
		o = ad_t( 0.0_wp , size(u(1,1,1)%grad()) )
		
		do k=1,N(3)
			do j=1,N(2)
				do i=1,N(1)
					o = o+u(i,j,k)
				end do
			end do
		end do
	end function sum_a3

	!===========!
	!= L2 Norm =!
	!===========!

	function norm2_a1(u) result(o)
		type(ad_t),dimension(:),intent(in)::u
		type(ad_t)::o
		
		o = sqrt(sum( u**2 ))
	end function norm2_a1

	function norm2_a2(u) result(o)
		type(ad_t),dimension(:,:),intent(in)::u
		type(ad_t)::o
		
		o = sqrt(sum( u**2 ))
	end function norm2_a2

	function norm2_a3(u) result(o)
		type(ad_t),dimension(:,:,:),intent(in)::u
		type(ad_t)::o
		
		o = sqrt(sum( u**2 ))
	end function norm2_a3

	!===================!
	!= Matrix Multiply =!
	!===================!

	pure function matmul_a21(u,v) result(o)
		type(ad_t),dimension(:,:),intent(in)::u
		type(ad_t),dimension(:),intent(in)::v
		type(ad_t),dimension(:),allocatable::o
		
		integer,dimension(2)::N
		integer::i
		
		N = shape(u)
		allocate( o(N(1)) )
		
		do i=1,N(1)
			o(i) = sum( u(i,:)*v(:) )
		end do
	end function matmul_a21

	pure function matmul_a22(u,v) result(o)
		type(ad_t),dimension(:,:),intent(in)::u
		type(ad_t),dimension(:,:),intent(in)::v
		type(ad_t),dimension(:,:),allocatable::o
		
		integer,dimension(2)::N,M
		integer::i,j
		
		N = shape(u)
		M = shape(v)
		allocate( o(N(1),M(2)) )
		
		do j=1,M(2)
			do i=1,N(1)
				o(i,j) = sum( u(i,:)*v(:,j) )
			end do
		end do
	end function matmul_a22

	!========!
	!= TDMA =!
	!========!

	function TDMA_sA(A,b) result(x)
		!! Solve a tridiagonal linear algebra problem \( [A]\{x\}=\{b\} \)
		!! @todo
		!! Put behind interface and allow for multiple RHS in another routine
		type(ad_t),dimension(:,:),intent(in)::A
			!! Coefficient matrix with the diagonals in columns \( [A] \)
		type(ad_t),dimension(:),intent(in)::b
			!! Right-hand-side of the system \( \{b\} \)
		type(ad_t),dimension(:),allocatable::x
			!! Problem solution \( \{x\} \)
		
		type(ad_t),dimension(:,:),allocatable::W
		type(ad_t)::r
		integer::N,k
		
		N = size(A,1)
		
		allocate( W(N,-1:+1) )
		
		W(:,-1) = A(:,1)
		W(:, 0) = A(:,2)
		W(:,+1) = A(:,3)
		x       = b
		
		do k=2,N
			r = W(k,-1)/W(k-1,0)
			W(k,-1:0) = W(k,-1:0)-r*W(k-1,0:1)
			x(k) = x(k)-r*x(k-1)
		end do
		
		do k=N-1,1,-1
			r = W(k,1)/W(k+1,0)
			W(k,0:1) = W(k,0:1)-r*W(k+1,-1:0)
			x(k) = x(k)-r*x(k+1)
		end do
		
		do k=1,N
			r = 1.0_wp/W(k,0)
			W(k,:) = r*W(k,:)
			x(k) = r*x(k)
		end do
	end function TDMA_sA

	function TDMA_mA(A,b) result(x)
		!! Solve a tridiagonal linear algebra problem \( [A]\{x\}=\{b\} \)
		!! @todo
		!! Put behind interface and allow for multiple RHS in another routine
		type(ad_t),dimension(:,:),intent(in)::A
			!! Coefficient matrix with the diagonals in columns \( [A] \)
		type(ad_t),dimension(:,:),intent(in)::b
			!! Right-hand-side of the system \( \{b\} \)
		type(ad_t),dimension(:,:),allocatable::x
			!! Problem solution \( \{x\} \)
		
		type(ad_t),dimension(:,:),allocatable::W
		type(ad_t)::r
		integer::N,k
		
		N = size(A,1)
		
		allocate( W(N,-1:+1) )
		
		W(:,-1) = A(:,1)
		W(:, 0) = A(:,2)
		W(:,+1) = A(:,3)
		x       = b
		
		do k=2,N
			r = W(k,-1)/W(k-1,0)
			W(k,-1:0) = W(k,-1:0)-r*W(k-1,0:1)
			x(k,:) = x(k,:)-r*x(k-1,:)
		end do
		
		do k=N-1,1,-1
			r = W(k,1)/W(k+1,0)
			W(k,0:1) = W(k,0:1)-r*W(k+1,-1:0)
			x(k,:) = x(k,:)-r*x(k+1,:)
		end do
		
		do k=1,N
			r = 1.0_wp/W(k,0)
			W(k,:) = r*W(k,:)
			x(k,:) = r*x(k,:)
		end do
	end function TDMA_mA

	!====================!
	!= solveLU Routines =!
	!====================!

	subroutine decomposeLU(A,LU,p)
		!! Shamelessly adapted from Rosetta Code
		!!
		!! L -> j<i , L(i,i) = 1
		!! U -> j>=i
		type(ad_t),dimension(:,:),intent(in)::A
		type(ad_t),dimension(:,:),allocatable,intent(inout):: LU
		integer,dimension(:),allocatable,intent(inout)::p
		integer::N,j,i,m
		
		LU = A
		
		N = size(A,1)
		p = [( i , i=1,N )]
		do i=1,N-1
			m = maxloc( abs( LU(p(i:),i) ) , 1 ) + (i-1)
			
			if(m/=i) p([i,m]) = p([m,i])
			
			LU(p(i+1:),i) = LU(p(i+1:),i) / LU(p(i),i)
			
			forall(j=i+1:N) LU(p(i+1:),j) = LU(p(i+1:),j) - LU(p(i+1:),i) * LU(p(i),j)
		end do
		
		LU = LU(p,:)
	end subroutine decomposeLU

	function applyLU(LU,p,b) result(x)
		type(ad_t),dimension(:,:),intent(in)::LU
		integer,dimension(:),intent(in)::p
		type(ad_t),dimension(:),intent(in)::b
		type(ad_t),dimension(:),allocatable::x
		
		type(ad_t),dimension(:),allocatable::pb
		type(ad_t),dimension(:),allocatable::r
		integer::N,i
		
		N  = size(b)
		allocate(r(N),x(N),pb(N))
		r  = ad_t(0.0_wp,size(b(1)%grad()))
		x  = ad_t(0.0_wp,size(b(1)%grad()))
		pb = b(p)
		
		! L.r=pb
		r(1) = pb(1)
		do i=2,N,+1
			r(i) = pb(i)-sum(LU(i,1:i-1)*r(1:i-1))
		end do
		
		! U.x=r
		x(N) = r(N)/LU(N,N)
		do i=N-1,1,-1
			x(i) = ( r(i)-sum(LU(i,i+1:N)*x(i+1:N)) )/LU(i,i)
		end do
	end function applyLU

	function solveLU_s(A,b) result(x)
		type(ad_t),dimension(:,:),intent(in)::A
		type(ad_t),dimension(:),intent(in)::b
		type(ad_t),dimension(:),allocatable::x
		
		type(ad_t),dimension(:,:),allocatable::LU
		integer,dimension(:),allocatable::p
		
		call decomposeLU(A,LU,p)
		
		x = applyLU(LU,p,b)
	end function solveLU_s

	function solveLU_m(A,b) result(x)
		type(ad_t),dimension(:,:),intent(in)::A
		type(ad_t),dimension(:,:),intent(in)::b
		type(ad_t),dimension(:,:),allocatable::x
		
		type(ad_t),dimension(:,:),allocatable::LU
		integer,dimension(:),allocatable::p
		integer::k
		
		call decomposeLU(A,LU,p)
		
		allocate( x(size(b,1),size(b,2)) )
		
		do k=1,size(b,2)
			x(:,k) = applyLU(LU,p,b(:,k))
		end do
	end function solveLU_m

end module autoDiffArray_mod 
