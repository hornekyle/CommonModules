! Requires lapack and umfpack !

module linalg_mod
	use kinds_mod
	use iso_c_binding
	implicit none
	private

	interface
		subroutine dgesv(N,NHRS,A,LDA,IPIV,B,LDB,INFO)
			use kinds_mod
			integer::N
			integer::NRHS
			real(dp),dimension(LDA,*)::A
			integer::LDA
			integer,dimension(*)::IPIV
			real(dp),dimension(LDB,*)::B
			integer::LDB
			integer::INFO
		end subroutine dgesv

		subroutine zgesv(N,NHRS,A,LDA,IPIV,B,LDB,INFO)
			use kinds_mod
			integer::N
			integer::NRHS
			complex(dp),dimension(LDA,*)::A
			integer::LDA
			integer,dimension(*)::IPIV
			complex(dp),dimension(LDB,*)::B
			integer::LDB
			integer::INFO
		end subroutine zgesv

		subroutine dgeev(JOBVL,JOBVR,N,A,LDA,WR,WI,VL,LDVL,VR,LDVR,WORK,LWORK,INFO)
			use kinds_mod
			character(1),intent(in)::JOBVL,JOBVR
			integer,intent(in)::LDA,LDVL,LDVR,N
			real(dp),dimension(LDA,*),intent(inout)::A
			real(dp),dimension(LDVL,*),intent(out)::VL
			real(dp),dimension(LDVR,*),intent(out)::VR
			real(dp),dimension(*),intent(out)::WR,WI
			real(dp),dimension(*),intent(out)::WORK
			integer,intent(in)::LWORK
			integer,intent(out)::INFO
		end subroutine dgeev

		subroutine zgeev(JOBVL,JOBVR,N,A,LDA,W,VL,LDVL,VR,LDVR,WORK,LWORK,RWORK,INFO)
			use kinds_mod
			character(1),intent(in)::JOBVL,JOBVR
			integer,intent(in)::LDA,LDVL,LDVR,N
			complex(dp),dimension(LDA,*),intent(inout)::A
			complex(dp),dimension(LDVL,*),intent(out)::VL
			complex(dp),dimension(LDVR,*),intent(out)::VR
			complex(dp),dimension(*),intent(out)::W
			complex(dp),dimension(*),intent(out)::WORK
			real(dp),dimension(*),intent(out)::RWORK
			integer,intent(in)::LWORK
			integer,intent(out)::INFO
		end subroutine zgeev

		subroutine umfpack_dsolve(i,j,A,b,x,Nij,Nx) bind(C,name='umfpack_dsolve')
			use iso_c_binding
			integer(c_int),value::Nij,Nx
			integer(c_int),dimension(Nij),intent(in)::i,j
			real(c_double),dimension(Nij),intent(in)::A
			real(c_double),dimension(Nx),intent(in)::b
			real(c_double),dimension(Nx),intent(out)::x
		end subroutine umfpack_dsolve
		
		subroutine umfpack_zsolve(i,j,Ar,Ai,br,bi,xr,xi,Nij,Nx) bind(C,name='umfpack_zsolve')
			use iso_c_binding
			integer(c_int),value::Nij,Nx
			integer(c_int),dimension(Nij),intent(in)::i,j
			real(c_double),dimension(Nij),intent(in)::Ar,Ai
			real(c_double),dimension(Nx),intent(in)::br,bi
			real(c_double),dimension(Nx),intent(out)::xr,xi
		end subroutine umfpack_zsolve
	end interface

	interface directSolve
		module procedure full_direct_d
		module procedure full_direct_z
		module procedure banded_direct_r
		module procedure banded_direct_c
		module procedure sparse_direct_d
		module procedure sparse_direct_z
	end interface
	
	interface tdma
		module procedure tdma_r
		module procedure tdma_c
	end interface

	interface inverse
		module procedure inverse_d
		module procedure inverse_z
	end interface
	
	interface matvec
		module procedure matvec_r
		module procedure matvec_c
	end interface
	
	interface eigen
		module procedure eigen_d
		module procedure eigen_z
	end interface

	interface spectralRadius
		module procedure spectral_radius_full_r
		module procedure spectral_radius_sparse_r
	end interface

	interface sparse2Full
		module procedure sparse2full_r
		module procedure sparse2full_c
	end interface

	interface det
		module procedure det_r
		module procedure det_c
	end interface

	public::directSolve
	public::tdma
	public::inverse
	public::det
	public::spectralRadius
	public::matvec
	public::eigen
	public::identity
	public::sparse2Full

contains

	function full_direct_d(A,b) result(x)
		real(dp),dimension(:,:),intent(in)::A
		real(dp),dimension(:),intent(in)::b
		real(dp),dimension(size(b))::x
		
		real(dp),dimension(size(A,1),size(A,2))::WA
		real(dp),dimension(size(b),1)::Wb
		integer,dimension(size(A,2))::IPIV
		integer::info
		
		WA = A
		WB(:,1) = b
		call dgesv(size(A,2),1,WA,size(A,1),IPIV,WB,size(B),info)
		x = WB(:,1)
	end function full_direct_d

	function full_direct_z(A,b) result(x)
		complex(dp),dimension(:,:),intent(in)::A
		complex(dp),dimension(:),intent(in)::b
		complex(dp),dimension(size(b))::x
		
		complex(dp),dimension(size(A,1),size(A,2))::WA
		complex(dp),dimension(size(b),1)::Wb
		integer,dimension(size(A,2))::IPIV
		integer::info
		
		WA = A
		WB(:,1) = b
		call zgesv(size(A,2),1,WA,size(A,1),IPIV,WB,size(B),info)
		x = WB(:,1)
	end function full_direct_z

	function banded_direct_r(A,b,LB) result(x)
		integer,dimension(2),intent(in)::LB
		real(wp),dimension(LB(1):,LB(2):),intent(inout)::A
		real(wp),dimension(size(A,2)),intent(in)::b
		real(wp),dimension(size(A,2))::x
		
		integer::N,W,i,j
		real(wp)::r
		
		N = size(A,2)
		W = (size(A,1)-1)/2
		x = b
		do i=1,N
			do j=1,min(W,N-i)
				r = A(-j,i+j)/A(0,i)
				A(-j:W-j,i+j) = A(-j:W-j,i+j)-r*A(0:W,i)
				x(i+j) = x(i+j)-r*x(i)
			end do
		end do
		
		do i=N,1,-1
			do j=1,min(i-1,W)
				r = A(j,i-j)/A(0,i)
				A(j,i-j) = A(j,i-j)-r*A(0,i)
				x(i-j) = x(i-j)-r*x(i)
			end do
		end do

		do i=1,N
			x(i) = x(i)/A(0,i)
		end do
	end function banded_direct_r

	function banded_direct_c(A,b,LB) result(x)
		integer,dimension(2),intent(in)::LB
		complex(wp),dimension(LB(1):,LB(2):),intent(inout)::A
		complex(wp),dimension(size(A,2)),intent(in)::b
		complex(wp),dimension(size(A,2))::x
		
		integer::N,W,i,j
		complex(wp)::r
		
		N = size(A,2)
		W = (size(A,1)-1)/2
		x = b
		do i=1,N
			do j=1,min(W,N-i)
				r = A(-j,i+j)/A(0,i)
				A(-j:W-j,i+j) = A(-j:W-j,i+j)-r*A(0:W,i)
				x(i+j) = x(i+j)-r*x(i)
			end do
		end do
		
		do i=N,1,-1
			do j=1,min(i-1,W)
				r = A(j,i-j)/A(0,i)
				A(j,i-j) = A(j,i-j)-r*A(0,i)
				x(i-j) = x(i-j)-r*x(i)
			end do
		end do

		do i=1,N
			x(i) = x(i)/A(0,i)
		end do
	end function banded_direct_c

	function sparse_direct_d(i,j,A,b) result(x)
		real(dp),dimension(:),intent(in)::A
		integer(c_int),dimension(size(A)),intent(inout)::i
		integer(c_int),dimension(size(A)),intent(inout)::j
		real(dp),dimension(:),intent(in)::b
		real(dp),dimension(:),allocatable::x

		allocate(x(size(b)))

		i = i-1
		j = j-1
		call umfpack_dsolve(i,j,A,b,x,size(i),size(b))
		i = i+1
		j = j+1
	end function sparse_direct_d

	function sparse_direct_z(i,j,A,b) result(x)
		complex(dp),dimension(:),intent(in)::A
		integer(c_int),dimension(size(A)),intent(inout)::i
		integer(c_int),dimension(size(A)),intent(inout)::j
		complex(dp),dimension(:),intent(in)::b
		complex(dp),dimension(:),allocatable::x
		
		real(dp),dimension(:),allocatable::Ar,Ai
		real(dp),dimension(:),allocatable::br,bi
		real(dp),dimension(:),allocatable::xr,xi
		
		allocate(x(size(b)))
		
		allocate(Ar(size(A)),Ai(size(A)))
		allocate(br(size(b)),bi(size(b)))
		allocate(xr(size(b)),xi(size(b)))
		
		Ar = real(A)
		Ai = aimag(A)
		br = real(b)
		bi = aimag(b)
		
		i = i-1
		j = j-1
		call umfpack_zsolve(i-1,j-1,Ar,Ai,br,bi,xr,xi,size(i),size(b))
		i = i+1
		j = j+1
		
		x = xr+(0.0_dp,1.0_dp)*xi
	end function sparse_direct_z
	
	function tdma_r(A,b) result(x)
		real(wp),dimension(:),intent(in)::b
		real(wp),dimension(size(b),3),intent(in)::A
		
		real(wp),dimension(size(b))::x
		real(wp),dimension(size(b),3)::W
		real(wp)::r
		integer::N,k
		
		N = size(b)
		x = b
		W = A
		
		do k=1,N-1
			r = W(k+1,1)/W(k,2)
			x(k+1) = x(k+1)-r*x(k)
			W(k+1,1:2) = W(k+1,1:2)-r*W(k,2:3)
		end do
		x(N) = x(N)/W(N,2)
		do k=N-1,1,-1
			x(k) = (x(k)-W(k,3)*x(k+1))/W(k,2)
		end do
	end function tdma_r

	function tdma_c(A,b) result(x)
		complex(wp),dimension(:),intent(in)::b
		complex(wp),dimension(size(b),3),intent(in)::A
		
		complex(wp),dimension(size(b))::x
		complex(wp),dimension(size(b),3)::W
		complex(wp)::r
		integer::N,k
		
		N = size(b)
		x = b
		W = A
		
		do k=1,N-1
			r = W(k+1,1)/W(k,2)
			x(k+1) = x(k+1)-r*x(k)
			W(k+1,1:2) = W(k+1,1:2)-r*W(k,2:3)
		end do
		x(N) = x(N)/W(N,2)
		do k=N-1,1,-1
			x(k) = (x(k)-W(k,3)*x(k+1))/W(k,2)
		end do
	end function tdma_c

	function inverse_d(A) result(o)
		real(dp),dimension(:,:),intent(in)::A
		real(dp),dimension(size(A,1),size(A,2))::o
		
		real(dp),dimension(size(A,1),size(A,2))::WA
		integer,dimension(size(A,2))::IPIV
		integer::info
		integer::k
		
		WA = A
		o(:,:) = 0.0_dp
		forall(k=1:size(o,1)) o(k,k) = 1.0_dp
		call dgesv(size(A,2),size(A,2),WA,size(A,1),IPIV,o,size(A,1),info)
	end function inverse_d

	function inverse_z(A) result(o)
		complex(dp),dimension(:,:),intent(in)::A
		complex(dp),dimension(size(A,1),size(A,2))::o
		
		complex(dp),dimension(size(A,1),size(A,2))::WA
		integer,dimension(size(A,2))::IPIV
		integer::info
		integer::k
		
		WA = A
		o(:,:) = 0.0_dp
		forall(k=1:size(o,1)) o(k,k) = 1.0_dp
		call zgesv(size(A,2),size(A,2),WA,size(A,1),IPIV,o,size(A,1),info)
	end function inverse_z

	recursive function det_r(A) result(d)
		real(wp),dimension(:,:),intent(in)::A
		real(wp)::d
		
		integer::k
		real(wp),dimension(size(A,1)-1,size(A,2)-1)::CM
		
		if(size(A,1)==2) then
			d = A(1,1)*A(2,2)-A(2,1)*A(1,2)
			return
		end if
		
		d = 0.0
		do k=1,size(A,2)
			CM(:,1:k-1) = A(2:,1:k-1)
			CM(:,k:) = A(2:,k+1:)
			d = d+A(1,k)*(-1.0_wp)**(k+1)*det_r(CM)
		end do
	end function det_r

	recursive function det_c(A) result(d)
		complex(wp),dimension(:,:),intent(in)::A
		complex(wp)::d
		
		integer::k
		complex(wp),dimension(size(A,1)-1,size(A,2)-1)::CM
		
		if(size(A,1)==2) then
			d = A(1,1)*A(2,2)-A(2,1)*A(1,2)
			return
		end if
		
		d = 0.0
		do k=1,size(A,2)
			CM(:,1:k-1) = A(2:,1:k-1)
			CM(:,k:) = A(2:,k+1:)
			d = d+A(1,k)*(-1.0_wp)**(k+1)*det_c(CM)
		end do
	end function det_c

	function spectral_radius_full_r(A,vi) result(o)
		real(wp),dimension(:,:),intent(in)::A
		real(wp),dimension(size(A,1)),intent(out),optional::vi
		real(wp)::o
		
		real(wp),dimension(size(A,1))::v,vp
		real(wp)::l,lo
		integer::k
		
		v = 1.0_wp
		vp = matmul(A,v)
		lo = maxval(vp)
		v = vp/lo
		do k=1,100
			vp = matmul(A,v)
			l = maxval(vp)
			v = vp/l
			if(abs(l-lo)<128.0_wp*epsilon(1.0_wp)) exit
			lo = l
		end do
		
		o = l
		if(present(vi)) vi = v
	end function spectral_radius_full_r

	function spectral_radius_sparse_r(i,j,A,vi) result(o)
		integer,dimension(:),intent(in)::i
		integer,dimension(size(i)),intent(in)::j
		real(wp),dimension(size(i)),intent(in)::A
		real(wp),dimension(maxval(j)),intent(out),optional::vi
		real(wp)::o
		
		real(wp),dimension(maxval(j))::v,vp
		real(wp)::l,lo
		integer::k
		
		v = 1.0_wp
		vp = matvec_r(i,j,A,v)
		lo = maxval(vp)
		v = vp/lo
		do k=1,100
			vp = matvec_r(i,j,A,v)
			l = maxval(vp)
			v = vp/l
			if(abs(l-lo)<128.0_wp*epsilon(1.0_wp)) exit
			lo = l
		end do
		
		o = l
		if(present(vi)) vi = v
	end function spectral_radius_sparse_r

	function matvec_r(i,j,A,v) result(o)
		integer,dimension(:),intent(in)::i
		integer,dimension(size(i)),intent(in)::j
		real(wp),dimension(size(i)),intent(in)::A
		real(wp),dimension(:),intent(in)::v
		real(wp),dimension(maxval(i))::o
		
		integer::k
		
		o = 0.0_wp
		do k=1,size(i)
			o(i(k)) = o(i(k))+A(k)*v(j(k))
		end do
	end function matvec_r

	function matvec_c(i,j,A,v) result(o)
		integer,dimension(:),intent(in)::i
		integer,dimension(size(i)),intent(in)::j
		complex(wp),dimension(size(i)),intent(in)::A
		complex(wp),dimension(:),intent(in)::v
		complex(wp),dimension(maxval(i))::o
		
		integer::k
		
		o = 0.0_wp
		do k=1,size(i)
			o(i(k)) = o(i(k))+A(k)*v(j(k))
		end do
	end function matvec_c

	function eigen_d(A,V) result(o)
		real(dp),dimension(:,:),intent(in)::A
		real(dp),dimension(size(A,1),size(A,2)),intent(out),optional::V
		complex(dp),dimension(size(A,1))::o
		
		real(dp),dimension(size(A,1),size(A,2))::Al
		real(dp),dimension(size(A,1))::Wr,Wi
		real(dp),dimension(1,1)::VL,VR
		real(dp),dimension(4*size(A,1))::work
		integer::info
		
		Al = A
		
		if(.not.present(V)) then
			call dgeev('N','N',size(A,1),Al,size(A,1),Wr,Wi,VL,1,VR,1,work,size(work),info)
		else
			call dgeev('N','V',size(A,1),Al,size(A,1),Wr,Wi,VL,1,V,size(V,1),work,size(work),info)
		end if
		
		o = Wr+(0.0_dp,1.0_dp)*Wi
	end function eigen_d

	function eigen_z(A,V) result(o)
		complex(dp),dimension(:,:),intent(in)::A
		complex(dp),dimension(size(A,1),size(A,2)),intent(out),optional::V
		complex(dp),dimension(size(A,1))::o
		
		complex(dp),dimension(size(A,1),size(A,2))::Al
		complex(dp),dimension(1,1)::VL,VR
		complex(dp),dimension(4*size(A,1))::work
		real(dp),dimension(2*size(A,1))::rwork
		integer::info
		
		Al = A
		
		if(.not.present(V)) then
			call zgeev('N','N',size(A,1),Al,size(A,1),o,VL,1,VR,1,work,size(work),rwork,info)
		else
			call zgeev('N','V',size(A,1),Al,size(A,1),o,VL,1,V,size(V,1),work,size(work),rwork,info)
		end if
	end function eigen_z

	function identity(N) result(o)
		integer,intent(in)::N
		real(wp),dimension(N,N)::o
		
		integer::k
		
		o = 0.0_wp
		forall(k=1:n) o(k,k) = 1.0_wp
	end function identity

	function sparse2full_r(i,j,A) result(o)
		integer,dimension(:),intent(in)::i
		integer,dimension(size(i)),intent(in)::j
		real(wp),dimension(size(i)),intent(in)::A
		real(wp),dimension(maxval(i),maxval(j))::o
		
		integer::k
		o = 0.0_wp
		do k=1,size(i)
			o(i(k),j(k)) = o(i(k),j(k))+A(k)
		end do
	end function sparse2full_r

	function sparse2full_c(i,j,A) result(o)
		integer,dimension(:),intent(in)::i
		integer,dimension(size(i)),intent(in)::j
		complex(wp),dimension(size(i)),intent(in)::A
		complex(wp),dimension(maxval(i),maxval(j))::o
		
		integer::k
		o = 0.0_wp
		do k=1,size(i)
			o(i(k),j(k)) = o(i(k),j(k))+A(k)
		end do
	end function sparse2full_c

end module linalg_mod
