module array_mod
	!! Array reduction and transformation routines
	use kinds_mod
	implicit none
	private
	
	!==============!
	!= Interfaces =!
	!==============!
	
	interface mixval
		!! Return a 2-vector comprising the minimum and maximum values of an array
		module procedure mixval_1
		module procedure mixval_2
		module procedure mixval_3
	end interface
	
	interface span
		!! Return a the maximum-minumum values of an array
		module procedure span_1
		module procedure span_2
		module procedure span_3
	end interface
	
	interface flatten
		!! Reduce an array to one dimension
		module procedure flatten_2
		module procedure flatten_3
	end interface
	
	interface TDMA
		module procedure TDMA_s
		module procedure TDMA_m
	end interface
	
	!===========!
	!= Exports =!
	!===========!
	
	public::mixval
	public::span
	public::flatten
	
	public::deDup
	
	public::linspace
	public::meshGridX
	public::meshGridY
	public::TDMA
	
	public::linearInterp
	
contains

	!====================!
	!= Array Reductions =!
	!====================!

	function mixval_1(x) result(b)
		!! Return [hi,low] for an array
		real(wp),dimension(:),intent(in)::x
			!! Array to find extrema in
		real(wp),dimension(2)::b
		
		b = [minval(x),maxval(x)]
	end function mixval_1

	function mixval_2(x) result(b)
		!! Return [hi,low] for an array
		real(wp),dimension(:,:),intent(in)::x
			!! Array to find extrema in
		real(wp),dimension(2)::b
		
		b = [minval(x),maxval(x)]
	end function mixval_2

	function mixval_3(x) result(b)
		!! Return [hi,low] for an array
		real(wp),dimension(:,:,:),intent(in)::x
			!! Array to find extrema in
		real(wp),dimension(2)::b
		
		b = [minval(x),maxval(x)]
	end function mixval_3

	function span_1(x) result(o)!! x values of data
		!! Return (hi-low) for an array
		real(wp),dimension(:),intent(in)::x
			!! Array of which to find the range
		real(wp)::o
			!! Range of the array
		
		o = maxval(x)-minval(x)
	end function span_1

	function span_2(x) result(o)
		!! Return (hi-low) for an array
		real(wp),dimension(:,:),intent(in)::x
			!! Array of which to find the range
		real(wp)::o
			!! Range of the array
		
		o = maxval(x)-minval(x)
	end function span_2

	function span_3(x) result(o)
		!! Return (hi-low) for an array
		real(wp),dimension(:,:,:),intent(in)::x
			!! Array of which to find the range
		real(wp)::o
			!! Range of the array
		
		o = maxval(x)-minval(x)
	end function span_3

	!=========================!
	!= Array Transformations =!
	!=========================!

	function flatten_2(A) result(o)
		!! Convert a 2D array to 1D
		real(wp),dimension(:,:),intent(in)::A
			!! Array to convert
		real(wp),dimension(:),allocatable::o
			!! 1D version of the data
		
		o = reshape(A,[size(A)])
	end function flatten_2

	function flatten_3(A) result(o)
		!! Convert a 3D array to 1D
		real(wp),dimension(:,:,:),intent(in)::A
			!! Array to convert
		real(wp),dimension(:),allocatable::o
			!! 1D version of the data
		
		o = reshape(A,[size(A)])
	end function flatten_3

	function deDup(l) result(o)
		!! Remove duplicates from a list of positive integers
		integer,dimension(:),intent(in)::l
			!! List for de-duplication
		integer,dimension(:),allocatable::o
			!! List without duplicates
		
		integer,dimension(:),allocatable::b
		integer::T,k
		
		b = l
		do k=1,size(b)
			T = b(k)
			where(b==T) b = -1
			b(k) = T
		end do
		
		o = pack(b,b>0)
	end function deDup

	!=================!
	!= Grid Routines =!
	!=================!

	function linspace(l,h,N) result(o)
		!! Return an array of evenly-spaced values
		real(wp),intent(in)::l
			!! Low-bound for values
		real(wp),intent(in)::h
			!! High-bound for values
		integer,intent(in),optional::N
			!! Number of values (default 20)
		real(wp),dimension(:),allocatable::o
		
		integer::Nl,i
		
		Nl = 20
		if(present(N)) Nl = N
		
		if(Nl==1) then
			o = [ (l+h)/2.0_wp ]
		else
			o = [( (h-l)*real(i-1,wp)/real(Nl-1,wp)+l , i=1 , Nl )]
		end if
	end function linspace

	function meshGridX(x,y) result(o)
		!! Construct a grid from x and y spacing
		real(wp),dimension(:),intent(in)::x,y
			!! Grid values in x and y
		real(wp),dimension(:,:),allocatable::o
			!! Array x expanded into y
		
		integer::Nx,Ny
		integer::i,j
		
		Nx = size(x)
		Ny = size(y)
		
		allocate(o(Nx,Ny))
		
		forall(i=1:Nx,j=1:Ny) o(i,j) = x(i)
	end function meshGridX

	function meshGridY(x,y) result(o)
		!! Construct a grid from x and y spacing
		real(wp),dimension(:),intent(in)::x,y
			!! Grid values in x and y
		real(wp),dimension(:,:),allocatable::o
			!! Array x expanded into y
		
		integer::Nx,Ny
		integer::i,j
		
		Nx = size(x)
		Ny = size(y)
		
		allocate(o(Nx,Ny))
		
		forall(i=1:Nx,j=1:Ny) o(i,j) = y(j)
	end function meshGridY

	!========!
	!= TMDA =!
	!========!

	function TDMA_s(A,b) result(x)
		!! Solve a tridiagonal linear algebra problem \( [A]\{x\}=\{b\} \)
		!! @todo
		!! Put behind interface and allow for multiple RHS in another routine
		real(wp),dimension(:,:),intent(in)::A
			!! Coefficient matrix with the diagonals in columns \( [A] \)
		real(wp),dimension(:),intent(in)::b
			!! Right-hand-side of the system \( \{b\} \)
		real(wp),dimension(:),allocatable::x
			!! Problem solution \( \{x\} \)
		
		real(wp),dimension(:,:),allocatable::W
		real(wp)::r
		integer::N,k
		
		N = size(A,1)
		
		allocate( W(N,-1:+1) )
		
		W(:,-1) =  A(:,1)
		W(:, 0) =  A(:,2)
		W(:,+1) = -A(:,3)
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
	end function TDMA_s

	function TDMA_m(A,b) result(x)
		!! Solve a tridiagonal linear algebra problem \( [A]\{x\}=\{b\} \)
		!! @todo
		!! Put behind interface and allow for multiple RHS in another routine
		real(wp),dimension(:,:),intent(in)::A
			!! Coefficient matrix with the diagonals in columns \( [A] \)
		real(wp),dimension(:,:),intent(in)::b
			!! Right-hand-side of the system \( \{b\} \)
		real(wp),dimension(:,:),allocatable::x
			!! Problem solution \( \{x\} \)
		
		real(wp),dimension(:,:),allocatable::W
		real(wp)::r
		integer::N,k
		
		N = size(A,1)
		
		allocate( W(N,-1:+1) )
		
		W(:,-1) =  A(:,1)
		W(:, 0) =  A(:,2)
		W(:,+1) = -A(:,3)
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
	end function TDMA_m

	!========================!
	!= Linear Interpolation =!
	!========================!

	function linearInterp(r,x,y) result(o)
		!! Linear interpolation of y(x) at x=r
		!!
		!! Arrays x and y must be sorted
		real(wp),intent(in)::r
			!! Position of desired value
		real(wp),dimension(:),intent(in)::x
			!! x values of data
		real(wp),dimension(size(x)),intent(in)::y
			!! y values of data
		real(wp)::o
			!! y(x=r) via linear interpolation
		
		integer,dimension(2)::rng
		real(wp)::phi
		
		rng = getRange(r,x)
		phi = (r-x(rng(1)))/(x(rng(2))-x(rng(1)))
		o = y(rng(1))+phi*(y(rng(2))-y(rng(1)))
		
	contains
		
		function getRange(r,x) result(o)
			!! Find the locations in x that bracket r
			!!
			!! Use a quicksearch to find data
			real(wp),intent(in)::r
				!! Position of desired value
			real(wp),dimension(:),intent(in)::x
				!! x values of data
			integer,dimension(2)::o
				!! Indexes of x values that bracket r
			
			integer::N
			integer::L,M,H
			integer::MN
			
			N = size(x)
			
			if(r<x(2)) then
				o = [1,2]
				return
			else if(r>x(N-1)) then
				o = [N-1,N]
				return
			end if
			
			L = 2
			H = N-1
			M = (L+H)/2+1
			
			do
				if(x(M)<=r) then
					L = M
				else
					H = M
				end if
				MN = (L+H)/2+1
				if(MN==M) then
					o = [M,M+1]
					return
				end if
				M = MN
			end do
		end function getRange
		
	end function linearInterp

end module array_mod
