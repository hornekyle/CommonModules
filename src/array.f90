module array_mod
	use kinds_mod
	implicit none
	
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
	
contains

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

	function span_1(x) result(o)
		real(wp),dimension(:),intent(in)::x
		real(wp)::o
		
		o = maxval(x)-minval(x)
	end function span_1

	function span_2(x) result(o)
		real(wp),dimension(:,:),intent(in)::x
		real(wp)::o
		
		o = maxval(x)-minval(x)
	end function span_2

	function span_3(x) result(o)
		real(wp),dimension(:,:,:),intent(in)::x
		real(wp)::o
		
		o = maxval(x)-minval(x)
	end function span_3

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
		
		o = [( (h-l)*real(i-1,wp)/real(Nl-1,wp)+l , i=1 , Nl )]
	end function linspace

	function flatten_2(A) result(o)
		real(wp),dimension(:,:),intent(in)::A
		real(wp),dimension(:),allocatable::o
		
		o = reshape(A,[size(A)])
	end function flatten_2

	function flatten_3(A) result(o)
		real(wp),dimension(:,:,:),intent(in)::A
		real(wp),dimension(:),allocatable::o
		
		o = reshape(A,[size(A)])
	end function flatten_3

	function meshGridX(x,y) result(o)
		real(wp),dimension(:),intent(in)::x,y
		real(wp),dimension(:,:),allocatable::o
		
		integer::Nx,Ny
		integer::i,j
		
		Nx = size(x)
		Ny = size(y)
		
		allocate(o(Nx,Ny))
		
		forall(i=1:Nx,j=1:Ny) o(i,j) = x(i)
	end function meshGridX

	function meshGridY(x,y) result(o)
		real(wp),dimension(:),intent(in)::x,y
		real(wp),dimension(:,:),allocatable::o
		
		integer::Nx,Ny
		integer::i,j
		
		Nx = size(x)
		Ny = size(y)
		
		allocate(o(Nx,Ny))
		
		forall(i=1:Nx,j=1:Ny) o(i,j) = y(j)
	end function meshGridY

	function mean(d) result(o)
		real(wp),dimension(:),intent(in)::d
		real(wp)::o
		
		o = sum(d)/real(size(d),wp)
	end function mean

	function standardDeviation(d) result(o)
		real(wp),dimension(:),intent(in)::d
		real(wp)::o
		
		o = sqrt(sum((d-mean(d))**2)/real(size(d)-1,wp))
	end function standardDeviation

end module array_mod
