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
	
	!===========!
	!= Exports =!
	!===========!
	
	public::mixval
	public::span
	public::flatten
	
	public::linspace
	public::meshGridX
	public::meshGridY
	
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

	function span_1(x) result(o)
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
		
		o = [( (h-l)*real(i-1,wp)/real(Nl-1,wp)+l , i=1 , Nl )]
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

end module array_mod
