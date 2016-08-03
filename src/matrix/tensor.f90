module tensor_mod
	!! Module for working with tensors
	!! @note
	!! Array size must be specific (either 2 or 3) for this to work!
	use kinds_mod
	implicit none
	private
	
	!==============!
	!= Interfaces =!
	!==============!
	
	interface operator(.o.)
		!! Dot product
		module procedure dot_vv ! scalar
		module procedure dot_vT ! vector (matmul)
		module procedure dot_Tv ! vector (matmul)
		module procedure dot_TT ! tensor (matmul)
	end interface
	
	interface operator(.x.)
		!! Cross product
		module procedure cross_vv  ! vector
	end interface
	
	interface operator(.sx.)
		!! Cross product of 2D vectors
		module procedure cross_vv2 ! scalar (z component)
	end interface
	
	interface operator(.d.)
		!! Dyadic product
		module procedure dyadic_vv ! tensor
	end interface
	
	interface operator(.oo.)
		!! Double dot product
		module procedure ddot_TT ! scalar
	end interface
	
	!===========!
	!= Exports =!
	!===========!
	
	public::operator(.o.)
	public::operator(.x.)
	public::operator(.sx.)
	public::operator(.d.)
	public::operator(.oo.)
	
contains

	!================!
	!= Dot Routines =!
	!================!

	pure function dot_vv(u,v) result(o)
		!! Dot product of two vectors
		real(wp),dimension(3),intent(in)::u
			!! First vector
		real(wp),dimension(3),intent(in)::v
			!! Second vector
		real(wp)::o
			!! Result
		
		o = sum(u*v)
	end function dot_vv

	pure function dot_vT(u,T) result(o)
		!! Dot product (left) of a vector and a tensor
		real(wp),dimension(3),intent(in)::u
			!! Vector
		real(wp),dimension(3,3),intent(in)::T
			!! Tensor
		real(wp),dimension(3)::o
			!! Result
		
		o = matmul(u,T)
	end function dot_vT

	pure function dot_Tv(T,v) result(o)
		!! Dot product (right) of a tensor and a vector
		real(wp),dimension(3,3),intent(in)::T
			!! Tensor
		real(wp),dimension(3),intent(in)::v
			!! Vector
		real(wp),dimension(3)::o
			!! Result
		
		o = matmul(T,v)
	end function dot_Tv

	pure function dot_TT(U,V) result(o)
		!! Dot product of two tensors
		real(wp),dimension(3,3),intent(in)::U
			!! First tensor
		real(wp),dimension(3,3),intent(in)::V
			!! Second tensor
		real(wp),dimension(3,3)::o
			!! Result
		
		o = matmul(U,V)
	end function dot_TT

	!==================!
	!= Cross Routines =!
	!==================!

	pure function cross_vv2(u,v) result(o)
		!! Cross product of two 2D vectors
		real(wp),dimension(2),intent(in)::u
			!! First vector
		real(wp),dimension(2),intent(in)::v
			!! Second vector
		real(wp)::o
			!! Result z-component

		o = u(1)*v(2)-u(2)*v(1)
	end function cross_vv2

	pure function cross_vv(u,v) result(o)
		!! Cross product of two vectors
		real(wp),dimension(3),intent(in)::u
			!! First vector
		real(wp),dimension(3),intent(in)::v
			!! Second vector
		real(wp),dimension(3)::o
			!! Result

		o = u([2,3,1])*v([3,1,2])-u([3,1,2])*v([2,3,1])
	end function cross_vv

	!===================!
	!= Dyadic Routines =!
	!===================!

	pure function dyadic_vv(u,v) result(o)
		!! Dyadic (tensor) product of two vectors
		real(wp),dimension(3),intent(in)::u
			!! First vector
		real(wp),dimension(3),intent(in)::v
			!! Second vector
		real(wp),dimension(3,3)::o
			!! Result
		
		integer::i,j
		
		forall(i=1:3,j=1:3) o(i,j) = u(i)*v(j)
	end function dyadic_vv

	!=======================!
	!= Double Dot Routines =!
	!=======================!

	pure function ddot_TT(U,V) result(o)
		!! Double dot product of two tensors
		real(wp),dimension(3,3),intent(in)::U
			!! Fisrt tensor
		real(wp),dimension(3,3),intent(in)::V
			!! Second vector
		real(wp),dimension(3,3)::o
			!! Result
		
		o = sum(U*V)
	end function ddot_TT

end module tensor_mod