module matio_mod
	!! Module to write MATLAB's mat file (version 4 only)
	use iso_c_binding
	implicit none

	interface v2m
		!! Convert a vector into a matrix
		module procedure v2m_i2
		module procedure v2m_i4
		module procedure v2m_r4
		module procedure v2m_r8
		module procedure v2m_z4
		module procedure v2m_z8
	end interface

	interface writeMat
		!! Write a matrix into a file
		module procedure writeMat_i2
		module procedure writeMat_i4
		module procedure writeMat_r4
		module procedure writeMat_r8
		module procedure writeMat_z4
		module procedure writeMat_z8
	end interface

contains

	function v2m_i2(v) result(o)
		integer(c_int16_t),dimension(:),intent(in)::v
		integer(c_int16_t),dimension(size(v),1)::o
		
		o(:,1) = v(:)
	end function v2m_i2
	
	function v2m_i4(v) result(o)
		integer(c_int32_t),dimension(:),intent(in)::v
		integer(c_int32_t),dimension(size(v),1)::o
		
		o(:,1) = v(:)
	end function v2m_i4

	function v2m_r4(v) result(o)
		real(c_float),dimension(:),intent(in)::v
		real(c_float),dimension(size(v),1)::o
		
		o(:,1) = v(:)
	end function v2m_r4

	function v2m_r8(v) result(o)
		real(c_double),dimension(:),intent(in)::v
		real(c_double),dimension(size(v),1)::o
		
		o(:,1) = v(:)
	end function v2m_r8

	function v2m_z4(v) result(o)
		complex(c_float_complex),dimension(:),intent(in)::v
		complex(c_float_complex),dimension(size(v),1)::o
		
		o(:,1) = v(:)
	end function v2m_z4

	function v2m_z8(v) result(o)
		complex(c_double_complex),dimension(:),intent(in)::v
		complex(c_double_complex),dimension(size(v),1)::o
		
		o(:,1) = v(:)
	end function v2m_z8

	subroutine writeMat_i2(fn,an,A,new)
		character(*),intent(in)::fn
		character(*),intent(in)::an
		integer(c_int16_t),dimension(:,:),intent(in)::A
		logical,intent(in),optional::new
		
		integer(c_int32_t),dimension(5)::header
		logical::over
		integer::u
		
		header(1) = 0030
		header(2) = size(A,1)
		header(3) = size(A,2)
		header(4) = 0
		header(5) = len(an)+1
		
		if(present(new)) then
			over = new
		else
			over = .false.
		end if
		
		if(over) then
			open(file=fn,access='stream',form='unformatted',newunit=u,status='replace')
		else
			open(file=fn,access='stream',form='unformatted',newunit=u,status='old',position='append')
		end if
		
		write(u) header
		write(u) an,C_NULL_CHAR
		write(u) A
		
		close(u)
	end subroutine writeMat_i2

	subroutine writeMat_i4(fn,an,A,new)
		character(*),intent(in)::fn
		character(*),intent(in)::an
		integer(c_int32_t),dimension(:,:),intent(in)::A
		logical,intent(in),optional::new
		
		integer(c_int32_t),dimension(5)::header
		logical::over
		integer::u
		
		header(1) = 0020
		header(2) = size(A,1)
		header(3) = size(A,2)
		header(4) = 0
		header(5) = len(an)+1
		
		if(present(new)) then
			over = new
		else
			over = .false.
		end if
		
		if(over) then
			open(file=fn,access='stream',form='unformatted',newunit=u,status='replace')
		else
			open(file=fn,access='stream',form='unformatted',newunit=u,status='old',position='append')
		end if
		
		write(u) header
		write(u) an,C_NULL_CHAR
		write(u) A
		
		close(u)
	end subroutine writeMat_i4

	subroutine writeMat_r4(fn,an,A,new)
		character(*),intent(in)::fn
		character(*),intent(in)::an
		real(c_float),dimension(:,:),intent(in)::A
		logical,intent(in),optional::new
		
		integer(c_int32_t),dimension(5)::header
		logical::over
		integer::u
		
		header(1) = 0010
		header(2) = size(A,1)
		header(3) = size(A,2)
		header(4) = 0
		header(5) = len(an)+1
		
		if(present(new)) then
			over = new
		else
			over = .false.
		end if
		
		if(over) then
			open(file=fn,access='stream',form='unformatted',newunit=u,status='replace')
		else
			open(file=fn,access='stream',form='unformatted',newunit=u,status='old',position='append')
		end if
		
		write(u) header
		write(u) an,C_NULL_CHAR
		write(u) A
		
		close(u)
	end subroutine writeMat_r4

	subroutine writeMat_r8(fn,an,A,new)
		character(*),intent(in)::fn
		character(*),intent(in)::an
		real(c_double),dimension(:,:),intent(in)::A
		logical,intent(in),optional::new
		
		integer(c_int32_t),dimension(5)::header
		logical::over
		integer::u
		
		header(1) = 0000
		header(2) = size(A,1)
		header(3) = size(A,2)
		header(4) = 0
		header(5) = len(an)+1
		
		if(present(new)) then
			over = new
		else
			over = .false.
		end if
		
		if(over) then
			open(file=fn,access='stream',form='unformatted',newunit=u,status='replace')
		else
			open(file=fn,access='stream',form='unformatted',newunit=u,status='old',position='append')
		end if
		
		write(u) header
		write(u) an,C_NULL_CHAR
		write(u) A
		
		close(u)
	end subroutine writeMat_r8

	subroutine writeMat_z4(fn,an,A,new)
		character(*),intent(in)::fn
		character(*),intent(in)::an
		complex(c_float_complex),dimension(:,:),intent(in)::A
		logical,intent(in),optional::new
		
		integer(c_int32_t),dimension(5)::header
		logical::over
		integer::u
		
		header(1) = 0010
		header(2) = size(A,1)
		header(3) = size(A,2)
		header(4) = 1
		header(5) = len(an)+1
		
		if(present(new)) then
			over = new
		else
			over = .false.
		end if
		
		if(over) then
			open(file=fn,access='stream',form='unformatted',newunit=u,status='replace')
		else
			open(file=fn,access='stream',form='unformatted',newunit=u,status='old',position='append')
		end if
		
		write(u) header
		write(u) an,C_NULL_CHAR
		write(u) real(A)
		write(u) imag(A)
		
		close(u)
	end subroutine writeMat_z4

	subroutine writeMat_z8(fn,an,A,new)
		character(*),intent(in)::fn
		character(*),intent(in)::an
		complex(c_double_complex),dimension(:,:),intent(in)::A
		logical,intent(in),optional::new
		
		integer(c_int32_t),dimension(5)::header
		logical::over
		integer::u
		
		header(1) = 0000
		header(2) = size(A,1)
		header(3) = size(A,2)
		header(4) = 1
		header(5) = len(an)+1
		
		if(present(new)) then
			over = new
		else
			over = .false.
		end if
		
		if(over) then
			open(file=fn,access='stream',form='unformatted',newunit=u,status='replace')
		else
			open(file=fn,access='stream',form='unformatted',newunit=u,status='old',position='append')
		end if
		
		write(u) header
		write(u) an,C_NULL_CHAR
		write(u) real(A)
		write(u) imag(A)
		
		close(u)
	end subroutine writeMat_z8

end module matio_mod
