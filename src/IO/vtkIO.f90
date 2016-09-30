module vtkIO_mod
	!! Module to write structured VTK files
	use kinds_mod
	implicit none
	private
	
	!==============!
	!= Interfaces =!
	!==============!
	
	interface writeGridVTK
		!! Write the structured coordinates
		module procedure writeGridVTK_1d
		module procedure writeGridVTK_2d
		module procedure writeGridVTK_3d
	end interface
	
	interface writeVectorVTK
		!! Write a structured vector field
		module procedure writeVectorVTK_1d
		module procedure writeVectorVTK_2d
		module procedure writeVectorVTK_3d
	end interface
	
	interface writeScalarVTK
		!! Write a structured scalar field
		module procedure writeScalarVTK_1d
		module procedure writeScalarVTK_2d
		module procedure writeScalarVTK_3d
	end interface
	
	!===========!
	!= Exports =!
	!===========!
	
	public::writeHeaderVTK
	public::writeGridVTK
	public::writeVectorVTK
	public::writeScalarVTK
	
	! Kinds
	public::wp
	
contains

	subroutine writeHeaderVTK(u,title)
		integer,intent(in)::u
		character(*),intent(in)::title
	
		write(u,"(A)") "# vtk DataFile Version 2.0"
		write(u,"(A)") title
		write(u,"(A)") "ASCII"
		write(u,"(A)") ""
	end subroutine writeHeaderVTK

	subroutine writeGridVTK_1d(u,x)
		integer,intent(in)::u
		real(wp),dimension(:),intent(in)::x
		integer::k1
		
		write(u,"(A)") "DATASET STRUCTURED_GRID"
		write(u,"(A,3(I5,X))") "DIMENSIONS",size(x,1),1,1
		write(u,"(A,I10,A)") "POINTS",size(x)," double"
		do k1=lbound(x,1),ubound(x,1)
			write(u,"(3(ES28.15E4,X))") x(k1),0.0_wp,0.0_wp
		end do
		write(u,"(A)") ""
		write(u,"(A,I10)") "POINT_DATA",size(x)
	end subroutine writeGridVTK_1d

	subroutine writeGridVTK_2d(u,x,y)
		integer,intent(in)::u
		real(wp),dimension(:,:),intent(in)::x,y
		integer::k1,k2
		
		write(u,"(A)") "DATASET STRUCTURED_GRID"
		write(u,"(A,3(I5,X))") "DIMENSIONS",size(x,1),size(x,2),1
		write(u,"(A,I10,A)") "POINTS",size(x)," double"
		do k2=lbound(x,2),ubound(x,2)
			do k1=lbound(x,1),ubound(x,1)
				write(u,"(3(ES28.15E4,X))") x(k1,k2),y(k1,k2),0.0_wp
			end do
		end do
		write(u,"(A)") ""
		write(u,"(A,I10)") "POINT_DATA",size(x)
	end subroutine writeGridVTK_2d

	subroutine writeVectorVTK_1d(u1,vname,u,v)
		integer,intent(in)::u1
		character(*),intent(in)::vname
		real(wp),dimension(:),intent(in)::u,v
		integer::k1
		
		write(u1,"(A,A,A)") "VECTORS ",vname," double"
		do k1=lbound(u,1),ubound(u,1)
			write(u1,"(3(ES28.15E4,X))") u(k1),v(k1),0.0_wp
		end do
	end subroutine writeVectorVTK_1d

	subroutine writeVectorVTK_2d(u1,vname,u,v)
		integer,intent(in)::u1
		character(*),intent(in)::vname
		real(wp),dimension(:,:),intent(in)::u,v
		integer::k1,k2
		
		write(u1,"(A,A,A)") "VECTORS ",vname," double"
		do k2=lbound(u,2),ubound(u,2)
			do k1=lbound(u,1),ubound(u,1)
				write(u1,"(3(ES28.15E4,X))") u(k1,k2),v(k1,k2),0.0_wp
			end do
		end do
	end subroutine writeVectorVTK_2d

	subroutine writeScalarVTK_1d(u,sname,phi)
		integer,intent(in)::u
		character(*),intent(in)::sname
		real(wp),dimension(:),intent(in)::phi
		integer::k1
		
		write(u,"(A,A,A)") "SCALARS ",sname," double"
		write(u,"(A)") "LOOKUP_TABLE default"
		do k1=lbound(phi,1),ubound(phi,1)
			write(u,"(ES28.15E4)") phi(k1)
		end do
	end subroutine writeScalarVTK_1d

	subroutine writeScalarVTK_2d(u,sname,phi)
		integer,intent(in)::u
		character(*),intent(in)::sname
		real(wp),dimension(:,:),intent(in)::phi
		integer::k1,k2
		
		write(u,"(A,A,A)") "SCALARS ",sname," double"
		write(u,"(A)") "LOOKUP_TABLE default"
		do k2=lbound(phi,2),ubound(phi,2)
			do k1=lbound(phi,1),ubound(phi,1)
				write(u,"(ES28.15E4)") phi(k1,k2)
			end do
		end do
	end subroutine writeScalarVTK_2d

	subroutine writeGridVTK_3d(u,x,y,z)
		integer,intent(in)::u
		real(wp),dimension(:,:,:),intent(in)::x,y,z
		integer::k1,k2,k3
		
		write(u,"(A)") "DATASET STRUCTURED_GRID"
		write(u,"(A,3(I5,X))") "DIMENSIONS",size(x,1),size(x,2),size(x,3)
		write(u,"(A,I10,A)") "POINTS",size(x)," double"
		do k3=lbound(x,3),ubound(x,3)
			do k2=lbound(x,2),ubound(x,2)
				do k1=lbound(x,1),ubound(x,1)
					write(u,"(3(ES28.15E4,X))") x(k1,k2,k3),y(k1,k2,k3),z(k1,k2,k3)
				end do
			end do
		end do
		write(u,"(A)") ""
		write(u,"(A,I10)") "POINT_DATA",size(x)
	end subroutine writeGridVTK_3d

	subroutine writeVectorVTK_3d(u1,vname,u,v,w)
		integer,intent(in)::u1
		character(*),intent(in)::vname
		real(wp),dimension(:,:,:),intent(in)::u,v,w
		integer::k1,k2,k3
		
		write(u1,"(A,A,A)") "VECTORS ",vname," double"
		do k3=lbound(u,3),ubound(u,3)
			do k2=lbound(u,2),ubound(u,2)
				do k1=lbound(u,1),ubound(u,1)
					write(u1,"(3(ES28.15E4,X))") u(k1,k2,k3),v(k1,k2,k3),w(k1,k2,k3)
				end do
			end do
		end do
	end subroutine writeVectorVTK_3d

	subroutine writeScalarVTK_3d(u,sname,phi)
		integer,intent(in)::u
		character(*),intent(in)::sname
		real(wp),dimension(:,:,:),intent(in)::phi
		integer::k1,k2,k3
		
		write(u,"(A,A,A)") "SCALARS ",sname," double"
		write(u,"(A)") "LOOKUP_TABLE default"
		do k3=lbound(phi,3),ubound(phi,3)
			do k2=lbound(phi,2),ubound(phi,2)
				do k1=lbound(phi,1),ubound(phi,1)
					write(u,"(ES28.15E4)") phi(k1,k2,k3)
				end do
			end do
		end do
	end subroutine writeScalarVTK_3d

end module vtkIO_mod
