module netCDF_mod
	!! Module to simplify reading and writing grid data to files
	use kinds_mod
	use netcdf
	implicit none
	private
	
	interface readStep
		!! Read a variable for a timestep from a file
		module procedure readStep_1d
		module procedure readStep_2d
		module procedure readStep_3d
	end interface
	
	interface writeStep
		!! Write a variable for a timestep to a file
		module procedure writeStep_1d
		module procedure writeStep_2d
		module procedure writeStep_3d
	end interface
	
	public::readGrid
	public::readStep
	
	public::writeGrid
	public::writeStep
	
contains

	!==================!
	!= Input Routines =!
	!==================!

	subroutine readGrid(fn,vars,x,y,z,t)
		!! Read a grid from a file
		character(*),intent(in)::fn
			!! Filename
		character(*),dimension(:),allocatable,intent(inout)::vars
			!! List of variable names
		real(dp),dimension(:),allocatable,intent(inout)::x
			!! Grid data from file; allocated if present
		real(dp),dimension(:),allocatable,intent(inout)::y
			!! Grid data from file; allocated if present
		real(dp),dimension(:),allocatable,intent(inout)::z
			!! Grid data from file; allocated if present
		real(dp),dimension(:),allocatable,intent(inout)::t
			!! Grid data from file; allocated if present
		
		integer::f_id
		integer::Nd,Nv,Na,ud_id,fmtn,d_len
		character(NF90_MAX_NAME)::d_name,v_name
		
		integer::ier
		integer::k,kv
		
		ier = nf90_open(fn,NF90_NOWRITE,f_id)
		ier = nf90_inquire(f_id,Nd,Nv,Na,ud_id,fmtn)
		
		allocate(vars(Nv-Nd))
		kv = 0
		
		do k=1,Nd
			ier = nf90_inquire_dimension(f_id, k,d_name,d_len)
			select case(trim(d_name))
			case('x')
				allocate(x(d_len))
			case('y')
				allocate(y(d_len))
			case('z')
				allocate(z(d_len))
			case('t')
				allocate(t(d_len))
			end select
		end do
		
		do k=1,Nv
			ier = nf90_inquire_variable(f_id,k,v_name)
			select case(trim(v_name))
			case('x')
				ier = nf90_get_var(f_id,k,x)
			case('y')
				ier = nf90_get_var(f_id,k,y)
			case('z')
				ier = nf90_get_var(f_id,k,z)
			case('t')
				ier = nf90_get_var(f_id,k,t)
			case default
				kv = kv+1
				vars(kv) = trim(v_name)
			end select
		end do
		
		ier = nf90_close(f_id)
	end subroutine readGrid

	subroutine readStep_1d(fn,vn,v,ts)
		!! Read a variable step from a file
		character(*),intent(in)::fn
			!! Filename
		character(*),intent(in)::vn
			!! Variable name
		real(dp),dimension(:),intent(out)::v
			!! Variable to fill with data from file
		integer,intent(in),optional::ts
			!! Timestep
		
		integer::f_id
		integer::Nd,Nv,Na,ud_id,fmtn,v_id
		
		integer::ier,lts
		
		lts = -1
		if(present(ts)) lts = ts
		
		ier = nf90_open(fn,NF90_NOWRITE,f_id)
		ier = nf90_inquire(f_id,Nd,Nv,Na,ud_id,fmtn)
		ier = nf90_inq_varid(f_id,vn,v_id)
		if(ud_id<0) then
			ier = nf90_get_var(f_id,v_id,v,[1],shape(v))
		else
			ier = nf90_get_var(f_id,v_id,v,[1,lts],[shape(v),1])
		end if
		
		ier = nf90_close(f_id)
	end subroutine readStep_1d

	subroutine readStep_2d(fn,vn,v,ts)
		!! Read a variable step from a file
		character(*),intent(in)::fn
			!! Filename
		character(*),intent(in)::vn
			!! Variable name
		real(dp),dimension(:,:),intent(out)::v
			!! Variable to fill with data from file
		integer,intent(in),optional::ts
			!! Timestep
		
		integer::f_id
		integer::Nd,Nv,Na,ud_id,fmtn,v_id
		
		integer::ier,lts
		
		lts = -1
		if(present(ts)) lts = ts
		
		ier = nf90_open(fn,NF90_NOWRITE,f_id)
		ier = nf90_inquire(f_id,Nd,Nv,Na,ud_id,fmtn)
		ier = nf90_inq_varid(f_id,vn,v_id)
		if(ud_id<0) then
			ier = nf90_get_var(f_id,v_id,v,[1,1],shape(v))
		else
			ier = nf90_get_var(f_id,v_id,v,[1,1,lts],[shape(v),1])
		end if
		
		ier = nf90_close(f_id)
	end subroutine readStep_2d

	subroutine readStep_3d(fn,vn,v,ts)
		!! Read a variable step from a file
		character(*),intent(in)::fn
			!! Filename
		character(*),intent(in)::vn
			!! Variable name
		real(dp),dimension(:,:,:),intent(out)::v
			!! Variable to fill with data from file
		integer,intent(in),optional::ts
			!! Timestep
		
		integer::f_id
		integer::Nd,Nv,Na,ud_id,fmtn,v_id
		
		integer::ier,lts
		
		lts = -1
		if(present(ts)) lts = ts
		
		ier = nf90_open(fn,NF90_NOWRITE,f_id)
		ier = nf90_inquire(f_id,Nd,Nv,Na,ud_id,fmtn)
		ier = nf90_inq_varid(f_id,vn,v_id)
		if(ud_id<0) then
			ier = nf90_get_var(f_id,v_id,v,[1,1,1],shape(v))
		else
			ier = nf90_get_var(f_id,v_id,v,[1,1,1,lts],[shape(v),1])
		end if
		
		ier = nf90_close(f_id)
	end subroutine readStep_3d

	!===================!
	!= Output Routines =!
	!===================!

	subroutine writeGrid(fn,vars,x,y,z)
		!! Write a grid to a file
		character(*),intent(in)::fn
			!! Filename
		character(*),dimension(:),intent(in)::vars
			!! Variable names
		real(dp),dimension(:),intent(in),optional::x
			!! Grid data
		real(dp),dimension(:),intent(in),optional::y
			!! Grid data
		real(dp),dimension(:),intent(in),optional::z
			!! Grid data
		
		character(100)::p
		integer::c
		integer,dimension(:),allocatable::dims
		integer::f_id
		integer::i_id,j_id,k_id,l_id
		integer::x_id,y_id,z_id,t_id
		integer::v_id
		
		integer::ier
		integer::k
		
		ier = nf90_create(fn,NF90_CLOBBER,f_id)
		ier = nf90_def_dim(f_id,'x',size(x),i_id)
		if(present(y)) ier = nf90_def_dim(f_id,'y',size(y),j_id)
		if(present(z)) ier = nf90_def_dim(f_id,'z',size(z),k_id)
		ier = nf90_def_dim(f_id,'t',NF90_UNLIMITED,l_id)
		
		ier = nf90_def_var(f_id,'x',NF90_DOUBLE,[i_id],x_id)
		if(present(y)) ier = nf90_def_var(f_id,'y',NF90_DOUBLE,[j_id],y_id)
		if(present(z)) ier = nf90_def_var(f_id,'z',NF90_DOUBLE,[k_id],z_id)
		ier = nf90_def_var(f_id,'t',NF90_DOUBLE,[l_id],t_id)
		ier = nf90_put_att(f_id,t_id,'units','seconds since start')
		
		c = 1
		if(present(y)) c = c+10
		if(present(z)) c = c+100
		select case(c)
		case(001)
			dims = [i_id,l_id]
		case(011)
			dims = [i_id,j_id,l_id]
			p = 'yp, product; xp, product'
		case(111)
			dims = [i_id,j_id,k_id,l_id]
		end select
		do k=1,size(vars)
			ier = nf90_def_var(f_id,vars(k),NF90_DOUBLE,dims,v_id)
			ier = nf90_put_att(f_id,v_id,'field',vars(k))
		end do
		
		ier = nf90_enddef(f_id)
		
		ier = nf90_put_var(f_id,x_id,x)
		if(present(y)) ier = nf90_put_var(f_id,y_id,y)
		if(present(z)) ier = nf90_put_var(f_id,z_id,z)
		
		ier = nf90_close(f_id)
	end subroutine writeGrid

	subroutine writeStep_1d(fn,t,ts,vn,v)
		!! Write a variable at a timestep
		character(*),intent(in)::fn
			!! Filename
		real(dp),intent(in)::t
			!! Time
		integer,intent(in)::ts
			!! Timestep
		character(*),intent(in)::vn
			!! Variable name
		real(dp),dimension(:),intent(in)::v
			!! Variable
		
		integer::f_id,t_id,v_id
		integer::ier
		
		ier = nf90_open(fn,NF90_WRITE,f_id)
		
		ier = nf90_inq_varid(f_id,vn,v_id)
		ier = nf90_inq_varid(f_id,'t',t_id)
		
		ier = nf90_put_var(f_id,t_id,[t],[ts])
		ier = nf90_put_var(f_id,v_id,v,[1,ts])
		
		ier = nf90_close(f_id)
	end subroutine writeStep_1d

	subroutine writeStep_2d(fn,t,ts,vn,v)
		!! Write a variable at a timestep
		character(*),intent(in)::fn
			!! Filename
		real(dp),intent(in)::t
			!! Time
		integer,intent(in)::ts
			!! Timestep
		character(*),intent(in)::vn
			!! Variable name
		real(dp),dimension(:,:),intent(in)::v
			!! Variable
		
		integer::f_id,t_id,v_id
		integer::ier
		
		ier = nf90_open(fn,NF90_WRITE,f_id)
		
		ier = nf90_inq_varid(f_id,vn,v_id)
		ier = nf90_inq_varid(f_id,'t',t_id)
		
		ier = nf90_put_var(f_id,t_id,[t],[ts])
		ier = nf90_put_var(f_id,v_id,v,[1,1,ts])
		
		ier = nf90_close(f_id)
	end subroutine writeStep_2d

	subroutine writeStep_3d(fn,t,ts,vn,v)
		!! Write a variable at a timestep
		character(*),intent(in)::fn
			!! Filename
		real(dp),intent(in)::t
			!! Time
		integer,intent(in)::ts
			!! Timestep
		character(*),intent(in)::vn
			!! Variable name
		real(dp),dimension(:,:,:),intent(in)::v
			!! Variable
		
		integer::f_id,t_id,v_id
		integer::ier
		
		ier = nf90_open(fn,NF90_WRITE,f_id)
		
		ier = nf90_inq_varid(f_id,vn,v_id)
		ier = nf90_inq_varid(f_id,'t',t_id)
		
		ier = nf90_put_var(f_id,t_id,[t],[ts])
		ier = nf90_put_var(f_id,v_id,v,[1,1,1,ts])
		
		ier = nf90_close(f_id)
	end subroutine writeStep_3d

end module netCDF_mod
