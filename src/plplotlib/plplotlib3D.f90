module plplotlib3D_mod
	!! Wrapper module for plplot to give it a more matplotlib like personality
	use kinds_mod, only: wp
	use plplot
	use array_mod
	use plplotlibBase_mod
	implicit none
	public
	
contains

	!=====================!
	!= Plotting Routines =!
	!=====================!

	subroutine plot3(x,y,z,lineColor,lineStyle,lineWidth,markColor,markStyle,markSize)
		!! Plot data using lines and or markers
		real(wp),dimension(:),intent(in)::x
			!! x-data for plot
		real(wp),dimension(:),intent(in)::y
			!! y-data for plot
		real(wp),dimension(:),intent(in)::z
			!! z-data for plot
		character(*),intent(in),optional::lineColor
			!! Color of line
		character(*),intent(in),optional::lineStyle
			!! Style of line; '' for no line
		real(wp),intent(in),optional::lineWidth
			!! Width of line
		character(*),intent(in),optional::markColor
			!! Color of markers, if any
		character(*),intent(in),optional::markStyle
			!! Style of markers; '' or absent for none
		real(wp),intent(in),optional::markSize
			!! Size of markers, if any
		
		real(pp),dimension(:),allocatable::xl,yl,zl
		real(pp)::dx,dy,dz,sx,sy,sz
		character(32)::code
		integer::k
		
		xl = localize(x)
		yl = localize(y)
		zl = localize(z)
		
		if(present(lineColor)) call setColor(lineColor)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		if(present(lineStyle)) then
			call setLineStyle(lineStyle)
			if(lineStyle/='') call plline(xl,yl)
		else
			call plline3(xl,yl,zl)
		end if
		call resetPen()
		
		if(present(markColor)) call setColor(markColor)
		if(present(markSize)) call plssym(0.0_pp,real(markSize,pp))
		if(present(markStyle)) then
			code = getSymbolCode(markStyle)
			if(markStyle/='') then
				dx = 1.0_pp
				dy = 0.0_pp
				dz = 0.0_pp
				sx = 0.0_pp
				sy = 0.0_pp
				sz = 0.0_pp
				do k=1,size(x)
					call plptex3(xl(k),yl(k),zl(k),dx,dy,dz,sx,sy,sz,0.5_pp,code)
				end do
			end if
		end if
		call resetPen()
	end subroutine plot3

	subroutine surface(x,y,z,N,lineStyle)
		!! Plot a 3d surface
		real(wp),dimension(:),intent(in)::x
			!! x-coordinates of data
		real(wp),dimension(:),intent(in)::y
			!! y-coordinates of data
		real(wp),dimension(:,:),intent(in)::z
			!! Data for contouring
		integer,intent(in),optional::N
			!! Number of levels to use in surface colors
		character(*),intent(in),optional::lineStyle
			!! Style for xy lines ( '-' = on, '' = off )
		
		real(pp),dimension(:),allocatable::xl,yl
		real(pp),dimension(:,:),allocatable::zl
		
		real(pp),dimension(:),allocatable::edge
		integer::Nl,opt
		
		opt = MAG_COLOR
		
		xl = localize(x)
		yl = localize(y)
		zl = localize(z)
		Nl = 20
		if(present(N)) then
			Nl = N
			opt = ior(opt,SURF_CONT)
		end if
		edge = localize(linspace(minval(z),maxval(z),Nl))
		
		if(present(lineStyle)) then
			select case(lineStyle)
			case('')
				opt = opt
			case('-')
				opt = ior(opt,FACETED)
			end select
		end if
		
		call plsurf3d(xl,yl,zl,opt,edge)
		call resetPen()
	end subroutine surface

	subroutine wireframe(x,y,z,lineColor)
		!! Plot a 3d wireframe
		real(wp),dimension(:),intent(in)::x
			!! x-coordinates of data
		real(wp),dimension(:),intent(in)::y
			!! y-coordinates of data
		real(wp),dimension(:,:),intent(in)::z
			!! Data for contouring
		character(*),intent(in),optional::lineColor
			!! Color of contour lines
		
		real(pp),dimension(:),allocatable::xl,yl
		real(pp),dimension(:,:),allocatable::zl
		
		xl = localize(x)
		yl = localize(y)
		zl = localize(z)
		
		if(present(lineColor)) then
			call setColor(lineColor)
			call plot3d(xl,yl,zl,DRAW_LINEXY,.false.)
		else
			call plot3d(xl,yl,zl,ior(DRAW_LINEXY,MAG_COLOR),.false.)
		end if
		
		call resetPen()
	end subroutine wireframe

end module plplotlib3D_mod
