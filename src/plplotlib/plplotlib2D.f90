module plplotlib2D_mod
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

	subroutine contour(x,y,z,N,lineColor,lineStyle,lineWidth)
		!! Plot contour lines
		real(wp),dimension(:),intent(in)::x
			!! x-coordinates of data
		real(wp),dimension(:),intent(in)::y
			!! y-coordinates of data
		real(wp),dimension(:,:),intent(in)::z
			!! Data for contouring
		integer,intent(in),optional::N
			!! Number of levels to use in contour
		character(*),intent(in),optional::lineColor
			!! Color of contour lines
		character(*),intent(in),optional::lineStyle
			!! Style of contour lines
		real(wp),optional::lineWidth
			!! Width of contour lines
		
		real(pp),dimension(:),allocatable::xl,yl
		real(pp),dimension(:,:),allocatable::zl
		
		real(pp),dimension(:),allocatable::edge
		integer::Nl,k
		
		xl = localize(x)
		yl = localize(y)
		zl = localize(z)
		Nl = 20
		if(present(N)) Nl = N
		edge = [( real(k-1,pp)/real(Nl-1,pp)*(maxval(zl)-minval(zl))+minval(zl) ,k=1,Nl)]
		
		if(present(lineColor)) call setColor(lineColor)
		if(present(lineStyle)) call setLineStyle(lineStyle)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		
		call plcont(zl,edge,xl,yl)
		call resetPen()
	end subroutine contour

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

	subroutine contourf(x,y,z,N)
		!! Plot filled contours
		real(wp),dimension(:),intent(in)::x
			!! x-coordinates of data
		real(wp),dimension(:),intent(in)::y
			!! y-coordinates of data
		real(wp),dimension(:,:),intent(in)::z
			!! Data for contouring
		integer,intent(in),optional::N
			!! Number of levels to use in contour
		
		real(pp),dimension(:),allocatable::xl,yl
		real(pp),dimension(:,:),allocatable::zl
		
		real(pp),dimension(:),allocatable::edge
		
		character(1)::defined
		real(pp)::fill_width
		real(pp)::cont_width
		integer::cont_color
		integer::Nl
		
		xl = localize(x)
		yl = localize(y)
		zl = localize(z)
		Nl = 20
		if(present(N)) Nl = N
		
		edge = localize(linspace(minval(z),maxval(z),Nl))
		
		fill_width = -1.0_pp
		cont_width = -1.0_pp
		cont_color = -1
		
		call plshades(zl,defined,minval(xl),maxval(xl),minval(yl),maxval(yl), &
			& edge,fill_width,cont_color,cont_width)
		call resetPen()
	end subroutine contourf

	subroutine quiver(x,y,u,v,s,c,scaling,lineColor,lineStyle,lineWidth)
		!! Plot vectors
		real(wp),dimension(:),intent(in)::x
			!! x-positions of vectors
		real(wp),dimension(:),intent(in)::y
			!! y-positions of vectors
		real(wp),dimension(:,:),intent(in)::u
			!! u-components of vectors
		real(wp),dimension(:,:),intent(in)::v
			!! v-components of vectors
		real(wp),dimension(:,:),intent(in),optional::s
			!! Scale of vectors
		real(wp),dimension(:,:),intent(in),optional::c
			!! Color values for vectors
		real(wp),intent(in),optional::scaling
			!! Scaling of vectors
			!! < 0 = Automatic, then scaled
			!!   0 = Automatic
			!! > 0 = Directly scaled
		character(*),intent(in),optional::lineColor
			!! Color of vectors
		character(*),intent(in),optional::lineStyle
			!! Style of vectors' lines
		real(wp),optional::lineWidth
			!! Width of vectors' lines
		
		real(pp),dimension(:),allocatable::xl,yl
		real(pp),dimension(:,:),allocatable::ul,vl,sl
		real(pp),dimension(2)::xb,yb,sb,cb,d
		real(pp)::scalingl,scl,mag,clr
		integer::i,j
		
		xl = localize(x)
		yl = localize(y)
		ul = localize(u)
		vl = localize(v)
		
		d = real([x(2)-x(1),y(2)-y(1)],pp)
		
		xb = real(mixval(x),pp)
		yb = real(mixval(y),pp)
		if(present(s)) then
			sl = localize(s)
			sl = sl/maxval(sl)
		else
			sl = localize(u**2+v**2)
			sl = sqrt(sl)
			sl = sl/maxval(sl)
		end if
		sb = [minval(sl),maxval(sl)]
		cb = 0.0_wp
		if(present(c)) cb = real([minval(c),maxval(c)],pp)
		
		scalingl = 1.0_pp
		if(present(scaling)) scalingl = real(scaling,pp)
		
		if(present(lineColor)) call setColor(lineColor)
		if(present(lineStyle)) call setLineStyle(lineStyle)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		
		do i=1,size(u,1)
			do j=1,size(u,2)
				mag = norm2([ul(i,j),vl(i,j)])
				scl = scalingl*norm2(d)*sl(i,j)
				if(abs(scl)<1.0E-5_wp) cycle
				if(present(c)) then
					clr = real( (c(i,j)-cb(1))/(cb(2)-cb(1)) ,pp)
					clr = max(clr,0.0_pp)
					clr = min(clr,1.0_pp)
					call plcol1( clr )
				end if
				call plvect(ul(i:i,j:j)/mag,vl(i:i,j:j)/mag,scl,xl(i:i),yl(j:j))
			end do
		end do
		
		call resetPen()
	end subroutine quiver

end module plplotlib2D_mod
