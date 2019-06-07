module plplotlib2D_mod
	!! Wrapper module for plplot to give it a more matplotlib like personality
	use plplotlibBase_mod
	implicit none
	public
	
contains

	!=====================!
	!= Plotting Routines =!
	!=====================!

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
		
		call plcont(zl, 1, size(zl,1), 1, size(zl,2) , edge, xl, yl)
		call resetPen()
	end subroutine contour

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
		
		call plshades(z,minval(xl),maxval(xl),minval(yl),maxval(yl), &
			&         edge,fill_width,cont_color,cont_width,.true.)
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

	subroutine fill(x,y,c,cR)
		!! Fill a polygon
		real(wp),dimension(:),intent(in)::x
			!! x-positions of vectors
		real(wp),dimension(:),intent(in)::y
			!! y-positions of vectors
		real(wp),intent(in)::c
			!! Color values for vectors
		real(wp),dimension(2),intent(in)::cR
			!! Range of color variable
		
		real(pp),dimension(:),allocatable::xl,yl
		real(pp)::clr
		
		xl = localize(x)
		yl = localize(y)
		
		clr = real( (c-cR(1))/(cR(2)-cR(1)) ,pp)
		clr = max(clr,0.0_pp)
		clr = min(clr,1.0_pp)
		call plcol1( clr )
		
		call plfill(xl,yl)
		
		call resetPen()
	end subroutine fill

end module plplotlib2D_mod
