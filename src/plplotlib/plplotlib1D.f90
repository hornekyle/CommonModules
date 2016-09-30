module plplotlib1D_mod
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

	subroutine hist(d,N,db,relWidth,fillColor,fillPattern,lineColor,lineWidth)
		!! Create a histogram
		real(wp),dimension(:),intent(in)::d
			!! Data for binning
		integer,intent(in),optional::N
			!! Number of bins
		real(wp),dimension(2),intent(in),optional::db
			!! Boundaries of bin range
		real(wp),intent(in),optional::relWidth
			!! Relative width of bars (default 0.8)
		character(*),intent(in),optional::fillColor
			!! Color of bar fills
		character(*),intent(in),optional::fillPattern
			!! Pattern of bar fills
		character(*),intent(in),optional::lineColor
			!! Color of lines around bars
		real(wp),optional::lineWidth
			!! Width of lines around bars
		
		real(wp),dimension(:,:),allocatable::h
		real(wp),dimension(2)::dbl
		integer::Nl
		
		real(wp)::relWidthl
		real(wp)::lineWidthl
		
		Nl = 20
		if(present(N)) Nl = N
		
		if(present(db)) then
			dbl = db
		else
			dbl = mixval(d)+[-1.0_wp,1.0_wp]*epsilon(1.0_wp)
		end if
		
		h = binData(d,Nl,dbl,normalize=3)
		
		relWidthl = 1.0_wp
		if(present(relWidth)) relWidthl = relWidth
		lineWidthl = 0.5_wp
		if(present(lineWidth)) lineWidthl = lineWidth
		
		if(present(lineColor)) then
			if(present(fillColor)) then
				if(present(fillPattern)) then
					call bar(h(:,1),h(:,2),relWidth=relWidthl,lineColor=lineColor,lineWidth=lineWidthl, &
					& fillColor=fillColor,fillPattern=fillPattern)
				else
					call bar(h(:,1),h(:,2),relWidth=relWidthl,lineColor=lineColor,lineWidth=lineWidthl, &
					& fillColor=fillColor)
				end if
			else
				if(present(fillPattern)) then
					call bar(h(:,1),h(:,2),h(:,2),relWidth=relWidthl,lineColor=lineColor,lineWidth=lineWidthl, &
					& fillPattern=fillPattern)
				else
					call bar(h(:,1),h(:,2),h(:,2),relWidth=relWidthl,lineColor=lineColor,lineWidth=lineWidthl)
				end if
			end if
		else
			if(present(fillColor)) then
				if(present(fillPattern)) then
					call bar(h(:,1),h(:,2),relWidth=relWidthl,lineWidth=lineWidthl, &
					& fillColor=fillColor,fillPattern=fillPattern)
				else
					call bar(h(:,1),h(:,2),relWidth=relWidthl,lineWidth=lineWidthl, &
					& fillColor=fillColor)
				end if
			else
				if(present(fillPattern)) then
					call bar(h(:,1),h(:,2),h(:,2),relWidth=relWidthl,lineWidth=lineWidthl, &
					& fillPattern=fillPattern)
				else
					call bar(h(:,1),h(:,2),h(:,2),relWidth=relWidthl,lineWidth=lineWidthl)
				end if
			end if
		end if

		
		call resetPen()
	end subroutine hist

	subroutine scatter(x,y,c,s,markColor,markStyle,markSize)
		!! Create scatter plot of data
		real(wp),dimension(:),intent(in)::x
			!! x-coordinates of data
		real(wp),dimension(:),intent(in)::y
			!! y-coordinates of data
		real(wp),dimension(:),intent(in),optional::c
			!! Data for smooth coloring
		real(wp),dimension(:),intent(in),optional::s
			!! Data for marker scaling
		character(*),intent(in),optional::markColor
			!! Color of markers; overridden by z
		character(*),intent(in),optional::markStyle
			!! Style of markers
		real(wp),intent(in),optional::markSize
			!! Size of markers
		
		real(pp),dimension(:),allocatable::xl,yl
		real(pp),dimension(:),allocatable::cb
		character(32)::code
		integer::k
		
		xl = localize(x)
		yl = localize(y)
		
		if(present(markColor)) call setColor(markColor)
		code = getSymbolCode('')
		if(present(markStyle)) code = getSymbolCode(markStyle)
		if(present(markSize)) call plschr(0.0_pp,real(markSize,pp))
		if(present(markSize)) call plssym(0.0_pp,real(markSize,pp))
		
		if(present(c)) cb = real(mixval(c),pp)
		do k=1,size(x)
			if(present(c)) call plcol1( real( (c(k)-cb(1))/(cb(2)-cb(1)) ,pp) )
			if(present(s)) call plschr(0.0_pp,real(s(k),pp))
			if(present(s)) call plssym(0.0_pp,real(s(k),pp))
			call plptex(xl(k),yl(k),0.0_pp,0.0_pp,0.5_pp,code)
		end do
		call resetPen()
	end subroutine scatter

	subroutine plot(x,y,lineColor,lineStyle,lineWidth,markColor,markStyle,markSize)
		!! Plot data using lines and or markers
		real(wp),dimension(:),intent(in)::x
			!! x-data for plot
		real(wp),dimension(:),intent(in)::y
			!! y-data for plot
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
		
		real(pp),dimension(:),allocatable::xl,yl
		character(32)::code
		integer::k
		
		xl = localize(x)
		yl = localize(y)
		
		if(present(lineColor)) call setColor(lineColor)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		if(present(lineStyle)) then
			call setLineStyle(lineStyle)
			if(lineStyle/='') call plline(xl,yl)
		else
			call plline(xl,yl)
		end if
		call resetPen()
		
		if(present(markColor)) call setColor(markColor)
		if(present(markSize)) call plssym(0.0_pp,real(markSize,pp))
		if(present(markStyle)) then
			code = getSymbolCode(markStyle)
			if(markStyle/='') then
				do k=1,size(x)
					call plptex(xl(k),yl(k),0.0_pp,0.0_pp,0.5_pp,code)
				end do
			end if
		end if
		call resetPen()
	end subroutine plot

	subroutine bar(x,y,c,relWidth,fillColor,fillPattern,lineColor,lineWidth)
		!! Create a bar graph
		real(wp),dimension(:),intent(in)::x
			!! x-positions of the bars' centers
		real(wp),dimension(:),intent(in)::y
			!! y-positions of the bars' tops
		real(wp),dimension(:),intent(in),optional::c
			!! Color scale for bars
		real(wp),intent(in),optional::relWidth
			!! Relative width of bars (default 0.8)
		character(*),intent(in),optional::fillColor
			!! Color of bar fills
		character(*),intent(in),optional::fillPattern
			!! Pattern of bar fills
		character(*),intent(in),optional::lineColor
			!! Color of lines around bars
		real(wp),optional::lineWidth
			!! Width of lines around bars
		
		real(pp),dimension(4)::xl,yl
		real(pp),dimension(2)::cb
		real(pp)::dx,dxs
		integer::k
		
		cb = 0.0_wp
		if(present(c)) cb = real(mixval(c),pp)
		dxs = 0.8_pp
		if(present(relWidth)) dxs = real(relWidth,pp)
		if(size(x)>1) then
			dx = dxs*real(x(2)-x(1),pp)/2.0_pp
		else
			dx = dxs
		end if
		
		if(present(lineWidth)) call setLineWidth(lineWidth)
		
		do k=1,size(x)
			xl = real([x(k)-dx,x(k)-dx,x(k)+dx,x(k)+dx],pp)
			yl = real([0.0_wp,y(k),y(k),0.0_wp],pp)
			
			if(present(fillColor)) call setColor(fillColor)
			if(present(fillPattern)) call setFillPattern(fillPattern)
			if(present(c)) call plcol1( real( (c(k)-cb(1))/(cb(2)-cb(1)) ,pp) )
			call plfill(xl,yl)
			
			if(present(lineColor)) call setColor(lineColor)
			call plline(xl,yl)
		end do
		call resetPen()
	end subroutine bar

	subroutine barh(y,x,c,relWidth,fillColor,fillPattern,lineColor,lineWidth)
		!! Create a horizontal bar graph
		real(wp),dimension(:),intent(in)::y
			!! y-positions of the bars' centers
		real(wp),dimension(:),intent(in)::x
			!! x-positions of the bars' tops
		real(wp),dimension(:),intent(in),optional::c
			!! Color scale for bars
		real(wp),intent(in),optional::relWidth
			!! Relative width of bars
		character(*),intent(in),optional::fillColor
			!! Color of bar fills
		character(*),intent(in),optional::fillPattern
			!! Pattern of bar fills
		character(*),intent(in),optional::lineColor
			!! Color of lines around bars
		real(wp),optional::lineWidth
			!! Width of lines around bars
		
		real(pp),dimension(4)::xl,yl
		real(pp),dimension(2)::cb
		real(pp)::dy,dys
		integer::k
		
		cb = 0.0_wp
		if(present(c)) cb = real(mixval(c),pp)
		dys = 0.8_pp
		if(present(relWidth)) dys = real(relWidth,pp)
		dy = dys*real(y(2)-y(1),pp)/2.0_pp
		
		if(present(lineWidth)) call setLineWidth(lineWidth)
		
		do k=1,size(x)
			yl = real([y(k)-dy,y(k)-dy,y(k)+dy,y(k)+dy],pp)
			xl = real([0.0_wp,x(k),x(k),0.0_wp],pp)
			
			if(present(fillColor)) call setColor(fillColor)
			if(present(fillPattern)) call setFillPattern(fillPattern)
			if(present(c)) call plcol1( real( (c(k)-cb(1))/(cb(2)-cb(1)) ,pp) )
			call plfill(xl,yl)
			
			if(present(lineColor)) call setColor(lineColor)
			call plline(xl,yl)
		end do
		call resetPen()
	end subroutine barh

	subroutine fillBetween(x,y1,y0,fillColor,fillPattern,lineWidth)
		!! Fill space between two lines
		real(wp),dimension(:),intent(in)::x
		real(wp),dimension(:),intent(in)::y1
		real(wp),dimension(:),intent(in),optional::y0
		character(*),intent(in),optional::fillColor
		character(*),intent(in),optional::fillPattern
		real(wp),intent(in),optional::lineWidth
		
		real(pp),dimension(:),allocatable::xl,y1l,y0l
		integer::N
		
		N = size(x)
		
		xl  = localize(x)
		y1l = localize(y1)
		if(present(y0)) then
			y0l = localize(y0)
		else
			allocate(y0l(N))
			y0l = 0.0_pp
		end if
		
		if(present(fillColor)) call setColor(fillColor)
		if(present(fillPattern)) call setFillPattern(fillPattern)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		call plfill([xl(1:N:1),xl(N:1:-1)],[y1l(1:N:1),y0l(N:1:-1)])
		call resetPen()
	end subroutine fillBetween

	subroutine fillBetweenx(y,x1,x0,fillColor,fillPattern,lineWidth)
		!! Fill space between two lines
		real(wp),dimension(:),intent(in)::y
		real(wp),dimension(:),intent(in)::x1
		real(wp),dimension(:),intent(in),optional::x0
		character(*),intent(in),optional::fillColor
		character(*),intent(in),optional::fillPattern
		real(wp),intent(in),optional::lineWidth
		
		real(pp),dimension(:),allocatable::yl,x1l,x0l
		integer::N
		
		N = size(y)
		
		yl  = localize(y)
		x1l = localize(x1)
		if(present(x0)) then
			x0l = localize(x0)
		else
			allocate(x0l(N))
			x0l = 0.0_pp
		end if
		
		if(present(fillColor)) call setColor(fillColor)
		if(present(fillPattern)) call setFillPattern(fillPattern)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		call plfill([x1l(1:N:1),x0l(N:1:-1)],[yl(1:N:1),yl(N:1:-1)])
		call resetPen()
	end subroutine fillBetweenx

	subroutine errorbar(x,y,xerr,yerr,lineColor,lineStyle,lineWidth)
		!! Plot error bars for a set of data points
		real(wp),dimension(:),intent(in)::x
			!! x-data for plot
		real(wp),dimension(:),intent(in)::y
			!! y-data for plot
		real(wp),dimension(:),intent(in),optional::xerr
			!! x-data error for plot
		real(wp),dimension(:),intent(in),optional::yerr
			!! y-data error for plot
		character(*),intent(in),optional::lineColor
			!! Color of line
		character(*),intent(in),optional::lineStyle
			!! Style of line; '' for no line
		real(wp),intent(in),optional::lineWidth
			!! Width of line
		
		real(pp),dimension(:),allocatable::xl,yl
		real(pp),dimension(:),allocatable::xll,xlh
		real(pp),dimension(:),allocatable::yll,ylh
		
		xl = localize(x)
		yl = localize(y)
		
		if(present(lineColor)) call setColor(lineColor)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		if(present(lineStyle)) call setLineStyle(lineStyle)
		
		if(present(xerr)) then
			xll = localize(x-xerr)
			xlh = localize(x+xerr)
			call plerrx(xll,xlh,yl)
		end if
		
		if(present(yerr)) then
			yll = localize(y-yerr)
			ylh = localize(y+yerr)
			call plerry(xl,yll,ylh)
		end if
		
		call resetPen()
	end subroutine

end module plplotlib1D_mod
