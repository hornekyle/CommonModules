module plplotlibBase_mod
	!! Wrapper module for plplot to give it a more matplotlib like personality
	use kinds_mod, only: wp
	use plplot
	use array_mod
	use text_mod
	implicit none
	public
	
	integer,parameter::pp = plflt
	integer,parameter::pi = kind(1)
	
	character(*),parameter::default_dev = 'svgqt'
		!! Default output device
	
	!=================!
	!= Library State =!
	!=================!
	
	logical::didShow = .false.
		!! Flag for library display status
	real(pp)::fontScale = 1.0_pp
		!! Font scale factor to resetPen
	logical::blackOnWhite = .true.
		!! Reverse black and white
	logical::transparentBackground = .false.
		!! Transparent background
	
	!==============!
	!= Interfaces =!
	!==============!
	
	interface localize
		module procedure localize_1
		module procedure localize_2
	end interface
	
	!===========!
	!= Exports =!
	!===========!
	
contains

	!===================!
	!= Helper Routines =!
	!===================!

	function binData(d,N,db,normalize) result(o)
		!! Count data in each bin
		real(wp),dimension(:),intent(in)::d
			!! Data for binning
		integer,intent(in),optional::N
			!! Number of bins
		real(wp),dimension(2),intent(in),optional::db
			!! Boundaries of bin range
		integer,intent(in),optional::normalize
			!! Normalization type (1=sum, 2=bin size, 3=maxval)
		real(wp),dimension(:,:),allocatable::o
		
		real(wp),dimension(:),allocatable::b
		integer::Nl,k
		
		Nl = 10
		if(present(N)) Nl = N
		
		if(present(db)) then
			b = linspace(db(1),db(2),Nl+1)
		else
			b = linspace(minval(d)-epsilon(1.0_wp),maxval(d)+epsilon(1.0_wp),Nl+1)
		end if
		
		allocate(o(Nl,2))
		o(:,1) = (b(1:Nl)+b(2:Nl+1))/2.0_wp
		
		do k=1,Nl
			o(k,2) = real(count(d>=b(k) .and. d<=b(k+1)),wp)
		end do
		
		if(present(normalize)) then
			select case(normalize)
			case(1)
				o(:,2) = o(:,2)/sum(o(:,2))
			case(2)
				do k=1,Nl
					o(k,2) = o(k,2)/(b(k+1)-b(k))
				end do
			case(3)
				o(:,2) = o(:,2)/maxval(o(:,2))
			end select
		end if
	end function binData

	function localize_1(A) result(o)
		real(wp),dimension(:),intent(in)::A
		real(pp),dimension(:),allocatable::o
		integer::N,k
		
		N = size(A)
		allocate(o(N))
		forall(k=1:N) o(k) = real(A(k),pp)
	end function localize_1

	function localize_2(A) result(o)
		real(wp),dimension(:,:),intent(in)::A
		real(pp),dimension(:,:),allocatable::o
		integer::N,M,i,j
		
		N = size(A,1)
		M = size(A,2)
		allocate(o(N,M))
		forall(i=1:N,j=1:M) o(i,j) = real(A(i,j),pp)
	end function localize_2

	!============================!
	!= Axes and Figure Routines =!
	!============================!

	subroutine figure
		!! Create a new figure
		logical,save::isFirst = .true.
		
		if(.not.isFirst) then
			call pleop()
		else
			isFirst = .false.
		end if
		
		call plbop()
		call plssub(1,1)
		call pladv(1)
		call resetPen()
	end subroutine figure

	subroutine subplot(ny,nx,i,aspect,is3d)
		!! Create a set of axes on a figure
		integer,intent(in)::nx
			!! Number of subplot columns
		integer,intent(in)::ny
			!! Number of subplot rows
		integer,intent(in)::i
			!! Subplot to use
		real(wp),intent(in),optional::aspect
			!! Aspect ratio of the axes
		logical,intent(in),optional::is3d
		
		logical::is3dl
		
		call plssub(nx,ny)
		call pladv(i)
		call resetPen()
		
		is3dl = .false.
		if(present(is3d)) is3dl = is3d
		
		if(is3dl) then
			call plvpor(0.0_pp,1.0_pp,0.0_pp,1.0_pp)
		else
			if(present(aspect)) then
				call plvasp(real(aspect,pp))
			else
				call plvsta()
			end if
		end if
		
		call defaultLim()
	end subroutine subplot

	subroutine defaultLim
		real(pp),parameter::eps = epsilon(1.0_pp)
		
		call plwind(-eps,eps,-eps,eps)
	end subroutine defaultLim

	subroutine xylim(xb,yb)
		!! Set the x and y ranges of the plot
		real(wp),dimension(2),intent(in)::xb
			!! x-range of plot
		real(wp),dimension(2),intent(in)::yb
			!! y-range of plot
		
		real(pp),dimension(2)::xbl,ybl
		
		xbl = localize(xb)
		ybl = localize(yb)
		
		call plwind(xbl(1),xbl(2),ybl(1),ybl(2))
	end subroutine xylim

	subroutine xlim(xl,xh)
		!! Set the limits of the x-axis
		real(wp),intent(in)::xl,xh
		
		real(pp)::x1,x2,y1,y2
		
		call plgvpw(x1,x2,y1,y2)
		call plwind(real(xl,pp),real(xh,pp),y1,y2)
	end subroutine xlim

	subroutine ylim(yl,yh)
		!! Set the limits of the y-axis
		real(wp),intent(in)::yl,yh
		
		real(pp)::x1,x2,y1,y2
		
		call plgvpw(x1,x2,y1,y2)
		call plwind(x1,x2,real(yl,pp),real(yh,pp))
	end subroutine ylim

	subroutine xyzlim(xb,yb,zb,altitude,azimuth,zoom)
		!! Set the limits for a 3d plot
		real(wp),dimension(2),intent(in)::xb
			!! x-range of plot
		real(wp),dimension(2),intent(in)::yb
			!! y-range of plot
		real(wp),dimension(2),intent(in)::zb
			!! z-range of plot
		real(wp),intent(in),optional::altitude
			!! Altitude angle of plot in degrees
		real(wp),intent(in),optional::azimuth
			!! Azimuth angle of plot in degrees
		real(wp),intent(in),optional::zoom
			!! Zoom ratio (default 1.0)
		
		real(pp)::al,az,zm
		
		al = 45.0_pp
		if(present(altitude)) al = real(altitude,pp)
		az = 60.0_pp
		if(present(azimuth)) az = real(azimuth,pp)
		zm = 1.0_pp
		if(present(zoom)) zm = real(zoom,pp)
		
		call plwind(-1.0_pp,1.0_pp,-1.0_pp,1.5_pp)
		call plw3d(zm,zm,1.2_pp*zm, &
		& real(xb(1),pp),real(xb(2),pp), &
		& real(yb(1),pp),real(yb(2),pp), &
		& real(zb(1),pp),real(zb(2),pp),al,az)
	end subroutine xyzlim

	subroutine ticks(dx,dy,logx,logy,color,lineWidth)
		!! Set the ticks for the axes
		real(wp),intent(in),optional::dx
			!! Spacing between ticks on x-axis
		real(wp),intent(in),optional::dy
			!! Spacing between ticks on y-axis
		logical,intent(in),optional::logx
			!! Flag for log-ticks and labels on x-axis
		logical,intent(in),optional::logy
			!! Flag for log-ticks and labels on y-axis
		character(*),intent(in),optional::color
			!! Color code for ticks, box, and labels
		real(wp),optional::linewidth
			!! Line width for ticks and box
		
		real(pp)::dxl,dyl
		character(10)::xopts,yopts
		
		dxl = 0.0_pp
		if(present(dx)) dxl = real(dx,pp)
		
		dyl = 0.0_pp
		if(present(dy)) dyl = real(dy,pp)
		
		xopts = 'bcnst'
		if(present(logx)) then
			if(logx) xopts = 'bcnstl'
		end if
		
		yopts = 'bcnstv'
		if(present(logy)) then
			if(logy) yopts = 'bcnstvl'
		end if
		
		call resetPen()
		
		if(present(color)) call setColor(color)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		
		call plbox(xopts,dxl,0,yopts,dyl,0)
		call resetPen()
	end subroutine ticks

	subroutine box(xLabel,yLabel,zLabel,color)
		!! Set x,y and plot labels
		character(*),intent(in)::xLabel
			!! Label for x-axis
		character(*),intent(in)::yLabel
			!! Label for x-axis
		character(*),intent(in)::zLabel
			!! Label for z-axis
		character(*),intent(in),optional::color
			!! Color of labels
		
		if(present(color)) call setColor(color)
		call plbox3('bnstu',xLabel,0.0_pp,0,'bnstu',yLabel,0.0_pp,0,'bnstu',zLabel,0.0_pp,0)
		call resetPen()
	end subroutine box

	subroutine xticks(d,logScale,primary,secondary,color,lineWidth)
		!! Set the ticks for the x-axis
		real(wp),intent(in),optional::d
			!! Spacing between ticks
		logical,intent(in),optional::logScale
			!! Flag for log-ticks and labels
		logical,intent(in),optional::primary
			!! Draw primary axis
		logical,intent(in),optional::secondary
			!! Draw secondary axis
		character(*),intent(in),optional::color
			!! Color code for ticks, box, and labels
		real(wp),optional::linewidth
			!! Line width for ticks and box
		real(pp)::dxl,dyl
		character(10)::xopts,yopts
		
		dxl = 0.0_pp
		dyl = 0.0_pp
		if(present(d)) dxl = real(d,pp)
		
		xopts = 'nst'
		
		if(present(primary)) then
			if(primary) xopts = trim(xopts)//'b'
		else
			xopts = trim(xopts)//'b'
		end if
		
		if(present(secondary)) then
			if(secondary) xopts = trim(xopts)//'c'
		else
			xopts = trim(xopts)//'c'
		end if
		
		if(present(logScale)) then
			if(logScale) xopts = trim(xopts)//'l'
		end if
		yopts = ''
		
		if(present(color)) call setColor(color)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		call plbox(xopts,dxl,0,yopts,dyl,0)
		call resetPen()
	end subroutine xticks

	subroutine yticks(d,logScale,primary,secondary,color,lineWidth)
		!! Set the ticks for the y-axis
		real(wp),intent(in),optional::d
			!! Spacing between ticks
		logical,intent(in),optional::logScale
			!! Flag for log-ticks and labels
		logical,intent(in),optional::primary
			!! Draw primary axis
		logical,intent(in),optional::secondary
			!! Draw secondary axis
		character(*),intent(in),optional::color
			!! Color code for ticks, box, and labels
		real(wp),optional::linewidth
			!! Line width for ticks and box
		real(pp)::dxl,dyl
		character(10)::xopts,yopts
		
		dxl = 0.0_pp
		dyl = 0.0_pp
		if(present(d)) dyl = real(d,pp)
		
		yopts = 'nst'
		
		if(present(primary)) then
			if(primary) yopts = trim(xopts)//'b'
		else
			yopts = trim(yopts)//'b'
		end if
		
		if(present(secondary)) then
			if(secondary) yopts = trim(yopts)//'c'
		else
			yopts = trim(yopts)//'c'
		end if
		
		if(present(logScale)) then
			if(logScale) yopts = trim(yopts)//'l'
		end if
		xopts = ''
		
		if(present(color)) call setColor(color)
		if(present(lineWidth)) call setLineWidth(lineWidth)
		call plbox(xopts,dxl,0,yopts,dyl,0)
		call resetPen()
	end subroutine yticks

	subroutine labels(xLabel,yLabel,plotLabel,color)
		!! Set x,y and plot labels
		character(*),intent(in)::xLabel
			!! Label for x-axis
		character(*),intent(in)::yLabel
			!! Label for x-axis
		character(*),intent(in)::plotLabel
			!! Label entire plot
		character(*),intent(in),optional::color
			!! Color of labels
		
		if(present(color)) call setColor(color)
		call pllab(xLabel,yLabel,plotLabel)
		call resetPen()
	end subroutine labels

	subroutine xlabel(label,color)
		!! Set x-label
		character(*),intent(in)::label
			!! Label for axis
		character(*),intent(in),optional::color
			!! Color of labels
		
		if(present(color)) call setColor(color)
		call plmtex('b',3.0_pp,0.5_pp,0.5_pp,label)
		call resetPen()
	end subroutine xlabel

	subroutine ylabel(label,color)
		!! Set y-label
		character(*),intent(in)::label
			!! Label for axis
		character(*),intent(in),optional::color
			!! Color of labels
		
		if(present(color)) call setColor(color)
		call plmtex('l',5.0_pp,0.5_pp,0.5_pp,label)
		call resetPen()
	end subroutine ylabel

	subroutine title(label,color)
		!! Set plot title
		character(*),intent(in)::label
			!! Label for plot
		character(*),intent(in),optional::color
			!! Color of labels
		
		if(present(color)) call setColor(color)
		call plmtex('t',1.5_pp,0.5_pp,0.5_pp,label)
		call resetPen()
	end subroutine title

	subroutine colorbar(z,N,leftLabel,rightLabel)
		!! Add a colorbar to the top of the plot
		real(wp),dimension(:,:),intent(in)::z
			!! Data used for levels computation
		integer,intent(in)::N
			!! Number of levels to compute
		character(*),intent(in),optional::leftLabel
			!! Label for left side of colorbar
		character(*),intent(in),optional::rightLabel
			!! Label for right side of colorbar
		
		real(pp),dimension(:,:),allocatable::values
		character(64),dimension(2)::labels
		
		real(pp)::fill_width
		real(pp)::cont_width
		integer::cont_color
		real(pp)::colorbar_width
		real(pp)::colorbar_height
		integer::k
		
		values = reshape( &
			& real([( real(k-1,wp)/real(N-1,wp)*(maxval(z)-minval(z))+minval(z) ,k=1,N)],pp), &
			& [N,1])
		
		fill_width = 2.0_pp
		cont_width = 0.0_pp
		cont_color = 1
		labels = ''
		if(present(leftLabel )) labels(1) = leftLabel
		if(present(rightLabel)) labels(2) = rightLabel
		
		call plcolorbar(colorbar_width,colorbar_height,&
			& ior(PL_COLORBAR_GRADIENT,PL_COLORBAR_SHADE_LABEL),PL_POSITION_TOP,&
			& 0.0_pp,0.01_pp,0.75_pp,0.05_pp,&
			& 0,1,1,0.0_pp,0.0_pp, &
			& cont_color,cont_width, &
			& [PL_COLORBAR_LABEL_LEFT,PL_COLORBAR_LABEL_RIGHT],labels, &
			& ['bcvmt'],[0.0_pp],[0],[size(values)],values)
	end subroutine colorbar

	subroutine colorbar2(z,N,leftLabel,rightLabel)
		!! Add a colorbar to the top of the plot
		real(wp),dimension(:,:),intent(in)::z
			!! Data used for levels computation
		integer,intent(in)::N
			!! Number of levels to compute
		character(*),intent(in),optional::leftLabel
			!! Label for left side of colorbar
		character(*),intent(in),optional::rightLabel
			!! Label for right side of colorbar
		
		real(pp),dimension(:,:),allocatable::values
		character(64),dimension(2)::labels
		
		real(pp)::fill_width
		real(pp)::cont_width
		integer::cont_color
		real(pp)::colorbar_width
		real(pp)::colorbar_height
		integer::k
		
		values = reshape( &
			& real([( real(k-1,wp)/real(N-1,wp)*(maxval(z)-minval(z))+minval(z) ,k=1,N)],pp), &
			& [N,1])
		
		fill_width = 2.0_pp
		cont_width = 0.0_pp
		cont_color = 1
		labels = ''
		if(present(leftLabel )) labels(1) = leftLabel
		if(present(rightLabel)) labels(2) = rightLabel
		
		call plcolorbar(colorbar_width,colorbar_height,&
			& ior(PL_COLORBAR_GRADIENT,PL_COLORBAR_SHADE_LABEL),PL_POSITION_RIGHT,&
			& 0.01_pp,0.0_pp,0.05_pp,0.75_pp,&
			& 0,1,1,0.0_pp,0.0_pp, &
			& cont_color,cont_width, &
			& [PL_COLORBAR_LABEL_BOTTOM,PL_COLORBAR_LABEL_TOP],labels, &
			& ['bcvmt'],[0.0_pp],[0],[size(values)],values)
	end subroutine colorbar2

	subroutine legend(corner,series,lineWidths,markScales,markCounts,ncol)
		!! Create legend for plot data
		!!
		!! FIXME: Text sizing should be modifiable
		character(*),intent(in)::corner
			!! Corner for legend
		character(*),dimension(:,:),intent(in)::series
			!! Data series in rows
			!! [name,textColor,lineStyle,lineColor,markStyle,markColor,boxColor]
		real(wp),dimension(:),intent(in),optional::lineWidths
			!! Line widths for the plots
		real(wp),dimension(:),intent(in),optional::markScales
			!! Marker sizes for the plots
		integer,dimension(:),intent(in),optional::markCounts
			!! Marker counts for the plots
		integer,intent(in),optional::ncol
			!! Number of columns
		
		real(pp)::width,height,xoff,yoff
		real(pp)::plotWidth
		integer::opt,cornerl
		integer::bg_color,bb_color,bb_style,lncol,lnrow
		integer,dimension(size(series,1))::opts
		real(pp),dimension(size(series,1))::lwidths,mscales
		integer,dimension(size(series,1))::mcounts,text_colors
		real(pp)::text_offset,text_scale,text_spacing,text_justification
		integer,dimension(size(series,1))::box_colors,box_patterns
		real(pp),dimension(size(series,1))::box_scales,box_line_widths
		integer,dimension(size(series,1))::line_colors,line_styles
		integer,dimension(size(series,1))::mark_colors
		character(64),dimension(size(series,1))::mark_styles
		integer::k
		
		call doLegendBox()
		
		opts = 0
		do k=1,size(series,1)
			if(series(k,3)/='') opts(k) = ior(opts(k),PL_LEGEND_LINE)
			if(series(k,5)/='') opts(k) = ior(opts(k),PL_LEGEND_SYMBOL)
			if(series(k,7)/='') opts(k) = ior(opts(k),PL_LEGEND_COLOR_BOX)
		end do
		
		call doText()
		call doBoxes()
		call doLines()
		call doMarkers()
		
		call pllegend(width,height,opt,cornerl,xoff,yoff,plotWidth, &
			& bg_color,bb_color,bb_style, &
			& lnrow,lncol,size(series,1),opts,text_offset, &
			& text_scale,text_spacing,text_justification,text_colors,series(:,1), &
			& box_colors,box_patterns,box_scales,box_line_widths, &
			& line_colors,line_styles,lwidths, &
			& mark_colors,mscales,mcounts,mark_styles)
		
	contains
	
		subroutine doLegendBox
			opt = PL_LEGEND_BACKGROUND+PL_LEGEND_BOUNDING_BOX
			cornerl = getCorner(corner)
			xoff = 0.0_pp
			yoff = 0.0_pp
			plotWidth = 0.05_pp
			bg_color = 0
			bb_color = 1
			bb_style = getLineStyleCode('-')
			
			lncol = 1
			if(present(ncol)) lncol = ncol
			lnrow = size(series,1)/lncol
		end subroutine doLegendBox
	
		subroutine doText
			text_offset  = 0.3_pp
			text_scale   = fontScale
			text_spacing = 3.0_pp
			text_justification = 0.0_pp
			
			do k=1,size(series,1)
				text_colors = getColorCode(series(k,2))
			end do
		end subroutine doText
	
		subroutine doBoxes
			do k=1,size(series,1)
				box_colors(k) = getColorCode(series(k,7))
			end do
			box_patterns = 0
			box_scales = 0.5_pp
			box_line_widths = 0.0_pp
		end subroutine doBoxes
	
		subroutine doLines
			lwidths = 1.0_pp
			if(present(lineWidths)) lwidths = real(lineWidths,pp)
			
			do k=1,size(series,1)
				line_colors(k) = getColorCode(series(k,4))
				line_styles(k) = getLineStyleCode(series(k,3))
			end do
		end subroutine doLines
	
		subroutine doMarkers
			mcounts = 2
			if(present(markCounts)) mcounts = markCounts
			mscales = 1.0_pp
			if(present(markScales)) mscales = real(markScales,pp)
			
			do k=1,size(series,1)
				mark_colors(k) = getColorCode(series(k,6))
				mark_styles(k) = getSymbolCode(series(k,5))
			end do
		end subroutine doMarkers
	
		function getCorner(text) result(code)
			character(*),intent(in)::text
			integer::code
			
			code = PL_POSITION_INSIDE
			if( startsWith(text,'upper') ) code = code+PL_POSITION_TOP
			if( startsWith(text,'lower') ) code = code+PL_POSITION_BOTTOM
			if(   endsWith(text,'right') ) code = code+PL_POSITION_RIGHT
			if(   endsWith(text,'left' ) ) code = code+PL_POSITION_LEFT
		end function getCorner
	
	end subroutine legend

	!========================!
	!= Drawing Pen Routines =!
	!========================!

	subroutine resetPen
		!! Reset pen to default state
		
		call setColor('')
		call setLineStyle('')
		call setLineWidth(0.5_wp)
		call setFillPattern('')
		call plschr(0.0_pp,real(fontScale,pp))
		call plssym(0.0_pp,real(fontScale,pp))
	end subroutine resetPen

	subroutine setLineWidth(lineWidth)
		real(wp),intent(in)::lineWidth
		
		call plwidth(real(lineWidth,pp))
	end subroutine setLineWidth

	subroutine setLineStyle(style)
		!! Set the current pen line style
		character(*),intent(in)::style
			!! Style to set
		
		call pllsty(getLineStyleCode(style))
	end subroutine setLineStyle

	function getLineStyleCode(style) result(code)
		!! Return the code for a line style
		character(*),intent(in)::style
			!! Style desired
		integer::code
		
		select case(style)
		case('-')
			code = 1
		case(':')
			code = 2
		case('--')
			code = 3
		case default
			code = 1
		end select
	end function getLineStyleCode

	function getSymbolCode(style) result(code)
		!! Return the code for a symbol style
		character(*),intent(in)::style
			!! Style desired
		character(32)::code
		
		select case(style)
		case('+')
			code = '#(140)'
		case('x')
			code = '#(141)'
		case('*')
			code = '#(142)'
		case('.')
			code = '#(143)'
		case('s')
			code = '#(144)'
		case(',')
			code = '#(850)'
		case('^')
			code = '#(852)'
		case('<')
			code = '#(853)'
		case('v')
			code = '#(854)'
		case('>')
			code = '#(855)'
		case default
			code = '#(143)'
		end select
	end function getSymbolCode

	subroutine setFillPattern(style)
		character(*),intent(in)::style
		
		call plpsty(getFillCode(style))
	end subroutine setFillPattern

	function getFillCode(style) result(code)
		character(*),intent(in)::style
		integer::code
		
		select case(style)
		case('')
			code = 0
		case('-')
			code = 1
		case('/')
			code = 3
		case('|')
			code = 2
		case('\')
			code = 4
		case('#')
			code = 7
		case('x')
			code = 8
		case default
			code = 0
		end select
	end function getFillCode

	subroutine setColor(color)
		!! Set the current pen color
		character(*),intent(in)::color
			!! Name of color to set
		
		integer::ios
		real(pp)::v
		
		read(color,*,iostat=ios) v
		if(ios==0) then
			call plcol1(v)
		else
			call plcol0(getColorCode(color))
		end if
	end subroutine setColor

	function getColorCode(color) result(code)
		character(*),intent(in)::color
		integer::code
		
		select case(color)
		case('w','white')
			if(blackOnWhite) then
				code = 1
			else
				code = 2
			end if
		case('k','black')
			if(blackOnWhite) then
				code = 2
			else
				code = 1
			end if
		case('r','red')
			code = 3
		case('g','green')
			code = 4
		case('b','blue')
			code= 5
		case('c','cyan')
			code = 6
		case('m','magenta')
			code= 7
		case('y','yellow')
			code = 8
		case('fg')
			code = 2
		case('bg')
			code = 1
		case default
			code = 2
		end select
		
		code = code-1
	end function getColorCode

	!===========================!
	!= Library Status Routines =!
	!===========================!

	subroutine setup(device,fileName,fontScaling,whiteOnBlack,transparent,colormap,figSize)
		!! Setup PlPlot library, optionally overriding defaults
		character(*),intent(in),optional::device
			!! Output device to use
			!!
			!! * qtwidget
			!! * svgqt
			!! * pngqt
		character(*),intent(in),optional::fileName
			!! Name of file(s) to write to
			!! 
			!! The text `%n` will be replaced with the figure number
		real(wp),intent(in),optional::fontScaling
			!! Font scaling relative to default value
		logical,intent(in),optional::whiteOnBlack
			!! Default foreground and background colors
		logical,intent(in),optional::transparent
			!! Transparent background
		character(*),intent(in),optional::colormap
			!! Colormap to use
		integer,dimension(2),intent(in),optional::figSize
			!! Size of figures to produce in pixels
		
		character(64)::bufx,bufy
		
		if(present(device)) then
			call plsdev(device)
		else
			call plsdev(default_dev)
		end if
		
		call plsfam(1,1,100)
		if(present(fileName)) then
			call plsfnam(fileName)
		else
			call plsfnam('out')
		end if
		
		if(present(whiteOnBlack)) blackOnWhite = .not. whiteOnBlack
		
		if(present(transparent)) transparentBackground = transparent
		
		call setIndexedColors()
		
		if(present(colormap)) then
			call setColormap(colormap)
		else
			call setColormap('CoolWarm')
		end if
		
		call plfontld(0)
		if(present(fontScaling)) fontScale = real(fontScaling,pp)
		
		if(present(figSize)) then
			write(bufx,*) figSize(1)
			write(bufy,*) figSize(2)
			call plsetopt('geometry',trim(adjustl(bufx))//'x'//trim(adjustl(bufy)))
		else
			call plsetopt('geometry','640x480')
		end if
		
		call plinit()
		
		call resetPen()
	end subroutine setup

	subroutine show
		!! Show the plots end finialize the PlPlot library
		if(.not.didShow) then
			call plend()
			didShow = .true.
		end if
	end subroutine show

	!======================!
	!= Color Map Routines =!
	!======================!

	subroutine setIndexedColors
		!! Setup the indexed colors
		integer,dimension(8,3)::rgb
		real(plflt),dimension(8)::a
		
		rgb(getColorCode('w')+1,:) = [255,255,255] ! White
		rgb(getColorCode('k')+1,:) = [  0,  0,  0] ! Black
		rgb(getColorCode('r')+1,:) = [255,  0,  0] ! Red
		rgb(getColorCode('g')+1,:) = [  0,255,  0] ! Green
		rgb(getColorCode('b')+1,:) = [  0,  0,255] ! Blue
		rgb(getColorCode('c')+1,:) = [  0,255,255] ! Cyan
		rgb(getColorCode('m')+1,:) = [255,  0,255] ! Magenta
		rgb(getColorCode('y')+1,:) = [255,255,  0] ! Yellow
		
		a = 1.0_plflt
		if(transparentBackground) a(1) = 0.0_wp
		
		call plscmap0a(rgb(:,1),rgb(:,2),rgb(:,3),a)
	end subroutine setIndexedColors

	subroutine setColormap(colormap)
		!! Set the continuous colormap
		character(*),intent(in)::colormap
			!! Name of colormap to use
		
		real(pp),dimension(:),allocatable::i,h,s,v
		
		select case(colormap)
		case('CoolWarm')
			h = [240.0,195.0,45.0,0.0]
			
			s = [0.60, 0.95, 0.95, 0.60]
			v = [0.80, 0.30, 0.30, 0.80]
			i = [0.00, 0.50, 0.50, 1.00]
			
			call plscmap1n(256)
			call plscmap1l(.false.,i,h,s,v)
		case('Gray')
			call plspal1('cmap1_gray.pal',1)
		case('BlueYellow')
			call plspal1('cmap1_blue_yellow.pal',1)
		case('BlueRed')
			call plspal1('cmap1_blue_red.pal',1)
		case('Radar')
			call plspal1('cmap1_radar.pal',1)
		case('HighFreq')
			call plspal1('cmap1_highfreq.pal',1)
		case('LowFreq')
			call plspal1('cmap1_lowfreq.pal',1)
		end select
	end subroutine setColormap

end module plplotlibBase_mod
