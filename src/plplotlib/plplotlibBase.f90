module plplotlibBase_mod
	!! Wrapper module for plplot to give it a more matplotlib like personality
	use plplot
	use array_mod
	implicit none
	public
	
	integer,parameter::pp = plflt
	integer,parameter::pi = kind(1)
	
	character(*),parameter::default_dev = 'svg'
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
		case('C0')
			code = 9
		case('C1')
			code = 10
		case('C2')
			code = 11
		case('C3')
			code = 12
		case('C4')
			code = 13
		case('C5')
			code = 14
		case('C6')
			code = 15
		case('C7')
			code = 16
		case('C8')
			code = 17
		case('C9')
			code = 18
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
		integer::ierr
		
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
			call setColormap('Viridis')
		end if
		
		call plfontld(0)
		if(present(fontScaling)) fontScale = real(fontScaling,pp)
		
		if(present(figSize)) then
			write(bufx,*) figSize(1)
			write(bufy,*) figSize(2)
			ierr = plsetopt('geometry',trim(adjustl(bufx))//'x'//trim(adjustl(bufy)))
		else
			ierr = plsetopt('geometry','640x480')
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
		integer,dimension(18,3)::rgb
		real(plflt),dimension(18)::a
		
		rgb(getColorCode('w' )+1,:) = [255,255,255] ! White
		rgb(getColorCode('k' )+1,:) = [  0,  0,  0] ! Black
		rgb(getColorCode('r' )+1,:) = [255,  0,  0] ! Red
		rgb(getColorCode('g' )+1,:) = [  0,255,  0] ! Green
		rgb(getColorCode('b' )+1,:) = [  0,  0,255] ! Blue
		rgb(getColorCode('c' )+1,:) = [  0,255,255] ! Cyan
		rgb(getColorCode('m' )+1,:) = [255,  0,255] ! Magenta
		rgb(getColorCode('y' )+1,:) = [255,255,  0] ! Yellow
		
		rgb(getColorCode('C0')+1,:) = hexToRGB('1f77b4') ! C0
		rgb(getColorCode('C1')+1,:) = hexToRGB('ff7f0e') ! C1
		rgb(getColorCode('C2')+1,:) = hexToRGB('2ca02c') ! C2
		rgb(getColorCode('C3')+1,:) = hexToRGB('d62728') ! C3
		rgb(getColorCode('C4')+1,:) = hexToRGB('9647bd') ! C4
		rgb(getColorCode('C5')+1,:) = hexToRGB('8c564b') ! C5
		rgb(getColorCode('C6')+1,:) = hexToRGB('e377c2') ! C6
		rgb(getColorCode('C7')+1,:) = hexToRGB('7f7f7f') ! C7
		rgb(getColorCode('C8')+1,:) = hexToRGB('bcbd22') ! C8
		rgb(getColorCode('C9')+1,:) = hexToRGB('17becf') ! C9
		
		a = 1.0_plflt
		if(transparentBackground) a(1) = 0.0_wp
		
		call plscmap0a(rgb(:,1),rgb(:,2),rgb(:,3),a)
		
	contains
		
		function hexToRGB(h) result(o)
			character(6),intent(in)::h
			integer,dimension(3)::o
			
			read(h(1:2),'(1Z2)') o(1)
			read(h(3:4),'(1Z2)') o(2)
			read(h(5:6),'(1Z2)') o(3)
		end function hexToRGB
		
	end subroutine setIndexedColors

	subroutine setColormap(colormap)
		!! Set the continuous colormap
		character(*),intent(in)::colormap
			!! Name of colormap to use
		
		real(pp),dimension(:),allocatable::i
		real(pp),dimension(:),allocatable::h,s,v
		real(pp),dimension(:),allocatable::r,g,b
		
		select case(colormap)
		case('Viridis')
			r = [0.267004, 0.231674, 0.129933, 0.344074, 0.993248]
			g = [0.004874, 0.318106, 0.559582, 0.780029, 0.906157]
			b = [0.329415, 0.544834, 0.551864, 0.397381, 0.143936]
			i = [0.      , 0.24705882, 0.49411765, 0.74117647, 1.0]
			
			call plscmap1n(256)
			call plscmap1l(.true.,i,r,g,b)
		case('CoolWarm')
			h = [240.0,195.0,45.0,0.0]
			
			s = [0.60, 0.95, 0.95, 0.60]
			v = [0.80, 0.30, 0.30, 0.80]
			i = [0.00, 0.50, 0.50, 1.00]
			
			call plscmap1n(256)
			call plscmap1l(.false.,i,h,s,v)
		case('Gray')
			call plspal1('cmap1_gray.pal',.true.)
		case('BlueYellow')
			call plspal1('cmap1_blue_yellow.pal',.true.)
		case('BlueRed')
			call plspal1('cmap1_blue_red.pal',.true.)
		case('Radar')
			call plspal1('cmap1_radar.pal',.true.)
		case('HighFreq')
			call plspal1('cmap1_highfreq.pal',.true.)
		case('LowFreq')
			call plspal1('cmap1_lowfreq.pal',.true.)
		end select
	end subroutine setColormap

end module plplotlibBase_mod
