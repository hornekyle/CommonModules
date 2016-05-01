module text_mod
	!! Text processing module
	use iso_fortran_env
	use kinds_mod
	use time_mod
	implicit none
	private
	
	!==============!
	!= Parameters =!
	!==============!
	
	integer,parameter::stdin  = INPUT_UNIT
	integer,parameter::stdout = OUTPUT_UNIT
	integer,parameter::stderr = ERROR_UNIT
	
	integer,parameter::strLong = 128
	integer,parameter::strShort = 32
	
	character(:),parameter::fmtLong = '(1A128)'
	character(:),parameter::fmtShort = '(1A32)'
	
	!===========!
	!= Exports =!
	!===========!
	
	public::stdin
	public::stdout
	public::stderr
	
	public::strLong
	public::strShort
	
	public::fmtLong
	public::fmtShort
	
	public::startsWith
	public::endsWith
	
	public::intToChar
	public::realToChar
	public::realToTime
	
	public::colorize
	public::colorMap
	
	public::showProgress
	
contains

	function startsWith(text,str) result(o)
		!! Test if text starts with str
		character(*),intent(in)::text
			!! Text to search
		character(*),intent(in)::str
			!! String to look for
		logical::o
		integer::k
		
		if(len(str)==0) then
			o = .true.
		else if(len(text)==0) then
			o = .false.
		else
			k = len(str)
			o = text(1:k)==str
		end if
		
	end function startsWith

	function endsWith(text,str) result(o)
		!! Test if text ends with str
		character(*),intent(in)::text
			!! Text to search
		character(*),intent(in)::str
			!! String to look for
		logical::o
		integer::k
		
		if(len(str)==0) then
			o = .true.
		else if(len(text)==0) then
			o = .false.
		else
			k = len(text)
			o = text(k-len(str)+1:k)==str
		end if
		
	end function endsWith

	elemental function intToChar(a,f,l) result(o)
		!! Create a string from an integer
		integer,intent(in)::a
			!! Integer value to convert
		character(*),optional,intent(in)::f
			!! Format to use
		integer,optional,intent(in)::l
			!! Final length of string
		character(:),allocatable::o
		
		character(128)::buf
		
		if(present(l)) then
			allocate(character(l)::o)
			if(present(f)) then
				write(o,'('//f//')') a
			else
				write(o,*) a
			end if
			o = adjustl(o)
		else
			if(present(f)) then
				write(buf,'('//f//')') a
			else
				write(buf,*) a
			end if
			o = trim(adjustl(buf))
		end if
	end function intToChar

	elemental function realToChar(a,f,l) result(o)
		!! Create a string from a real number
		real(wp),intent(in)::a
			!! Real value to convert
		character(*),optional,intent(in)::f
			!! Format to use
		integer,optional,intent(in)::l
			!! Final string length
		character(:),allocatable::o
		
		character(128)::buf
		
		if(present(l)) then
			allocate(character(l)::o)
			if(present(f)) then
				write(o,'('//f//')') a
			else
				write(o,*) a
			end if
			o = adjustl(o)
		else
			if(present(f)) then
				write(buf,'('//f//')') a
			else
				write(buf,*) a
			end if
			o = trim(adjustl(buf))
		end if
	end function realToChar

	elemental function realToTime(a) result(o)
		!! Convert a real number to a string
		real(wp),intent(in)::a
			!! Time span in seconds
		character(:),allocatable::o
		
		integer::d,r,h,m,s
		
		r = nint(a)
		
		d = r/(3600*24)
		r = mod(r,3600*24)
		
		h = r/3600
		r = mod(r,3600)
		
		m = r/60
		r = mod(r,60)
		
		s = r
		
		o = ''
		if(d>0) o = o//intToChar(d)//'d '
		if(h>0.or.d>0) o = o//intToChar(h)//'h '
		if(m>0.or.h>0.or.d>0) o = o//intToChar(m)//'m '
		o = o//intToChar(s)//'s'
	end function realToTime

	function colorize(s,c) result(o)
		!! Bracket a string with text to change its color on a terminal
		character(*),intent(in)::s
			!! String to colorize
		integer,dimension(3),intent(in)::c ! c in [0,5]
			!! Color to use in [r,g,b] format, where \(r,b,g \in [0,5]\)
		character(:),allocatable::o
		
		character(1),parameter::CR  = achar(13)
		character(1),parameter::ESC = achar(27)
		character(3),parameter::post = '[0m'
		
		character(:),allocatable::pre
		
		pre = ESC//'[38;5;'//intToChar(36*c(1)+6*c(2)+c(3)+16)//'m'
		o = trim(pre)//s//ESC//post
	end function colorize

	subroutine showProgress(m,p,ml)
		!! Create a progress bar through successive calls
		character(*),intent(in)::m
			!! Message to display
		real(wp),intent(in)::p
			!! Progress fraction \(p\in[0,1]\)  
			!! 0 = start progress  
			!! 1 = complete progress
		integer,intent(in),optional::ml
			!! Message reserve length (used to align long messages)
		
		real(wp)::r
		real(wp),save::po
		real(wp),save::tStart
		real(wp)::tNow
		integer::mld
		integer::N,k
		
		N = 40
		mld = 40
		if(present(ml)) mld = ml
		
		if(p<=0.0_wp) then
!~ 			write(stdout,'(1A)') ''
			po = p
			tStart = wallTime()
		else if(p-po<=0.01 .and. p<1.0_wp) then
			return
		else
			po = p
		end if
		tNow = wallTime()
		
		write(stdout,'(1A)',advance='no') achar(13)//colorize(m//repeat(' ',mld-len(m))//' [',[5,5,0])
		do k=1,N
			r = real(k-1,wp)/real(N-1,wp)
			if(r<=p) then
				write(stdout,'(1A)',advance='no') colorize('=',colorMap(r,[0.0_wp,1.0_wp]))
			else
				write(stdout,'(1A)',advance='no') colorize(' ',[0,0,0])
			end if
		end do
		write(stdout,'(1A,1A,1X,1A,1A,1A,1A,1A,1A)',advance='no') colorize('] ',[5,5,0]), &
		& colorize(realToChar(100.0_wp*p,'1F5.1'),colorMap(p,[0.0_wp,1.0_wp])), &
		& colorize('%',[5,5,0]), colorize(' (',[5,5,0]), realToTime(tNow-tStart), &
		& colorize(' / ',[5,5,0]), realToTime( (tNow-tStart)/(p+0.0001_wp) ), colorize(')',[5,5,0])
		if(p>=1.0_wp) write(stdout,'(1A)') ''
		flush(stdout)
	end subroutine showProgress

	function colorMap(v,r) result(c)
		!! Return the color code for colorize based on the coolwarm color map
		real(wp),intent(in)::v
			!! Value to map
		real(wp),dimension(2),intent(in)::r
			!! Range over which to scale the colors
		integer,dimension(3)::c
		
		integer::s
		
		if(v<sum(r)/2.0_wp) then
			s = nint((v-r(1))/(sum(r)/2.0_wp-r(1))*5.0_wp)
			c = [s,s,5]
		else
			s = 5-nint((v-sum(r)/2.0_wp)/(r(2)-sum(r)/2.0_wp)*5.0_wp)
			c = [5,s,s]
		end if
	end function colorMap

end module text_mod
