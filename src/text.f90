module text_mod
	use iso_fortran_env
	use kinds_mod
	implicit none
	
	integer,parameter::stdin  = INPUT_UNIT
	integer,parameter::stdout = OUTPUT_UNIT
	integer,parameter::stderr = ERROR_UNIT
	
	integer,parameter::str_long = 128
	integer,parameter::str_short = 32
	
	character(:),parameter::fmt_long = '(1A128)'
	character(:),parameter::fmt_short = '(1A32)'
	
contains

	function startsWith(text,str) result(o)
		!! Test if text starts with str
		character(*),intent(in)::text
			!! Text to search
		character(*),intent(in)::str
			!! String to look for
		logical::o
		integer::k
		
		k = len(str)
		o = text(1:k)==str
	end function startsWith

	function endsWith(text,str) result(o)
		!! Test if text ends with str
		character(*),intent(in)::text
			!! Text to search
		character(*),intent(in)::str
			!! String to look for
		logical::o
		integer::k
		
		k = len(text)
		o = text(k-len(str)+1:k)==str
	end function endsWith

	function colorize(s,c) result(o)
		character(*),intent(in)::s
		integer,dimension(3)::c ! c in [0,5]
		character(:),allocatable::o
		
		character(1),parameter::CR  = achar(13)
		character(1),parameter::ESC = achar(27)
		
		character(20)::pre
		character(3)::cb
		
		write(cb,'(1I3)') 36*c(1)+6*c(2)+c(3)+16
		pre = ESC//'[38;5;'//trim(adjustl(cb))//'m'
		o = trim(pre)//s//ESC//'[0m'
	end function colorize

	elemental function real2char(a,f,l) result(o)
		real(wp),intent(in)::a
		character(*),optional,intent(in)::f
		integer,optional,intent(in)::l
		character(:),allocatable::o
		
		character(128)::buf
		
		if(present(l)) then
			allocate(character(l)::o)
			if(present(f)) then
				write(o,'('//f//')') a
			else
				write(o,*) a
			end if
		else
			if(present(f)) then
				write(buf,'('//f//')') a
			else
				write(buf,*) a
			end if
			o = trim(adjustl(buf))
		end if
	end function real2char

	elemental function real2time(a) result(o)
		real(wp),intent(in)::a
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
		if(d>0) o = o//int2char(d)//'d '
		if(h>0.or.d>0) o = o//int2char(h)//'h '
		if(m>0.or.h>0.or.d>0) o = o//int2char(m)//'m '
		o = o//int2char(s)//'s'
	end function real2time

	elemental function int2char(a,f,l) result(o)
		integer,intent(in)::a
		character(*),optional,intent(in)::f
		integer,optional,intent(in)::l
		character(:),allocatable::o
		
		character(128)::buf
		
		if(present(l)) then
			allocate(character(l)::o)
			if(present(f)) then
				write(o,'('//f//')') a
			else
				write(o,*) a
			end if
		else
			if(present(f)) then
				write(buf,'('//f//')') a
			else
				write(buf,*) a
			end if
			o = trim(adjustl(buf))
		end if
	end function int2char

	subroutine showProgress(m,p,ml)
		character(*),intent(in)::m
		real(wp),intent(in)::p
		integer,intent(in),optional::ml
		
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
				write(stdout,'(1A)',advance='no') colorize('=',cmap(r,[0.0_wp,1.0_wp]))
			else
				write(stdout,'(1A)',advance='no') colorize(' ',[0,0,0])
			end if
		end do
		write(stdout,'(1A,1A,1X,1A,1A,1A,1A,1A,1A)',advance='no') colorize('] ',[5,5,0]), &
		& colorize(real2char(100.0_wp*p,'1F5.1'),cmap(p,[0.0_wp,1.0_wp])), &
		& colorize('%',[5,5,0]), colorize(' (',[5,5,0]), real2time(tNow-tStart), &
		& colorize(' / ',[5,5,0]), real2time( (tNow-tStart)/(p+0.0001_wp) ), colorize(')',[5,5,0])
		if(p>=1.0_wp) write(stdout,'(1A)') ''
		flush(stdout)
	end subroutine showProgress

	function cmap(v,r) result(c)
		real(wp),intent(in)::v
		real(wp),dimension(2),intent(in)::r
		integer,dimension(3)::c
		
		integer::s
		
		if(v<sum(r)/2.0_wp) then
			s = nint((v-r(1))/(sum(r)/2.0_wp-r(1))*5.0_wp)
			c = [s,s,5]
		else
			s = 5-nint((v-sum(r)/2.0_wp)/(r(2)-sum(r)/2.0_wp)*5.0_wp)
			c = [5,s,s]
		end if
	end function cmap

	function wallTime() result(o)
		real(wp)::o
		
		integer,parameter::ip = selected_int_kind(15)
		integer(ip)::ticks,tickRate,r
		
		call system_clock(ticks,tickRate)
		
		r = mod(ticks,tickRate)
		
		o = real(ticks/tickRate,wp)+real(r,wp)/real(tickRate,wp)
	end function wallTime

end module text_mod
