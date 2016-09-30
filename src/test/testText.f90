program testText_prg
	!! Test program for text_mod
	use text_mod
	implicit none
	
	call testRemoveSpaces
	
	call testStartsWith
	call testEndsWith
	
	call testIntToChar
	call testRealToChar
	call testRealToTime
	
	call testColorize
	call testColorMap
	
contains

	subroutine testRemoveSpaces
		!! Test removeSpaces to verify operation
		!! @todo
		!! Make me a test with stop conditions
		character(:),allocatable::s1,s2
		
		s1 = 'This Is A Test'
		s2 = removeSpaces(s1)
		
		write(*,*) '|'//s2//'|'
	end subroutine testRemoveSpaces

	subroutine testStartsWith
		!! Test startsWith to verify operation
		logical,dimension(4)::results
		
		results(1) = startsWith('thisFunction','this')
		results(2) = .not.startsWith('thisFunction','that')
		results(3) = .not.startsWith('','this')
		results(4) = startsWith('thisFunction','')
		
		if( .not.all(results) ) error stop "Failed startsWith check"
	end subroutine testStartsWith

	subroutine testEndsWith
		!! Test endsWith to verify operation
		logical,dimension(4)::results
		
		results(1) = endsWith('thisFunction','Function')
		results(2) = .not.endsWith('thisFunction','Subroutine')
		results(3) = .not.endsWith('','this')
		results(4) = endsWith('thisFunction','')
		
		if( .not.all(results) ) error stop "Failed endsWith check"
	end subroutine testEndsWith

	subroutine testIntToChar
		!! Test intToChar to verify operation
		logical,dimension(4)::results
		
		results(1) = intToChar(1)=='1'
		results(2) = intToChar(-1)=='-1'
		results(3) = intToChar(1,'(1I4.4)')=='0001'
		results(4) = intToChar(1,'(1I4.4)',6)=='0001  '
		
		if( .not.all(results) ) error stop "Failed intToChar check"
	end subroutine testIntToChar

	subroutine testRealToChar
		!! Test realToChar to verify operation
		logical,dimension(2)::results
		
		results(1) = realToChar(1.0_wp,'(1F10.5)')=='1.00000'
		results(2) = realToChar(1.0_wp,'(1F10.5)',20)=='1.00000             '
		
		if( .not.all(results) ) error stop "Failed realToChar check"
	end subroutine testRealToChar

	subroutine testRealToTime
		!! Test realToTime to verify operation
		logical,dimension(1)::results
		
		results(1) = realToTime(3600.0_wp*24.0_wp+3600.0_wp*2.0_wp+60.0_wp*3.0_wp+4.0_wp)=='1d 2h 3m 4s'
		
		if( .not.all(results) ) error stop "Failed realToTime check"
	end subroutine testRealToTime

	subroutine testColorize
		!! Test colorize to verify operation
		character(:),allocatable::test,true
		
		character(1),parameter::ESC = achar(27)
		
		test = colorize('white',[5,5,5])
		true = ESC//'[38;5;'//'231'//'m'//'white'//ESC//'[0m'
		
		if( .not.(test==true) ) error stop "Failed colorize check"
	end subroutine testColorize

	subroutine testColorMap
		!! Test colorMap to verify operation
		integer,parameter::N = 11
		integer,dimension(N,3),parameter::true = & 
			& reshape([ 0,1,2,3,4,5,5,5,5,5,5, &
			&           0,1,2,3,4,5,4,3,2,1,0, &
			&           5,5,5,5,5,5,4,3,2,1,0 ],[N,3])
		real(wp),dimension(2),parameter::R = [0.0_wp,1.0_wp]
		
		integer,dimension(N,3)::test
		real(wp)::x
		integer::k
		
		do k=1,N
			x = real(k-1,wp)/real(N-1,wp)
			test(k,1:3) = colorMap(x,R)
		end do
		
		if( .not.all(true==test) ) error stop "Failed colorMap check"
	end subroutine testColorMap

end program testText_prg
