module config_mod
	!! Module for reading variables from config files
	!! @todo
	!! Make code fault tolerant
	use kinds_mod
	use text_mod
	implicit none
	private

	type::pair_t
		!! Type to store a single key-value pair and the data's type
		character(100)::key
		
		integer::pType
		
		logical::l = .false.                      ! 0
		integer::i = 0                            ! 1
		real(wp)::r = 0.0_wp                      ! 2
		complex(wp)::c = 0.0_wp                   ! 3
		real(wp),dimension(:),allocatable::v      ! 4
		real(wp),dimension(:,:),allocatable::m    ! 5
		character(strShort)::s = ' '              ! 6
	end type

	type::config_t
		!! Type to store a set of pairs and access their data
		character(strLong)::fn
			!! Filename data was read from
		type(pair_t),dimension(:),allocatable::pairs
			!! Pairs of data
	contains
		procedure::readFile
		procedure::writeContents
		
		procedure::getLogical
		procedure::getInteger
		procedure::getReal
		procedure::getComplex
		procedure::getVector
		procedure::getMatrix
		procedure::getString
		
		procedure::getType
		procedure::isFound
		
		procedure,private::findKey
		procedure,private::sortKeys
	end type
	
	public::config_t
	public::newConfig
	
contains

	function newConfig(fn) result(o)
		!! Read a config file and return it in memory
		character(*),intent(in)::fn
		type(config_t)::o
		
		call o%readFile(fn)
	end function newConfig

	subroutine readFile(self,fn)
		type::node_t
			type(node_t),pointer::next => null()
			type(pair_t)::obj
		end type

		class(config_t),intent(inout)::self
		character(*),intent(in)::fn
		
		integer::ios
		character(strLong)::buf
		type(node_t),target::head
		type(node_t),pointer::cur,next,tail
		integer::N,k
		
		self%fn = fn
		
		N = 0
		tail => head
		open(100,file=fn,status='old',iostat=ios)
		if(ios/=0) return
		read(100,fmtLong,iostat=ios) buf
		do while(ios==0)
			buf = adjustl(buf)
			if(buf(1:1) /= '#' .and. buf(1:1) /= '[' .and. buf(1:1) /= ' ') then
				N = N+1
				allocate(tail%next)
				tail => tail%next
				tail%obj = newPair(buf)
			end if
			read(100,fmtLong,iostat=ios) buf
		end do
		close(100)
		if(allocated(self%pairs)) deallocate(self%pairs)
		allocate(self%pairs(N))
		cur => head%next
		do k=1,N
			self%pairs(k) = cur%obj
			next => cur%next
			if(allocated(cur%obj%v)) deallocate(cur%obj%v)
			if(allocated(cur%obj%m)) deallocate(cur%obj%m)
			deallocate(cur)
			cur => next
		end do
		call self%sortKeys()
	end subroutine readFile

	function newPair(b) result(p)
		character(*),intent(inout)::b
		type(pair_t)::p
		
		character(1024)::v
		integer::N,k,Nr,Nc
		
		p%key = trim(adjustl(b(1:index(b,'=')-1)))
		v = adjustl(b(index(b,'=')+1:len(b)))
		p%s = trim(v)
		p%ptype = parseType(trim(v))
		select case(p%ptype)
		case(0)
			read(v,*) p%l
		case(1)
			read(v,*) p%i
		case(2)
			read(v,*) p%r
		case(3)
			read(v,*) p%c
		case(4)
			N = 0
			do k=1,len(v)
				if(v(k:k)=='[' .or. v(k:k)==']') v(k:k) = ' '
				if(v(k:k)=='.') N = N+1
			end do
			allocate(p%v(N))
			read(v,*) p%v
		case(5)
			do k=1,len(v)
				if(verify(v(k:k),'0123456789')/=0) v(k:k) = ' '
			end do
			read(v,*) Nr,Nc
			allocate(p%m(Nr,Nc))
			do N=1,Nr
				read(100,'(1A1024)') v
				do k=1,1024
					if(v(k:k)=='[' .or. v(k:k)==']') v(k:k) = ' '
				end do
				read(v,*) p%m(N,:)
			end do
		case(6)
			do k=1,len(v)
				if(v(k:k)=='''' .or. v(k:k)=='"') then
					v(k:k) = ' '
					exit
				end if
			end do
			do k=len(v),1,-1
				if(v(k:k)=='''' .or. v(k:k)=='"') then
					v(k:k) = ' '
					exit
				end if
			end do
			p%s = trim(adjustl(v))
		end select
	end function newPair

	function parseType(v) result(t)
		character(*),intent(in)::v
		integer::t
		
		integer::N
		
		N = len(v)

		if(v(1:1)=='[' .and. v(N:N)==']' .and. verify(v,' +-.E0123456789[,]')==0) then
			t = 4
		else if(v(1:1)=='(' .and. v(N:N)==')' .and. verify(v,' +-.E0123456789(,)')==0) then
			t = 3
		else if(v(1:1)=='''' .and. v(N:N)=='''') then
			t = 6
		else if(v(1:1)=='"' .and. v(N:N)=='"') then
			t = 6
		else if(verify(v,' +-0123456789')==0) then
			t = 1
		else if(verify(v,' +-.E0123456789')==0) then
			t = 2
		else if(verify(v,' MATRIXmatrix0123456789(,)')==0) then
			t = 5
		else if(verify(v,' .TRUEtrueFALSEfalse')==0) then
			t = 0
		else
			t = -1
		end if
	end function parseType

	subroutine sortKeys(self)
		class(config_t),intent(inout)::self
		type(pair_t)::t
		integer::k,p
		
		do p=0,size(self%pairs)/2
			do k=1+p,size(self%pairs)-p-1
				if(self%pairs(k)%key>self%pairs(k+1)%key) then
					t = self%pairs(k+1)
					self%pairs(k+1) = self%pairs(k)
					self%pairs(k) = t
				end if
			end do
			do k=size(self%pairs)-p-1,1+p,-1
				if(self%pairs(k)%key>self%pairs(k+1)%key) then
					t = self%pairs(k+1)
					self%pairs(k+1) = self%pairs(k)
					self%pairs(k) = t
				end if
			end do
		end do
	end subroutine sortKeys

	subroutine writeContents(self,iou)
		class(config_t),intent(inout)::self
		integer,intent(in)::iou
		
		integer::k
		
		do k=1,size(self%pairs)
			write(iou,'(1A)',advance='no') trim(self%pairs(k)%key)//' = '
			select case(self%pairs(k)%ptype)
			case(0)
				if(self%pairs(k)%l) then
					write(iou,*) 'TRUE'
				else
					write(iou,*) 'FALSE'
				end if
			case(1)
				write(iou,*) self%pairs(k)%i
			case(2)
				write(iou,*) self%pairs(k)%r
			case(3)
				write(iou,*) self%pairs(k)%c
			case(4)
				write(iou,*) '[',self%pairs(k)%v,']'
			case(5)
				write(iou,*) 'Matrix: (',shape(self%pairs(k)%m),')'
			case(6)
				write(iou,*) ''''//trim(self%pairs(k)%s)//''''
			case default
				write(iou,*) 'Error'
			end select
		end do
	end subroutine writeContents

	function isFound(self,key) result(o)
		class(config_t),intent(in)::self
		character(*),intent(in)::key
		logical::o
		
		o = self%findKey(key)>0
	end function isFound

	function getType(self,key) result(o)
		class(config_t),intent(in)::self
		character(*),intent(in)::key
		integer::o
	
		o = self%pairs(self%findKey(key))%pType
	end function getType

	function getLogical(self,key) result(o)
		class(config_t),intent(in)::self
		character(*),intent(in)::key
		logical::o
	
		o = self%pairs(self%findKey(key))%l
	end function getLogical

	function getInteger(self,key) result(o)
		class(config_t),intent(in)::self
		character(*),intent(in)::key
		integer::o
		
		o = self%pairs(self%findKey(key))%i
	end function getInteger

	function getReal(self,key) result(o)
		class(config_t),intent(in)::self
		character(*),intent(in)::key
		real(wp)::o
		
		o = self%pairs(self%findKey(key))%r
	end function getReal

	function getComplex(self,key) result(o)
		class(config_t),intent(in)::self
		character(*),intent(in)::key
		complex(wp)::o
		
		o = self%pairs(self%findKey(key))%c
	end function getComplex
	
	function getVector(self,key) result(o)
		class(config_t),intent(in)::self
		character(*),intent(in)::key
		real(wp),dimension(:),allocatable::o
		
		integer::k
		
		k = self%findKey(key)
		allocate(o(size(self%pairs(k)%v)))
		o = self%pairs(k)%v
	end function getVector
	
	function getMatrix(self,key) result(o)
		class(config_t),intent(in)::self
		character(*),intent(in)::key
		real(wp),dimension(:,:),allocatable::o

		integer::k
		
		k = self%findKey(key)
		allocate(o(size(self%pairs(k)%m,1),size(self%pairs(k)%m,2)))
		o = self%pairs(k)%m
	end function getMatrix
	
	function getString(self,key) result(o)
		class(config_t),intent(in)::self
		character(*),intent(in)::key
		character(1024)::o
		
		o = self%pairs(self%findKey(key))%s
	end function getString

	function findKey(self,key) result(idx)
		class(config_t),intent(in)::self
		character(*),intent(in)::key
		integer::idx
		
		integer::L,M,H
		logical::found
		
		L = 1
		M = size(self%pairs)/2+1
		H = size(self%pairs)
		found = .false.
		
		idx = 0
		do while(.not.found)
			if(key==self%pairs(L)%key) then
				idx = L
				found = .true.
			else if(key==self%pairs(M)%key) then
				idx = M
				found = .true.
			else if(key==self%pairs(H)%key) then
				idx = H
				found = .true.
			else if(H-L<=2) then
				exit
			else if(key<self%pairs(M)%key) then
				H = M
				M = (L+H)/2+1
			else if(key>self%pairs(M)%key) then
				L = M
				M = (L+H)/2+1
			end if
		end do
		if(idx==0) error stop 'Key not found'
	end function findKey

end module config_mod
