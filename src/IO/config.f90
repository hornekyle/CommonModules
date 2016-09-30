module config_mod
	!! Module for reading variables from config files
	!!
	!! @todo
	!! Change string to character
	!! Change pair_t component names to data_[type]
	use kinds_mod
	use text_mod
	implicit none
	private
	
	!==============================!
	!= pair_t Type and Interfaces =!
	!==============================!
	
	type::pair_t
		!! Type to store a single key-value pair and the data's type
		character(:),allocatable::key
			!! Key of pair
		integer::pType = -1
			!! Type of data in pair
		logical::l     =  .false.
			!! Logical data component
		integer::i     =  0
			!! Integer data component
		real(wp)::r    =  0.0_wp
			!! Real data component
		complex(wp)::c =  0.0_wp
			!! Complex data component
		real(wp),dimension(:),allocatable::v
			!! Vector data component
		real(wp),dimension(:,:),allocatable::m
			!! Matrix data component
		character(:),allocatable::s
			!! String data component
	end type
	
	interface pair_t
		!! Constructor for pair_t
		module procedure newPair
	end interface
	
	integer,parameter::PT_LOGICAL = 0
	integer,parameter::PT_INTEGER = 1
	integer,parameter::PT_REAL    = 2
	integer,parameter::PT_COMPLEX = 3
	integer,parameter::PT_VECTOR  = 4
	integer,parameter::PT_MATRIX  = 5
	integer,parameter::PT_STRING  = 6
	
	!================================!
	!= config_t Type and Interfaces =!
	!================================!
	
	type::config_t
		!! Type to store a set of pairs and access their data
		character(:),allocatable::fn
			!! Filename data was read from
		type(pair_t),dimension(:),allocatable::pairs
			!! Pairs of data
	contains
		procedure::isFound
		procedure::getType
		
		procedure::getLogical
		procedure::getInteger
		procedure::getReal
		procedure::getComplex
		procedure::getVector
		procedure::getMatrix
		procedure::getString
		
		procedure::writeContents
	end type
	
	interface config_t
		!! Constructor for config_t
		module procedure newConfig
	end interface
	
	!===========!
	!= Exports =!
	!===========!
	
	public::config_t
	
	! Kinds
	public::wp
	
contains

	!=======================!
	!= pair_t Constructors =!
	!=======================!

	function newPair(b,s) result(self)
		!! Constructor for pair_t
		character(*),intent(inout)::b
		character(*),intent(in)::s
		type(pair_t)::self
		
		character(1024)::v
		
		self%key   = s//trim(adjustl(b(1:index(b,'=')-1)))
		v       = adjustl(b(index(b,'=')+1:len(b)))
		self%ptype = parseType(trim(v))
		
		select case(self%ptype)
		case(PT_LOGICAL)
			read(v,*) self%l
		case(PT_INTEGER)
			read(v,*) self%i
		case(PT_REAL)
			read(v,*) self%r
		case(PT_COMPLEX)
			read(v,*) self%c
		case(PT_VECTOR)
			call doVector
		case(PT_MATRIX)
			call doMatrix
		case(PT_STRING)
			call doString
		end select
		
	contains
	
		function parseType(v) result(t)
			!! Return type of entry
			character(*),intent(in)::v
			integer::t
			
			integer::N
			
			N = len(v)

			if(v(1:1)=='[' .and. v(N:N)==']' .and. verify(v,' +-.E0123456789[,]')==0) then
				t = PT_VECTOR
			else if(v(1:1)=='(' .and. v(N:N)==')' .and. verify(v,' +-.E0123456789(,)')==0) then
				t = PT_COMPLEX
			else if(v(1:1)=='''' .and. v(N:N)=='''') then
				t = PT_STRING
			else if(v(1:1)=='"' .and. v(N:N)=='"') then
				t = PT_STRING
			else if(verify(v,' +-0123456789')==0) then
				t = PT_INTEGER
			else if(verify(v,' +-.E0123456789')==0) then
				t = PT_REAL
			else if(verify(v,' MATRIXmatrix0123456789(,)')==0) then
				t = PT_MATRIX
			else if(verify(v,' .TRUEtrueFALSEfalse')==0) then
				t = PT_LOGICAL
			else
				t = -1
			end if
		end function parseType
	
		subroutine doVector
			!! Read a vector
			integer::N,k
			
			N = 0
			do k=1,len(v)
				if(v(k:k)=='[' .or. v(k:k)==']') v(k:k) = ' '
				if(v(k:k)=='.') N = N+1
			end do
			
			allocate(self%v(N))
			read(v,*) self%v
		end subroutine doVector
	
		subroutine doMatrix
			!! Read a matrix
			integer::N,k,Nr,Nc
			
			do k=1,len(v)
				if(verify(v(k:k),'0123456789')/=0) v(k:k) = ' '
			end do
			
			read(v,*) Nr,Nc
			allocate(self%m(Nr,Nc))
			
			do N=1,Nr
				read(100,'(1A1024)') v
				do k=1,1024
					if(v(k:k)=='[' .or. v(k:k)==']') v(k:k) = ' '
				end do
				read(v,*) self%m(N,:)
			end do
		end subroutine doMatrix
	
		subroutine doString
			!! Read a string
			integer::k
			
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
			self%s = trim(adjustl(v))
		end subroutine doString
	
	end function newPair

	!=========================!
	!= config_t Constructors =!
	!=========================!

	function newConfig(fn) result(self)
		!! Constructor for config_t
		type::node_t
			type(node_t),pointer::next => null()
			type(pair_t)::obj
		end type

		character(*),intent(in)::fn
			!! Name of file to read
		type(config_t)::self
			!! Returned config_t object
		
		integer::ios
		character(strLong)::buf
		type(node_t),target::head
		type(node_t),pointer::cur,next,tail
		character(:),allocatable::section
		integer::N,k
		
		self%fn = fn
		
		open(100,file=fn,status='old',iostat=ios)
		if(ios/=0) call doError('Cannot open file: '//fn)
		section = ''
		
		! Read pairs into linked list
		N    =  0
		tail => head
		read(100,fmtLong,iostat=ios) buf
		do while(ios==0)
			buf = adjustl(buf)
			if(buf(1:1) /= '#' .and. buf(1:1) /= '[' .and. buf(1:1) /= ' ') then
				N = N+1
				allocate(tail%next)
				
				tail     => tail%next
				tail%obj =  pair_t(buf,section)
			end if
			if(buf(1:1)=='[' .and. index(buf,']')>0) then
				k = index(buf,']')
				section = trim(buf(2:k-1))//'.'
			end if
			read(100,fmtLong,iostat=ios) buf
		end do
		close(100)
		
		! Copy pairs into array and free list
		allocate(self%pairs(N))
		cur => head%next
		do k=1,N
			self%pairs(k) = cur%obj
			
			next => cur%next
			deallocate(cur)
			cur => next
		end do
		
		call sortKeys(self)
	end function newConfig

	!=====================!
	!= config_t Routines =!
	!=====================!

	function isFound(self,key) result(o)
		!! Check for presence of a key in config
		class(config_t),intent(in)::self
			!! Config to check in
		character(*),intent(in)::key
			!! Key to check
		logical::o
			!! Presence of key
		
		o = findKey(self,key)>0
	end function isFound

	function getType(self,key) result(o)
		!! Get the type of an entry for a key
		class(config_t),intent(in)::self
			!! Config to check in
		character(*),intent(in)::key
			!! Key to check
		integer::o
			!! Type of data for key
		
		integer::idx
		
		idx = findKey(self,key)
		o = self%pairs(idx)%pType
	end function getType

	function getLogical(self,key) result(o)
		!! Return a logical value from a config
		class(config_t),intent(in)::self
			!! Config to search in
		character(*),intent(in)::key
			!! Key to search for
		logical::o
			!! Logical value for key
		
		integer::idx
		
		idx = findKey(self,key)
		if(idx<1 .or. idx>size(self%pairs)) call doError('Invalid index for key: '//key)
		if(self%pairs(idx)%pType/=PT_LOGICAL) call doError('Data not LOGICAL type: '//key)
		
		o = self%pairs(idx)%l
	end function getLogical

	function getInteger(self,key) result(o)
		!! Return an integer value from a config
		class(config_t),intent(in)::self
			!! Config to search in
		character(*),intent(in)::key
			!! Key to search for
		integer::o
			!! Integer value for key
		
		integer::idx
		
		idx = findKey(self,key)
		if(idx<1 .or. idx>size(self%pairs)) call doError('Invalid index for key: '//key)
		if(self%pairs(idx)%pType/=PT_INTEGER) call doError('Data not INTEGER type: '//key)
		
		o = self%pairs(idx)%i
	end function getInteger

	function getReal(self,key) result(o)
		!! Return a real value from a config
		class(config_t),intent(in)::self
			!! Config to search in
		character(*),intent(in)::key
			!! Key to search for
		real(wp)::o
			!! Read value for key
		
		integer::idx
		
		idx = findKey(self,key)
		if(idx<1 .or. idx>size(self%pairs)) call doError('Invalid index for key: '//key)
		if(self%pairs(idx)%pType/=PT_REAL) call doError('Data not REAL type: '//key)
		
		o = self%pairs(idx)%r
	end function getReal

	function getComplex(self,key) result(o)
		!! Return a complex value from a config
		class(config_t),intent(in)::self
			!! Config to search in
		character(*),intent(in)::key
			!! Key to search for
		complex(wp)::o
			!! Complex value for key
		
		integer::idx
		
		idx = findKey(self,key)
		if(idx<1 .or. idx>size(self%pairs)) call doError('Invalid index for key: '//key)
		if(self%pairs(idx)%pType/=PT_COMPLEX) call doError('Data not COMPLEX type: '//key)
		
		o = self%pairs(idx)%c
	end function getComplex
	
	function getVector(self,key) result(o)
		!! Return a real vector value from a config
		class(config_t),intent(in)::self
			!! Config to search in
		character(*),intent(in)::key	
			!! Key to search for
		real(wp),dimension(:),allocatable::o
			!! Real vector value for key
		
		integer::idx
		
		idx = findKey(self,key)
		if(idx<1 .or. idx>size(self%pairs)) call doError('Invalid index for key: '//key)
		if(self%pairs(idx)%pType/=PT_VECTOR) call doError('Data not VECTOR type: '//key)

		allocate(o(size(self%pairs(idx)%v)))
		o = self%pairs(idx)%v
	end function getVector
	
	function getMatrix(self,key) result(o)
		!! Return a real matrix value from a config
		class(config_t),intent(in)::self
			!! Config to search in
		character(*),intent(in)::key
			!! Key to search for
		real(wp),dimension(:,:),allocatable::o
			!! Real matrix value for key

		integer::idx
		
		idx = findKey(self,key)
		if(idx<1 .or. idx>size(self%pairs)) call doError('Invalid index for key: '//key)
		if(self%pairs(idx)%pType/=PT_MATRIX) call doError('Data not MATRIX type: '//key)
		
		allocate(o(size(self%pairs(idx)%m,1),size(self%pairs(idx)%m,2)))
		o = self%pairs(idx)%m
	end function getMatrix
	
	function getString(self,key) result(o)
		!! Return a string value from a config
		class(config_t),intent(in)::self
			!! Config to search in
		character(*),intent(in)::key
			!! Key to search for
		character(:),allocatable::o
			!! String value for key
		
		integer::idx
		
		idx = findKey(self,key)
		if(idx<1 .or. idx>size(self%pairs)) call doError('Invalid index for key: '//key)
		if(self%pairs(idx)%pType/=PT_STRING) call doError('Data not STRING type: '//key)
		
		o = self%pairs(idx)%s
	end function getString

	subroutine writeContents(self,iou)
		!! Write the contents of a config to an I/O unit
		class(config_t),intent(inout)::self
			!! Config to write
		integer,intent(in)::iou
			!! I/O unit to write to
		
		integer::k
		
		do k=1,size(self%pairs)
			write(iou,'(1A)',advance='no') trim(self%pairs(k)%key)//' = '
			select case(self%pairs(k)%ptype)
			case(PT_LOGICAL)
				if(self%pairs(k)%l) then
					write(iou,*) 'TRUE'
				else
					write(iou,*) 'FALSE'
				end if
			case(PT_INTEGER)
				write(iou,*) self%pairs(k)%i
			case(PT_REAL)
				write(iou,*) self%pairs(k)%r
			case(PT_COMPLEX)
				write(iou,*) self%pairs(k)%c
			case(PT_VECTOR)
				write(iou,*) '[',self%pairs(k)%v,']'
			case(PT_MATRIX)
				write(iou,*) 'Matrix: (',shape(self%pairs(k)%m),')'
			case(PT_STRING)
				write(iou,*) ''''//trim(self%pairs(k)%s)//''''
			case default
				write(iou,*) 'Error'
			end select
		end do
	end subroutine writeContents

	!====================!
	!= Utility Routines =!
	!====================!

	subroutine sortKeys(self)
		!! Sort the keys in a config_t object
		!!
		!! Sorting is done in-place (bang-type)
		class(config_t),intent(inout)::self
			!! Config to sort
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

	function findKey(self,key) result(idx)
		!! Find the index of a key in a config
		!!
		!! Will print an error and stop execution if
		!! the key is not found.
		class(config_t),intent(in)::self
			!! Config to search in
		character(*),intent(in)::key
			!! Key to find
		integer::idx
			!! Index of key
		
		integer,dimension(2)::R
		integer::N
		
		N = size(self%pairs)
		
		if(N<7) then
			R = [1,N]
		else
			R = narrowSearch()
		end if
		
		idx = directSearch( R(1) , R(2) )
		if(idx<1 .or. idx>N) call doError('Key not found: '//key)
		
	contains
	
		function narrowSearch() result(o)
			!! Use a quick search to narrow the search range
			integer,dimension(2)::o
			
			integer::l,m,h
			
			l = 1
			h = N
			m = (l+h-1)/2+1
			
			do while(h-l>5)
				if( self%pairs(m)%key == key) then
					h = m
					l = m
				else if( self%pairs(m)%key < key ) then
					l = m
					m = (l+h-1)/2+1
				else if( self%pairs(m)%key > key ) then
					h = m
					m = (l+h-1)/2+1
				end if
			end do
			
			o = [l,h]
		end function narrowSearch
	
		function directSearch(l,h) result(o)
			!! Finish the search with a brute-force approach
			integer,intent(in)::l,h
			integer::o
			
			integer::k
			
			do k=l,h
				if( self%pairs(k)%key/=key ) cycle
				o = k
				exit
			end do
			
			if(k>h) o = -1
		end function directSearch
	
	end function findKey

	subroutine doError(msg)
		!! Print a message and stop execution
		character(*),intent(in)::msg
			!! Message to print
		
		write(*,*) msg
		stop 'Error in config_mod'
	end subroutine doError

end module config_mod
