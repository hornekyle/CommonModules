module mesh_mod
	!! Module for handeling 2D meshes
	!! @todo
	!! Figure out why there are two connect routines
	use kinds_mod
	implicit none
	private
	
	!==============!
	!= Parameters =!
	!==============!
	
	integer,parameter::ET_POINT_1 = 11
	
	integer,parameter::ET_EDGE_1  = 21
	integer,parameter::ET_EDGE_2  = 22
	integer,parameter::ET_EDGE_3  = 23
	
	integer,parameter::ET_TRIANGLE_1 = 31
	integer,parameter::ET_TRIANGLE_2 = 32
	integer,parameter::ET_TRIANGLE_3 = 33
	
	!=========!
	!= Types =!
	!=========!
	
	type::node_t
		integer::nidx
		real(wp),dimension(2)::x
		integer::ngroup
		integer,dimension(:),allocatable::nodes
		integer,dimension(:),allocatable::elements
	end type
	
	type::element_t
		integer::eidx
		integer::etype
		integer::egroup
		integer::ecolor
		integer,dimension(:),allocatable::nodes
		integer,dimension(:),allocatable::elements
	end type
	
	type::group_t
		integer::gidx
		integer::gdim
		character(128)::gname
		real(wp),dimension(:),allocatable::gdata
	end type
	
	type::mesh_t
		type(node_t),dimension(:),allocatable::nodes
		type(element_t),dimension(:),allocatable::elements
		type(group_t),dimension(:),allocatable::groups
	contains
		procedure::readGmsh
		procedure::writeVTK
		procedure::connect
	end type
	
	!===========!
	!= Exports =!
	!===========!
	
	public::ET_POINT_1
	public::ET_EDGE_1
	public::ET_EDGE_2
	public::ET_EDGE_3
	public::ET_TRIANGLE_1
	public::ET_TRIANGLE_2
	public::ET_TRIANGLE_3
	
	public::node_t
	public::element_t
	public::group_t
	public::mesh_t

contains

	subroutine readGmsh(self,fn)
		!! Read a gmsh .msh file into memory
		class(mesh_t),intent(inout)::self
			!! Self mesh_t object
		character(*),intent(in)::fn
			!! Filename
		
		character(128)::buf
		integer::iou,ier
		
		open(file=fn,newunit=iou)
		read(iou,'(1A)',iostat=ier) buf
		do while(ier==0)
			select case(buf)
			case('$MeshFormat')
			case('$PhysicalNames')
				call readPhysical
			case('$Nodes')
				call readNodes
			case('$Elements')
				call readElements
			end select
			call complete(trim(adjustl(buf)))
			read(iou,*,iostat=ier) buf
		end do
		close(iou)
		
		call connect(self)
		call self%connect()
		
	contains
		
		subroutine complete(tag)
			!! Read a file until the end for tag is found
			character(*),intent(in)::tag
			character(128)::buf
			integer::ier
			
			read(iou,'(1A)',iostat=ier) buf
			do while(buf/='$End'//tag(2:) .and. ier==0)
				read(iou,'(1A)',iostat=ier) buf
			end do
		end subroutine complete
		
		subroutine readNodes
			!! Read the nodes from a msh file
			integer::N,i,k
			real(wp)::x,y,z
			
			read(iou,*) N
			if(allocated(self%nodes)) deallocate(self%nodes)
			allocate(self%nodes(N))
			do k=1,N
				read(iou,*) i,x,y,z
				self%nodes(i)%nidx = i
				self%nodes(i)%x = [x,y]
			end do
		end subroutine readNodes
		
		subroutine readElements
			!! Read the elements from a msh file
			integer::N,k,Nt,i,t,p
			integer,dimension(10)::nodes
			integer,dimension(10)::tags
			
			read(iou,*) N
			if(allocated(self%elements)) deallocate(self%elements)
			allocate(self%elements(N))
			do k=1,N
				read(iou,*) i,t,Nt,p,tags(1:Nt-1),nodes(1:elementNodeCount(t))
				self%elements(i)%eidx = i
				self%elements(i)%etype = elementGmshToInt(t)
				self%elements(i)%egroup = p
				self%elements(i)%nodes = nodes(1:elementNodeCount(t))
			end do
		end subroutine readElements
		
		function elementGmshToInt(gmsh_type) result(internalType)
			!! Convert gmsh element types to internal types
			integer,intent(in)::gmsh_type
			integer::internalType
			
			select case(gmsh_type)
			case(1)
				internalType = ET_EDGE_1
			case(2)
				internalType = ET_TRIANGLE_1
			case(8)
				internalType = ET_EDGE_2
			case(9)
				internalType = ET_TRIANGLE_2
			case(15)
				internalType = ET_POINT_1
			case(21)
				internalType = ET_TRIANGLE_3
			case(26)
				internalType = ET_EDGE_3
			case default
				internalType = -1
			end select
		end function elementGmshToInt
		
		function elementNodeCount(gmshType) result(nodeCount)
			!! Output the number of nodes for each gmsh element type
			integer,intent(in)::gmshType
			integer::nodeCount
			
			select case(gmshType)
			case(1)
				nodeCount = 2
			case(2)
				nodeCount = 3
			case(8)
				nodeCount = 3
			case(9)
				nodeCount = 6
			case(15)
				nodeCount = 1
			case(21)
				nodeCount = 10
			case(26)
				nodeCount = 4
			case default
				nodeCount = -1
			end select
		end function elementNodeCount
		
		subroutine readPhysical
			!! Read physical names from msh file
			integer::N,k,i,d
			
			read(iou,*) N
			if(allocated(self%groups)) deallocate(self%groups)
			allocate(self%groups(N))
			do k=1,N
				read(iou,*) d,i,self%groups(i)%gname
				self%groups(i)%gdim = d
			end do
		end subroutine readPhysical
		
		subroutine connect(self)
			!! Build connectivity in the mesh
			class(mesh_t),intent(inout)::self
				!! Self mesh_t object
			
			integer::i,k,l,g
			
			self%nodes%ngroup = -1
			
			do k=1,size(self%elements)
				g = self%elements(k)%egroup
				do l=1,size(self%elements(k)%nodes)
					i = self%elements(k)%nodes(l)
					
					if(self%nodes(i)%ngroup == -1) self%nodes(i)%ngroup = g
					if(self%groups(g)%gdim>self%groups(self%nodes(i)%ngroup)%gdim) cycle
					self%nodes(i)%ngroup = g
				end do
			end do
		end subroutine connect
		
	end subroutine readGmsh

	subroutine writeVTK(self,fn)
		!! Write the mesh as a VTK file
		class(mesh_t),intent(in)::self
			!! Self mesh_t object
		character(*),intent(in)::fn
			!! Filename
		
		integer::iou
		integer::k,l
		
		open(file=fn,newunit=iou)
		write(iou,'(1A)') '# vtk DataFile Version 2.0'
		write(iou,'(1A256)') '[Arbitrary data description]'
		write(iou,'(1A)') 'ASCII'
		write(iou,'(1A)') 'DATASET UNSTRUCTURED_GRID'
		write(iou,'(1A,1X,I10,1X,1A)') 'POINTS',size(self%nodes),'double'
		do k=1,size(self%nodes)
			write(iou,'(3E25.15)') real(self%nodes(k)%x,dp),0.0_dp
		end do
		write(iou,'(1A,2I10)') 'CELLS',size(self%elements), &
		& sum([(1+size(self%elements(l)%nodes),l=1,size(self%elements))])
		do k=1,size(self%elements)
			write(iou,'(15I10)') size(self%elements(k)%nodes),self%elements(k)%nodes-1
		end do
		write(iou,'(1A,1I10)') 'CELL_TYPES',size(self%elements)
		do k=1,size(self%elements)
			write(iou,'(1I10)') elementIntToVTK(self%elements(k)%etype)
		end do
		write(iou,'(1A,1I10)') 'CELL_DATA',size(self%elements)
		write(iou,'(1A,1X,1A,1X,1A,1I10)') 'SCALARS','element_idx','int',1
		write(iou,'(1A,1X,1A)') 'LOOKUP_TABLE','default'
		do k=1,size(self%elements)
			write(iou,'(1I10)') self%elements(k)%eidx
		end do
		write(iou,'(1A,1X,1A,1X,1A,1I10)') 'SCALARS','element_group','int',1
		write(iou,'(1A,1X,1A)') 'LOOKUP_TABLE','default'
		do k=1,size(self%elements)
			write(iou,'(1I10)') self%elements(k)%egroup
		end do
		write(iou,'(1A,1X,1A,1X,1A,1I10)') 'SCALARS','element_color','int',1
		write(iou,'(1A,1X,1A)') 'LOOKUP_TABLE','default'
		do k=1,size(self%elements)
			write(iou,'(1I10)') self%elements(k)%ecolor
		end do
		write(iou,'(1A,1I10)') 'POINT_DATA',size(self%nodes)
		write(iou,'(1A,1X,1A,1X,1A,1I10)') 'SCALARS','node_idx','int',1
		write(iou,'(1A,1X,1A)') 'LOOKUP_TABLE','default'
		do k=1,size(self%nodes)
			write(iou,'(1I10)') self%nodes(k)%nidx
		end do
		write(iou,'(1A,1X,1A,1X,1A,1I10)') 'SCALARS','node_group','int',1
		write(iou,'(1A,1X,1A)') 'LOOKUP_TABLE','default'
		do k=1,size(self%nodes)
			write(iou,'(1I10)') self%nodes(k)%ngroup
		end do
		close(iou)
	
	contains
	
		function elementIntToVTK(internal_type) result(vtk_type)
			!! Convert internal element types to VTK types
			integer,intent(in)::internal_type
			integer::vtk_type
			
			select case(internal_type)
			case(ET_POINT_1)
				vtk_type = 1
			case(ET_EDGE_1)
				vtk_type = 3
			case(ET_EDGE_2)
				vtk_type = 21
			case(ET_EDGE_3)
				vtk_type = -1
			case(ET_TRIANGLE_1)
				vtk_type = 5
			case(ET_TRIANGLE_2)
				vtk_type = 22
			case(ET_TRIANGLE_3)
				vtk_type = -1
			end select
		end function elementIntToVTK
	
	end subroutine writeVTK

	subroutine connect(self)
		!! Build connectivity in the mesh
		class(mesh_t),intent(inout)::self
		
		integer,dimension(:),allocatable::c,b
		type(element_t)::e
		integer::k,l,m,n
		
		! Create nodal element lists
		c = [(0,k=1,size(self%nodes))]
		do k=1,size(self%elements)
			e = self%elements(k)
			do l=1,size(e%nodes)
				c(e%nodes(l)) = c(e%nodes(l))+1
			end do
		end do
		do k=1,size(c)
			self%nodes(k)%elements = [(0,l=1,c(k))]
		end do
		c = 0
		do k=1,size(self%elements)
			e = self%elements(k)
			do l=1,size(e%nodes)
				n = e%nodes(l)
				c(n) = c(n)+1
				self%nodes(n)%elements(c(n)) = k
			end do
		end do
		
		! Create nodal node lists
		do k=1,size(self%nodes)
			b = self%nodes(k)%elements
			self%nodes(k)%nodes = deDup([(self%elements(b(l))%nodes,l=1,size(b))],k)
		end do
		
		! Create elemental element lists
		do k=1,size(self%elements)
			b = self%elements(k)%nodes
			self%elements(k)%elements = deDup([(self%nodes(b(l))%elements,l=1,size(b))],k)
		end do
		
		! Assign element colors
		c = [(0,k=1,size(self%elements))]
		self%elements%ecolor = 0
		do m=1,size(self%elements)
			n = 0
			where(self%elements%ecolor==-1) self%elements%ecolor = 0
			do k=1,size(self%elements)
				e = self%elements(k)
				if(e%ecolor/=0) cycle
				
				self%elements(k)%ecolor = m
				do l=1,size(e%elements)
					if(self%elements(e%elements(l))%ecolor>0) cycle
					self%elements(e%elements(l))%ecolor = -1
					n = n+1
				end do!! Build connectivity in the mesh
			end do
			if(n==0) exit
		end do
		
	contains
	
		function deDup(l,n) result(o)
			!! Remove duplicates from a list
			integer,dimension(:),intent(in)::l
			integer,intent(in)::n
			integer,dimension(:),allocatable::o
			
			integer::k,b
			
			o = l
			where(o==n) o = -1
			do k=1,size(l)
				if(o(k)==-1) cycle
				b = o(k)
				where(o==b) o = -1
				o(k) = b
			end do
			
			o = pack(o,o>=0)
		end function deDup
	
	end subroutine connect

end module mesh_mod
