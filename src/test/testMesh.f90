module plotMesh_mod
	use mesh_mod
	use plplotlib_mod
	use array_mod
	
contains

	recursive subroutine fillTriangle(e,xy,f,fR,vxi,depth)
		type(element_t),intent(in)::e
		real(wp),dimension(:,:),intent(in)::xy
		real(wp),dimension(:),intent(in)::f
		real(wp),dimension(2),intent(in)::fR
		real(wp),dimension(2,3),intent(in),optional::vxi
		integer,intent(in),optional::depth
		
		real(wp),dimension(:),allocatable::N
		real(wp),dimension(2,3)::lxi
		real(wp),dimension(3)::lx,ly,lf
		integer::ldepth
		integer::k
		
		if(present(vxi)) then
			lxi = vxi
		else
			lxi = transpose(reshape([0.0_wp,1.0_wp,0.0_wp,0.0_wp,0.0_wp,1.0_wp],[3,2]))
		end if
		
		if(present(depth)) then
			ldepth = depth
		else
			ldepth = 1
		end if
		
		do k=1,3
			N = e%N(lxi(:,k))
			lx(k) = dot_product(N,xy(1,:))
			ly(k) = dot_product(N,xy(2,:))
			lf(k) = dot_product(N,f)
		end do
		
		if(span(lf)<span(fR)/50.0_wp .or. ldepth>=5) then
			call fill(lx,ly,sum(lf)/real(size(lf),wp),fR)
		else
			call doTesselate()
		end if
	
	contains
	
		recursive subroutine doTesselate()
			real(wp),dimension(2,3)::mxi,txi
			
			mxi(:,1) = (lxi(:,1)+lxi(:,2))/2.0_wp
			mxi(:,2) = (lxi(:,2)+lxi(:,3))/2.0_wp
			mxi(:,3) = (lxi(:,3)+lxi(:,1))/2.0_wp
			
			txi = reshape([lxi(:,1),mxi(:,1),mxi(:,3)],[2,3])
			call fillTriangle(e,xy,f,fR,txi,ldepth+1)
			
			txi = reshape([mxi(:,1),lxi(:,2),mxi(:,2)],[2,3])
			call fillTriangle(e,xy,f,fR,txi,ldepth+1)
			
			txi = reshape([mxi(:,3),mxi(:,2),lxi(:,3)],[2,3])
			call fillTriangle(e,xy,f,fR,txi,ldepth+1)
			
			txi = reshape([mxi(:,1),mxi(:,2),mxi(:,3)],[2,3])
			call fillTriangle(e,xy,f,fR,txi,ldepth+1)
		end subroutine doTesselate
	
	end subroutine fillTriangle

end module plotMesh_mod

program testMesh_prg
	!! Test program for mesh_mod
	use mesh_mod
	use plplotlib_mod
	use array_mod
	use plotMesh_mod
	implicit none
	
	type(mesh_t)::m
	real(wp),dimension(:),allocatable::s
	real(wp),dimension(:,:),allocatable::v
	
	call setup(fileName='testsMesh-%n.svg',figSize=[400,300])
	call testReadGmsh
	call testWriteVTK
	call testShapeFunctions
	call show()
	
contains

	subroutine testReadGmsh
		!! Verify operation of readGmsh
		
		call execute_command_line('gmsh -2 ./input/square.geo -o square.msh -format msh2')
		call m%readGmsh('square.msh')
	end subroutine testReadGmsh

	subroutine testWriteVTK
		!! Verify operation of writeVTK
		integer::N,k
		
		N = size(m%nodes)
		allocate( s(N) , v(N,3) )
		v(:,3) = 0.0_wp
		do k=1,N
			s(k) = norm2( m%nodes(k)%x )
			v(k,1:2) = m%nodes(k)%x
		end do
		
		call m%writeVTK('square.vtk')
		call m%appendScalarVTK('square.vtk',s,'s')
		call m%appendVectorVTK('square.vtk',v,'v')
	end subroutine testWriteVTK

	subroutine testShapeFunctions
		!! Verify operation of element shape functions
		
		real(wp),dimension(:),allocatable::x,y
		real(wp),dimension(:),allocatable::ex,ey
		real(wp),dimension(:),allocatable::lx,ly
		real(wp),dimension(:,:),allocatable::lxy
		type(element_t)::e
		integer::k
		
		x = m%nodes(:)%x(1)
		y = m%nodes(:)%x(2)
		
		call figure()
		call subplot(1,1,1,aspect=span(y)/span(x))
		call xylim(mixval(x),mixval(y)+0.05_wp*[-1.0_wp,1.0_wp*span(y)])
		
		do k=1,size(m%elements)
			e = m%elements(k)
			if(.not.any(e%etype==[ET_TRIANGLE_1,ET_TRIANGLE_2])) cycle
			ex = m%nodes(e%nodes)%x(1)
			ey = m%nodes(e%nodes)%x(2)
			lxy = transpose(reshape([ex,ey],[size(ex),2]))
			call fillTriangle(e,lxy,exampleFunction(ex,ey),[-1.0_wp,1.0_wp])
		end do
		
		do k=1,size(m%elements)
			e = m%elements(k)
			if(.not.any(e%etype==[ET_TRIANGLE_1,ET_TRIANGLE_2])) cycle
			ex = m%nodes(e%nodes)%x(1)
			ey = m%nodes(e%nodes)%x(2)
			
			lx = ex([1,2,3,1])
			ly = ey([1,2,3,1])
			call plot(lx,ly,lineStyle='-',lineColor='k',lineWidth=2.0_wp)
		end do
		call plot(x,y,lineStyle='',markStyle='s',markColor='C1',markSize=4.0_wp)
		
		call ticks()
		call labels('x','y','')
		call colorbar(reshape([-1.0_wp,1.0_wp],[2,1]),10,'','')
	end subroutine testShapeFunctions

	elemental function exampleFunction(x,y) result(o)
		real(wp),intent(in)::x,y
		real(wp)::o
		
		o = x*y
	end function exampleFunction

end program testMesh_prg
