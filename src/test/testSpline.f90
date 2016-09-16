program testSpline_prg
	!! Test program for Spline_mod
	use kinds_mod
	use spline_mod
	use plplotlib_mod
	use array_mod
	implicit none
	
	real(wp),dimension(5,2),parameter::x0 = reshape( [ &
		& -2.0_wp,-1.0_wp, 0.0_wp,1.0_wp,2.0_wp , &
		&  3.0_wp, 0.0_wp,-1.0_wp,0.0_wp,3.0_wp & 
		& ],[5,2])
	
	real(wp),dimension(5),parameter::t0 = &
		& [ 0.00_wp,0.25_wp,0.50_wp,0.75_wp,1.00_wp]
	
	call testNewSpline
	call testSplineX
	
contains

	subroutine testNewSpline
		type(cubicSpline_t)::spline
		
		spline = cubicSpline_t(t0,x0)
	end subroutine testNewSpline

	subroutine testSplineX
		type(cubicSpline_t)::sf,sc
		type(linearSpline_t)::sl
		real(wp),dimension(:,:),allocatable::xl,xf,xc,xt
		real(wp)::t
		integer::N,k
		
		N = 100
		
		sl = linearSpline_t(t0,x0)
		sf = cubicSpline_t(t0,x0,'finiteDifference')
		sc = cubicSpline_t(t0,x0,'cardinal',c=0.2_wp)
		
		allocate( xl(N,2) , xf(N,2) , xc(N,2) , xt(N,2) )
		
		do k=1,N
			t = real(k-1,wp)/real(N-1,wp)
			xl(k,1:2) = sl%x(t)
			xf(k,1:2) = sf%x(t)
			xc(k,1:2) = sc%x(t)
			xt(k,1:2) = [4.0_wp*t-2.0_wp,(4.0_wp*t-2.0_wp)**2-1.0_wp]
		end do
		
		call setup(fileName='spline-%n.svg',figSize=[400,300])
		
		call figure()
		call subplot(1,1,1)
		call xylim(mixval(xt(:,1)),mixval(xt(:,2)))
		
		call plot(xl(:,1),xl(:,2),lineStyle=':' ,lineColor='m',lineWidth=2.0_wp)
		call plot(xt(:,1),xt(:,2),lineStyle='-' ,lineColor='r',lineWidth=2.0_wp)
		call plot(xf(:,1),xf(:,2),lineStyle='-' ,lineColor='b',lineWidth=2.0_wp)
		call plot(xc(:,1),xc(:,2),lineStyle='--',lineColor='c',lineWidth=2.0_wp)
		call plot(x0(:,1),x0(:,2),lineStyle=''  ,markStyle='x',markColor='k')
		
		call ticks()
		call labels('x','y','Spline Test')
		
		call figure()
		call subplot(1,1,1)
		call xylim(mixval(xt(:,1)),[-2.0_wp,2.0_wp])
		
		call plot(xl(:,1),xl(:,2)-xt(:,2),lineStyle=':' ,lineColor='m',lineWidth=2.0_wp)
		call plot(xt(:,1),xt(:,2)-xt(:,2),lineStyle='-' ,lineColor='r',lineWidth=2.0_wp)
		call plot(xf(:,1),xf(:,2)-xt(:,2),lineStyle='-' ,lineColor='b',lineWidth=2.0_wp)
		call plot(xc(:,1),xc(:,2)-xt(:,2),lineStyle='--',lineColor='c',lineWidth=2.0_wp)
		call plot(x0(:,1),x0(:,2)-x0(:,2),lineStyle=''  ,markStyle='x',markColor='k')
		
		call ticks()
		call labels('x','y','Spline Test')
		
		call show()
	end subroutine testSplineX

end program testSpline_prg 
