module plplotlib_mod
	!! Wrapper module for plplot to give it a more matplotlib like personality
	use kinds_mod, only: wp
	use plplotlibBase_mod
	use plplotlib1D_mod
	use plplotlib2D_mod
	implicit none
	private
	
	public::setup,show
	public::figure
	public::subplot
	public::xylim,xlim,ylim,xyzlim
	public::labels,xlabel,ylabel,title
	public::ticks,xticks,yticks,box
	public::legend
	
	public::binData
	
	public::plot,plot3
	public::scatter,errorbar
	public::contour,contourf
	public::colorbar,colorbar2
	public::bar,barh
	public::hist
	public::fillBetween,fillBetweenx
	public::quiver
	public::surface,wireframe
	
end module plplotlib_mod
