module quaternion_mod
	!! Module for working with quaternions
	use kinds_mod
	use tensor_mod
	implicit none
	private
	
	type::quat_t
		real(wp)::r
		real(wp),dimension(3)::v
	end type
	
	interface norm2
		module procedure norm2_q
	end interface
	
	interface conjg
		module procedure conjg_q
	end interface
	
	interface exp
		module procedure exp_q
	end interface
	
	interface log
		module procedure log_q
	end interface
	
	interface log10
		module procedure log10_q
	end interface
	
	interface operator(+)
		module procedure add_rq
		module procedure add_qr
		module procedure add_vq
		module procedure add_qv
		module procedure add_qq
	end interface
	
	interface operator(-)
		module procedure sub_rq
		module procedure sub_qr
		module procedure sub_vq
		module procedure sub_qv
		module procedure sub_qq
	end interface
	
	interface operator(*)
		module procedure mul_rq
		module procedure mul_qr
		module procedure mul_vq
		module procedure mul_qv
		module procedure mul_qq
	end interface
	
	interface operator(/)
		module procedure div_qr
	end interface
	
	public::quat_t
	public::norm2
	public::conjg
	public::inv
	
	public::exp
	public::log
	public::log10
	
	public::operator(+)
	public::operator(-)
	public::operator(*)
	
contains

	!==================!
	!= Basic Routines =!
	!==================!

	function norm2_q(q) result(o)
		type(quat_t),intent(in)::q
		real(wp)::o
		
		o = sqrt(q%r**2+sum(q%v**2))
	end function norm2_q

	function conjg_q(q) result(o)
		type(quat_t),intent(in)::q
		type(quat_t)::o
		
		o%r =  q%r
		o%v = -q%v
	end function conjg_q

	function inv(q) result(o)
		type(quat_t),intent(in)::q
		type(quat_t)::o
		
		o = conjg(q)/norm2(q)**2
	end function inv

	!==============================!
	!= Standard Function Routines =!
	!==============================!

	function exp_q(q) result(o)
		type(quat_t),intent(in)::q
		type(quat_t)::o
		
		real(wp)::vm
		
		vm = norm2(q%v)
		
		o%r = exp(q%r)*( cos(vm) )
		o%v = exp(q%r)*( q%v/vm*sin(vm) )
	end function exp_q

	function log_q(q) result(o)
		type(quat_t),intent(in)::q
		type(quat_t)::o
		
		real(wp)::vm,qm
		
		vm = norm2(q%v)
		qm = norm2(q)
		
		o%r = log(qm)
		o%v = q%v/vm*acos(q%r/qm)
	end function log_q

	function log10_q(q) result(o)
		type(quat_t),intent(in)::q
		type(quat_t)::o
		
		o = log(q)/log(10.0_wp)
	end function log10_q

	!================!
	!= Add Routines =!
	!================!

	function add_rq(r,q) result(o)
		real(wp),intent(in)::r
		type(quat_t),intent(in)::q
		type(quat_t)::o
		
		o%r = r+q%r
		o%v = q%v
	end function add_rq

	function add_qr(q,r) result(o)
		type(quat_t),intent(in)::q
		real(wp),intent(in)::r
		type(quat_t)::o
		
		o%r = q%r+r
		o%v = q%v
	end function add_qr

	function add_vq(v,q) result(o)
		real(wp),dimension(3),intent(in)::v
		type(quat_t),intent(in)::q
		type(quat_t)::o
		
		o%r = q%r
		o%v = v+q%v
	end function add_vq

	function add_qv(q,v) result(o)
		type(quat_t),intent(in)::q
		real(wp),dimension(3),intent(in)::v
		type(quat_t)::o
		
		o%r = q%r
		o%v = q%v+v
	end function add_qv

	function add_qq(u,v) result(o)
		type(quat_t),intent(in)::u
		type(quat_t),intent(in)::v
		type(quat_t)::o
		
		o%r = u%r+v%r
		o%v = u%v+v%v
	end function add_qq

	!=====================!
	!= Subtract Routines =!
	!=====================!

	function sub_rq(r,q) result(o)
		real(wp),intent(in)::r
		type(quat_t),intent(in)::q
		type(quat_t)::o
		
		o%r = r-q%r
		o%v = -q%v
	end function sub_rq

	function sub_qr(q,r) result(o)
		type(quat_t),intent(in)::q
		real(wp),intent(in)::r
		type(quat_t)::o
		
		o%r = q%r-r
		o%v = q%v
	end function sub_qr

	function sub_vq(v,q) result(o)
		real(wp),dimension(3),intent(in)::v
		type(quat_t),intent(in)::q
		type(quat_t)::o
		
		o%r = -q%r
		o%v = v-q%v
	end function sub_vq

	function sub_qv(q,v) result(o)
		type(quat_t),intent(in)::q
		real(wp),dimension(3),intent(in)::v
		type(quat_t)::o
		
		o%r = q%r
		o%v = q%v-v
	end function sub_qv

	function sub_qq(u,v) result(o)
		type(quat_t),intent(in)::u
		type(quat_t),intent(in)::v
		type(quat_t)::o
		
		o%r = u%r-v%r
		o%v = u%v-v%v
	end function sub_qq

	!===========================!
	!= Multiplication Routines =!
	!===========================!

	function mul_rq(u,v) result(o)
		real(wp),intent(in)::u
		type(quat_t),intent(in)::v
		type(quat_t)::o
		
		o%r = u*v%r
		o%v = u*v%v
	end function mul_rq

	function mul_qr(u,v) result(o)
		type(quat_t),intent(in)::u
		real(wp),intent(in)::v
		type(quat_t)::o
		
		o%r = u%r*v
		o%v = u%v*v
	end function mul_qr

	function mul_vq(u,v) result(o)
		real(wp),dimension(3),intent(in)::u
		type(quat_t),intent(in)::v
		type(quat_t)::o
		
		o%r = -u.o.v%v
		o%v = u*v%r+u.x.v%v
	end function mul_vq

	function mul_qv(u,v) result(o)
		type(quat_t),intent(in)::u
		real(wp),dimension(3),intent(in)::v
		type(quat_t)::o
		
		o%r = -u%v.o.v
		o%v = u%r*v+u%v.x.v
	end function mul_qv

	function mul_qq(u,v) result(o)
		type(quat_t),intent(in)::u
		type(quat_t),intent(in)::v
		type(quat_t)::o
		
		o%r = u%r*v%r-u%v.o.v%v
		o%v = u%r*v%v+u%v*v%r+u%v.x.v%v
	end function mul_qq

	!=====================!
	!= Division Routines =!
	!=====================!

	function div_qr(q,r) result(o)
		type(quat_t),intent(in)::q
		real(wp),intent(in)::r
		type(quat_t)::o
		
		o%r = q%r/r
		o%v = q%v/r
	end function div_qr

end module quaternion_mod