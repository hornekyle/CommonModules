module quaternion_mod
	!! Module for working with quaternions
	!! @todo
	!! Add documentation
	use kinds_mod
	use tensor_mod
	implicit none
	private
	
	!=========!
	!= Types =!
	!=========!
	
	type::quat_t
		!! Hamilton's quaternion extension to complex numbers
		real(wp)::s = 0.0_wp
			!! Scalar part
		real(wp),dimension(3)::v = 0.0_wp
			!! Vector part
	contains
		procedure::getRotationMatrix
	end type
	
	!==============!
	!= Interfaces =!
	!==============!
	
	interface quat_t
		module procedure newQuat
	end interface
	
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
	
	!===========!
	!= Exports =!
	!===========!
	
	public::quat_t
	
	public::scaler
	public::vector
	
	public::norm2
	public::conjg
	public::inv
	
	public::exp
	public::log
	public::log10
	
	public::operator(+)
	public::operator(-)
	public::operator(*)
	public::operator(/)
	
contains

	!==================!
	!= Basic Routines =!
	!==================!

	function newQuat(r,v) result(self)
		real(wp),intent(in)::r
		real(wp),dimension(3),intent(in)::v
		type(quat_t)::self
		
		self%s = r
		self%v = v
	end function newQuat

	elemental function scaler(q) result(o)
		!! Return the scalar part of the quaternion
		type(quat_t),intent(in)::q
			!! The quaternion of interest
		real(wp)::o
		
		o = q%s
	end function scaler

	function vector(q) result(o)
		!! Return the vector part of the quaternion
		type(quat_t),intent(in)::q
			!! The quaternion of interest
		real(wp),dimension(3)::o
		
		o = q%v
	end function vector

	elemental function norm2_q(q) result(o)
		!! Return the magnitude of the quaternion
		type(quat_t),intent(in)::q
			!! The quaternion of interest
		real(wp)::o
		
		o = sqrt(q%s**2+sum(q%v**2))
	end function norm2_q

	elemental function conjg_q(q) result(o)
		!! Return the conjugate of the quaternion
		type(quat_t),intent(in)::q
			!! The quaternion of interest
		type(quat_t)::o
		
		o%s =  q%s
		o%v = -q%v
	end function conjg_q

	elemental function inv(q) result(o)
		!! Return the inverse of the quaternion
		type(quat_t),intent(in)::q
			!! The quaternion of interest
		type(quat_t)::o
		
		o = conjg(q)/norm2(q)**2
	end function inv

	!==============================!
	!= Standard Function Routines =!
	!==============================!

	elemental function exp_q(q) result(o)
		!! Take the exponent of a quaternion
		type(quat_t),intent(in)::q
			!! The quaternion of interest
		type(quat_t)::o
		
		real(wp)::vm
		
		vm = norm2(q%v)
		
		o%s = exp(q%s)*( cos(vm) )
		o%v = exp(q%s)*( q%v/vm*sin(vm) )
	end function exp_q

	elemental function log_q(q) result(o)
		!! Take the natural log of a quaternion
		type(quat_t),intent(in)::q
			!! The quaternion of interest
		type(quat_t)::o
		
		real(wp)::vm,qm
		
		vm = norm2(q%v)
		qm = norm2(q)
		
		o%s = log(qm)
		o%v = q%v/vm*acos(q%s/qm)
	end function log_q

	elemental function log10_q(q) result(o)
		!! Take the log10 of a quaternion
		type(quat_t),intent(in)::q
			!! The quaternion of interest
		type(quat_t)::o
		
		o = log(q)/log(10.0_wp)
	end function log10_q

	!================!
	!= Add Routines =!
	!================!

	elemental function add_rq(r,q) result(o)
		real(wp),intent(in)::r
		type(quat_t),intent(in)::q
		type(quat_t)::o
		
		o%s = r+q%s
		o%v = q%v
	end function add_rq

	elemental function add_qr(q,r) result(o)
		type(quat_t),intent(in)::q
		real(wp),intent(in)::r
		type(quat_t)::o
		
		o%s = q%s+r
		o%v = q%v
	end function add_qr

	pure function add_vq(v,q) result(o)
		real(wp),dimension(3),intent(in)::v
		type(quat_t),intent(in)::q
		type(quat_t)::o
		
		o%s = q%s
		o%v = v+q%v
	end function add_vq

	pure function add_qv(q,v) result(o)
		type(quat_t),intent(in)::q
		real(wp),dimension(3),intent(in)::v
		type(quat_t)::o
		
		o%s = q%s
		o%v = q%v+v
	end function add_qv

	elemental function add_qq(u,v) result(o)
		type(quat_t),intent(in)::u
		type(quat_t),intent(in)::v
		type(quat_t)::o
		
		o%s = u%s+v%s
		o%v = u%v+v%v
	end function add_qq

	!=====================!
	!= Subtract Routines =!
	!=====================!

	elemental function sub_rq(r,q) result(o)
		real(wp),intent(in)::r
		type(quat_t),intent(in)::q
		type(quat_t)::o
		
		o%s = r-q%s
		o%v = -q%v
	end function sub_rq

	elemental function sub_qr(q,r) result(o)
		type(quat_t),intent(in)::q
		real(wp),intent(in)::r
		type(quat_t)::o
		
		o%s = q%s-r
		o%v = q%v
	end function sub_qr

	pure function sub_vq(v,q) result(o)
		real(wp),dimension(3),intent(in)::v
		type(quat_t),intent(in)::q
		type(quat_t)::o
		
		o%s = -q%s
		o%v = v-q%v
	end function sub_vq

	pure function sub_qv(q,v) result(o)
		type(quat_t),intent(in)::q
		real(wp),dimension(3),intent(in)::v
		type(quat_t)::o
		
		o%s = q%s
		o%v = q%v-v
	end function sub_qv

	elemental function sub_qq(u,v) result(o)
		type(quat_t),intent(in)::u
		type(quat_t),intent(in)::v
		type(quat_t)::o
		
		o%s = u%s-v%s
		o%v = u%v-v%v
	end function sub_qq

	!===========================!
	!= Multiplication Routines =!
	!===========================!

	elemental function mul_rq(u,v) result(o)
		real(wp),intent(in)::u
		type(quat_t),intent(in)::v
		type(quat_t)::o
		
		o%s = u*v%s
		o%v = u*v%v
	end function mul_rq

	elemental function mul_qr(u,v) result(o)
		type(quat_t),intent(in)::u
		real(wp),intent(in)::v
		type(quat_t)::o
		
		o%s = u%s*v
		o%v = u%v*v
	end function mul_qr

	pure function mul_vq(u,v) result(o)
		real(wp),dimension(3),intent(in)::u
		type(quat_t),intent(in)::v
		type(quat_t)::o
		
		o%s = -(u.o.v%v)
		o%v = u*v%s+(u.x.v%v)
	end function mul_vq

	pure function mul_qv(u,v) result(o)
		type(quat_t),intent(in)::u
		real(wp),dimension(3),intent(in)::v
		type(quat_t)::o
		
		o%s = -(u%v.o.v)
		o%v = u%s*v+(u%v.x.v)
	end function mul_qv

	elemental function mul_qq(u,v) result(o)
		type(quat_t),intent(in)::u
		type(quat_t),intent(in)::v
		type(quat_t)::o
		
		o%s = u%s*v%s-(u%v.o.v%v)
		o%v = u%s*v%v+u%v*v%s+(u%v.x.v%v)
	end function mul_qq

	!=====================!
	!= Division Routines =!
	!=====================!

	elemental function div_qr(q,r) result(o)
		type(quat_t),intent(in)::q
		real(wp),intent(in)::r
		type(quat_t)::o
		
		o%s = q%s/r
		o%v = q%v/r
	end function div_qr

	!=======================!
	!= Quaternion Routines =!
	!=======================!

	function getRotationMatrix(self) result(o)
		class(quat_t),intent(in)::self
		real(wp),dimension(3,3)::o
		
		type(quat_t)::q
		real(wp)::a,b,c,d
		
		q = self/norm2(self)
		
		a = q%s
		b = q%v(1)
		c = q%v(2)
		d = q%v(3)
		
		o(1,1) = a**2 + b**2 - c**2 - d**2
		o(2,2) = a**2 - b**2 + c**2 - d**2
		o(3,3) = a**2 - b**2 - c**2 + d**2
		
		o(2,1) = 2.0_wp*b*c + 2.0_wp*a*d
		o(1,2) = 2.0_wp*b*c - 2.0_wp*a*d
		
		o(3,1) = 2.0_wp*b*d - 2.0_wp*a*c
		o(1,3) = 2.0_wp*b*d + 2.0_wp*a*c
		
		o(3,2) = 2.0_wp*c*d + 2.0_wp*a*b
		o(2,3) = 2.0_wp*c*d - 2.0_wp*a*b
	end function getRotationMatrix

end module quaternion_mod
