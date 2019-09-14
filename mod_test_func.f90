module mod_min

  use fgsl
  use, intrinsic :: iso_c_binding

contains
  function cosp(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
    real(c_double) :: cosp
    !   
    cosp = cos(x) + 1.0d0
  end function cosp
end module mod_min


module mod_multimin
  use, intrinsic :: iso_c_binding
  use fgsl
  use mod_unit
  implicit none
contains
  function paraboloid_f(v, p) bind(c)
    type(c_ptr), value :: v, p
    real(c_double) :: paraboloid_f
!
    type(fgsl_vector) :: vec
    real(fgsl_double), pointer :: par(:), pvec(:)
    integer(fgsl_int) :: status
    call fgsl_obj_c_ptr(vec, v)
    call c_f_pointer(p, par, (/ 2 /))
    status = fgsl_vector_align(pvec, vec)
    paraboloid_f = 10.0_fgsl_double * (pvec(1)-par(1))**2 + &
         20.0_fgsl_double * (pvec(2)-par(2))**2 + 30.0_fgsl_double
  end function paraboloid_f
  subroutine paraboloid_df(v, p, df) bind(c)
    type(c_ptr), value :: v, p, df
!
    type(fgsl_vector) :: vec, grad
    real(fgsl_double), pointer :: par(:), pvec(:), pdf(:)
    integer(fgsl_int) :: status
    call fgsl_obj_c_ptr(vec, v)
    call fgsl_obj_c_ptr(grad, df)
    call c_f_pointer(p, par, (/ 2 /))
    status = fgsl_vector_align(pvec, vec)
    status = fgsl_vector_align(pdf, grad)
    pdf(1) = 20.0_fgsl_double * (pvec(1)-par(1))
    pdf(2) = 40.0_fgsl_double * (pvec(2)-par(2))
  end subroutine paraboloid_df
  subroutine paraboloid_fdf(v, p, f, df) bind(c)
    type(c_ptr), value :: v, p, df
    real(c_double) :: f
    f = paraboloid_f(v, p)
    call paraboloid_df(v, p, df)
  end subroutine paraboloid_fdf
end module mod_multimin
