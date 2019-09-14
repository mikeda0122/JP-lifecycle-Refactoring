program vector
  use fgsl
  implicit none
  integer(fgsl_int), parameter :: itmax = 30
  real(fgsl_double), parameter :: eps=1.0E-6_fgsl_double
  integer(fgsl_int) :: status, vstatus, i
  real(fgsl_double) :: xmin, size
  character(kind=fgsl_char,len=fgsl_strmax) :: name
  type(fgsl_multimin_fminimizer) :: min_fslv
  type(fgsl_multimin_function) :: func

  integer(fgsl_size_t), parameter :: pdim = 7

  real(fgsl_double) :: intval(pdim), step(pdim), location(pdim)
  real(fgsl_double), pointer :: intptr(:), stptr(:), locptr(:)

  type(fgsl_vector) :: intvec, stepvec, locvec

  intvec = fgsl_vector_init(1.0_fgsl_double)
  stepvec = fgsl_vector_init(1.0_fgsl_double)
  locvec = fgsl_vector_init(1.0_fgsl_double)

  intval(1:pdim) = (/0.615_fgsl_double, 7.96_fgsl_double, 3399.0_fgsl_double, &
       &202.0_fgsl_double, 240.0_fgsl_double, 1.04_fgsl_double, 0.031734_fgsl_double/) !para(4) scaled
  step(1:pdim) = (/0.01_fgsl_double, 0.05_fgsl_double, 30.0_fgsl_double, &
       &3.0_fgsl_double, 10.0_fgsl_double, 0.01_fgsl_double, 0.005_fgsl_double/)

  !!Define vectors here. fgsl_vector_align(the actual fortran array, dimension of the array, fgsl_vector type vector, the number of elements put into "vector" from "array"(vector size), k, s: starting from the "k-1"th element, next "s" elements of "array" are put into "vector")
  !!Extra attention to "k-1"th.
  vstatus = fgsl_vector_align(intval, pdim, intvec, pdim, 0_fgsl_size_t, 1_fgsl_size_t)
  if (vstatus == fgsl_success) then
     vstatus = fgsl_vector_align(intptr, intvec)
     write(6, '(''Size of pointer is: '',i3)') size(intptr)
     write(6, '(''Components: '',3F12.5)') intptr(1:size(intptr))
     write(6, '(''Should be : '',3F12.5)') intval(1:pdim)
  else
     write(6, *) 'Failed to properly initialize vector object.'
  end if

end program vector
