module mod_GNU_min
  
  use mod_gmm

contains
  subroutine GNU_NM_min(intval, step)
    real(fgsl_double), intent(in) target :: intval(:), step(:)
    integer(fgsl_int), parameter :: itmax = 1000
    real(fgsl_double), parameter :: eps=1.0E-6_fgsl_double
    integer(fgsl_int) :: status, vstatus, i
    real(fgsl_double) :: xmin, simplex_size
    character(kind=fgsl_char,len=fgsl_strmax) :: name
    type(fgsl_multimin_fminimizer) :: min_fslv
    type(fgsl_multimin_function) :: func
    type(c_ptr) :: dummy_ptr

    integer(fgsl_size_t), parameter :: pdim = 7

    real(fgsl_double), target :: location(pdim), dummy(1)
    real(fgsl_double), pointer :: intptr(:), stptr(:), locptr(:)

    type(fgsl_vector) :: intvec, stepvec, locvec

    intvec = fgsl_vector_init(1.0_fgsl_double)
    stepvec = fgsl_vector_init(1.0_fgsl_double)
    locvec = fgsl_vector_init(1.0_fgsl_double)

    !intval(1:pdim) = (/0.615_fgsl_double, 7.96_fgsl_double, 3399.0_fgsl_double, &
    !     &202.0_fgsl_double, 240.0_fgsl_double, 1.04_fgsl_double, 0.031734_fgsl_double/) !para(4) scaled
    !step(1:pdim) = (/0.01_fgsl_double, 0.05_fgsl_double, 30.0_fgsl_double, &
    !     &3.0_fgsl_double, 10.0_fgsl_double, 0.01_fgsl_double, 0.005_fgsl_double/)

    !!Define vectors here. fgsl_vector_align(the actual fortran array, dimension of the array, fgsl_vector type vector, the number of elements put into "vector" from "array"(vector size), k, s: starting from the "k-1"th element, next "s" elements of "array" are put into "vector")
    !!Extra attention to "k-1"th.
    vstatus = fgsl_vector_align(intval, pdim, intvec, pdim, 0_fgsl_size_t, 1_fgsl_size_t)

    if (vstatus == fgsl_success) then
       !!To access the components of vector, we need to use a "pointer".
       vstatus = fgsl_vector_align(intptr, intvec)
       !write(6, '(''Size of pointer is: '',i3)') size(intptr)
       !write(6, '(''Components: '',3F12.5)') intptr(1:size(intptr))
       !write(6, '(''Should be : '',3F12.5)') intval(1:pdim)
       write(*,*) 'initial value'
       write(*,*) intptr(1:size(intptr))
    else
       write(6, *) 'Failed to properly initialize vector object.'
    end if

    vstatus = fgsl_vector_align(step, pdim, stepvec, pdim, 0_fgsl_size_t, 1_fgsl_size_t)
    if (vstatus == fgsl_success) then
       !!To access the components of vector, we need to use a "pointer".
       vstatus = fgsl_vector_align(stptr, stepvec)

       !write(6, '(''Size of pointer is: '',i3)') size(stptr)
       !write(6, '(''Components: '',3F12.5)') stptr(1:size(stptr))
       write(*,*) 'step'
       write(6,*) stptr(1:size(stptr))
    else
       write(6, *) 'Failed to properly initialize vector object.'
    end if

    !
    !!Specify which kind of minimizer (minimization method) we are using
    min_fslv = fgsl_multimin_fminimizer_alloc(fgsl_multimin_fminimizer_nmsimplex, pdim) !!possibly we need dimension of parameters, "n"

    !!Define function???? How is this works???
    !!I guess (function with name, dimension of arguments???, arguments as a pointer?)
    dummy(1:1) = (/1.0_fgsl_double/)
    dummy_ptr = c_loc(dummy)
    func = fgsl_multimin_function_init(gmm, pdim, dummy_ptr)

    if (fgsl_well_defined(min_fslv)) then

       !!Specify details of minimization process i.e. (method, function, initial vector, step vector)
       status = fgsl_multimin_fminimizer_set(min_fslv, func, intvec, stepvec)

       !!Find name for the method
       name = fgsl_multimin_fminimizer_name(min_fslv)

       i = 0

       do
          i = i + 1

          !!This is where the minimization routine is actually taken place.
          status = fgsl_multimin_fminimizer_iterate(min_fslv)

          if (status /= FGSL_SUCCESS .or. i > itmax) then
             write(6, *) 'Failed to iterate or converge. Aborting.'
             exit
          end if

          !!The following information is globally presereved every time iteration, min_fslv,  occur.
          !!***The current best estimate of the location of the minimum
          locvec = fgsl_multimin_fminimizer_x(min_fslv)
          !!***The current best estimate of the value of the function
          xmin = fgsl_multimin_fminimizer_minimum(min_fslv)
          !!***The current size of simplex
          simplex_size = fgsl_multimin_fminimizer_size(min_fslv)

          vstatus = fgsl_vector_align(locptr, locvec)

          write(*,*) 'Iteration', i
          write(*,*) 'Current Best'
          write(*,*) 'Parameters', locptr
          write(*,*) 'Objective function', xmin
          write(*,*) 'Size of Simplex', simplex_size

          !!See if simplex is small enough
          status = fgsl_multimin_test_size(simplex_size, eps)

          !!Exit if the size of simplex is below the criteria.
          if (status == FGSL_SUCCESS) then
             exit
          end if

       end do
    end if

    write(6, '(''Using the '',A,'' algorithm'')') trim(name)
    write(6, '(''Minimum at: '',1PE20.13)') locptr
    write(6, '(''Minimum is: '',1PE20.13)') xmin


    call fgsl_multimin_fminimizer_free(min_fslv)
    call fgsl_multimin_function_free(func)

  end subroutine GNU_NM_min
  
end module mod_GNU_min
