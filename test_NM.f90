program test_NM

  use NM_min

  implicit none

  real(8) :: minval
  real(8) :: min_point(7)
  real(8) :: intval(7) = (/0.615_8, 7.96_8, 3399.0_8, 202.0_8, 240.0_8, 1.04_8, 0.031734/)
  real(8) :: step(7) = (/0.1_8, 0.5_8, 300.0_8, 30.0_8, 100.0_8, 0.1_8, 0.05_8/)

  integer(4) :: icount, num, ifault
  
  call nelmin(gmm, 7, intval, min_point, minval, 0.001_8, step, 3, 120, icount, num, ifault)

end program test_NM
