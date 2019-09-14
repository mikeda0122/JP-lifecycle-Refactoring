program test

  use mod_parameter
  use mod_readmom
  
  implicit none
  
  real(8) :: data_mean_A(momage2-momage+1), data_median_A(momage2-momage+1)
  real(8) :: data_mean_H_good(momage2-momage+1), data_mean_H_bad(momage2-momage+1)
  real(8) :: data_mean_P_good(momage2-momage+1), data_mean_P_bad(momage2-momage+1)
  real(8) :: data_mean_W_good(momage2-momage+1), data_mean_W_bad(momage2-momage+1)

  call readmom(data_mean_A, data_median_A, data_mean_W_good, data_mean_W_bad, data_mean_H_good, data_mean_H_good, data_mean_P_good, data_mean_P_bad)
end program test
