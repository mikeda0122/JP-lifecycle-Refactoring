program main_run

  use nag_library, only : nag_wp
  use mod_gmm
  use mod_parameter
  use mod_NAG_min
  use mod_NAG_pd_sym_inverse

  implicit none
  
  real(8) :: min_val
  real(8) :: min_point(7)
  !real(8) :: intval(7) = (/0.615_8, 7.96_8, 3399.0_8, 202.0_8, 240.0_8, 1.04_8, 0.037_8/) !para(4) nonscaled
  !*********French's unscaled initial values for para(4)**************************
  !p_gamh , p_gamc , p_leispref , p_leisureprefbad, p_fixcost , p_beta , p_bequest
  !0.644163_8, 9.72825_8, 3580.03_8, 296.475_8, 286.139_8, 1.00974_8, 0.0368076_8
  !*******************************************************************************

  !French's "scaled" initial values for para(4)
  real(KIND=nag_wp) :: intval(7)
  real(KIND=nag_wp) :: rvec(7,100)
  real(KIND=nag_wp) :: params_opt(7)
  !real(KIND=nag_wp) :: intval(7) = (/6.44163_8, 9.72825_8, 3.58003_8, 2.96475_8, 2.86139_8, 10.0974_8, 3.68076_8/) 

  !real(8) :: intval(7) = (/0.533_8, 3.19_8, 3900.0_8, 196.0_8, 335.0_8, 0.981_8, 1.7_8/) !para(3) nonscaled

  !real(8) :: intval(7) = (/0.602_8, 3.78_8, 4889.0_8, 191.0_8, 1292.0_8, 0.985_8, 2.58_8/) !para(2) nonscaled
  !real(8) :: intval(7) = (/0.578_8, 3.34_8, 4466.0_8, 318.0_8, 1313.0_8, 0.992_8, 1.69_8/) !para(1) nonscaled
  !real(fgsl_double) :: step(7) = (/0.01_8, 0.05_8, 30.0_8, 3.0_8, 10.0_8, 0.01_8, 0.005_8/)
  !real(8) :: step(7) = (/0.01_8, 0.05_8, 30.0_8, 3.0_8, 10.0_8, 0.01_8, 0.005_8/)

  real(KIND=nag_wp) :: y
  integer :: idummy(1)
  real(KIND=nag_wp) :: rdummy(1)
  
  real(8) :: a(4,4), ainv(4,4)

  character :: filename*128
  integer(8) :: i, j

  call timestamp()


  if (job==0_8) then
!     open(unit=2, file='params/simulation_first_step_params.csv')
!     open(unit=2, file='params/simulation_params_French_4.csv') 
     open(unit=2, file='params/simulation_params_JP.csv') 
!     open(unit=2, file='params/initial_test.csv') 
!     open(unit=2, file='params/simulation_params_JP_notied.csv') 
!     open(unit=2,file='params/simulation_params_our_optimal.csv')
!     open(unit=2,file='params/Estimation_results_vinv.csv')
     read(2,*) intval(1:7)
     close(2)
  else if (job==1_8 .or. job==2_8 .or. job==3_8) then
     open(unit=3, file='params/initial_params_French_4.csv') 
     read(3,*) intval(1:7)
     close(3)
  end if

  
  write(*,*) '****************Output and Jobs****************'
  write(*,*) 'write results into csv files', PrintResults
  write(*,*) 'Job number is', job
  write(*,*) ''
  write(*,*) ''

  write(*,*) '****************Estimation**************************'
  write(*,*) 'This is a set of initial values:'
  write(*,*) intval
  !write(*,*) 'Units of change in each step of Estimation procedure:'
  !write(*,*) step
  write(*,*) ''
  write(*,*) ''
  
  write(*,*) '****************Model Settings****************'
  write(*,*) 'dimension of parameters', size(intval)
  write(*,*) 'Tax type:', taxtype
  write(*,*) 'Liquidity dummy is', liquid
  write(*,*) 'Tied-wage is', tiedwage
  write(*,*) 'Wage adjustment for selection bias', select_adj
  write(*,*) 'Nonseparable utility function', nonsep
  write(*,*) 'Weight matrix is computed in two step', two_step
  write(*,*) ''
  write(*,*) ''

  write(*,*) '****************Numbers for computation****************'
  write(*,*) 'The number of A grids is', Asnum
  write(*,*) 'The number of AIME grids is', AIMEnum
  write(*,*) 'The number of W grids is', Wnum
  write(*,*) 'The number of H grids is', Hnum
  write(*,*) 'The number of C grids is', Cnum
  write(*,*) 'The number of obs in the simulation is ', simnum
  write(*,*) 'The number of obs in the data is ', datanum
  write(*,*) 'The number of moments used is ', momnum
  write(*,*) ''
  write(*,*) ''

  write(*,*) 'Value should be greater than', vpanish
  write(*,*) 'Asset should be greater than', dpanish
  write(*,*) ''
  write(*,*) ''


  if (job==0_8) then
     call gmm(size(intval), intval, y, idummy, rdummy)
     !write(*,*) 'The value of objective function is', y
  else if(job==1_8) then
     write(*,*) 'Start Estimation procedure!!'

     open(unit=10, file='params/init_params_French_4.csv')
     write(10, '(A)') 'p_gamh , p_gamc , p_leispref , p_leisureprefbad, p_fixcost , p_beta , p_bequest , obj'
     write(10,'(f10.5, a, f10.5, a, f10.5, a, f10.5, a, f10.5, a, f10.5, a, f10.5, &
          & a, f10.5)') intval(1)/10.0_nag_wp, ',', intval(2), ',', intval(3)*1000_nag_wp, ',', intval(4)*100.0_nag_wp, ',', intval(5)*100.0_nag_wp, ',', intval(6)/10.0_nag_wp, ',', intval(7)/100.0_nag_wp
     close(10)

     call NAG_min(intval)

  else if(job==2_8) then     
     write(*,*) 'Start the First Step of Estimation procedure!!'
     write(*,*) ''
     write(*,*) ''

     call NAG_min(intval)

     open(unit=3, file='params/first_step_params.csv')
    read(3,*) params_opt
     close(3)

     write(*,*) 'Compute Optimal Weighting Matrix!!'
     write(*,*) ''
     write(*,*) ''
     two_step = 1_8
     call gmm(size(intval), params_opt, y, idummy, rdummy)

     write(*,*) 'Start the Second Step of Estimation procedure!!'
     write(*,*) ''
     write(*,*) ''
     two_step = 2_8
     call NAG_min(intval)

  else if (job==3_8) then
     write(*,*) 'Start Estimation procedure!!'

     call random_number(rvec)
        
     rvec = rvec*10.0_8
     rvec(6,:) = 0.0_8
     rvec(3,:) = rvec(3,:)/100.0_8
     rvec(7,:) = rvec(7,:)*2.0_8
 
     do i = 3, 100

        open(unit=3, file='params/initial_params_JP.csv') 
        read(3,*) intval(1:7)
        close(3)
              
        init_iter = i
        
        write(filename, '("params/init_params_iter", i3.3, ".csv")') init_iter
        
        intval = intval + rvec(:,i)
        
        open(unit=10, file=filename, status='replace')
        write(10, '(A)') 'p_gamh , p_gamc , p_leispref , p_leisureprefbad, p_fixcost , p_beta , p_bequest , obj'
        write(10,'(f10.5, a, f10.5, a, f10.5, a, f10.5, a, f10.5, a, f10.5, a, f10.5, &
             & a, f10.5)') intval(1)/10.0_nag_wp, ',', intval(2), ',', intval(3)*1000_nag_wp, ',', intval(4)*100.0_nag_wp, ',', intval(5)*100.0_nag_wp, ',', intval(6)/10.0_nag_wp, ',', intval(7)/10.0_nag_wp
        close(10)
        
        call NAG_min(intval)
     end do

  end if
  
  write(*,*) 'start from 17 June 2019 5:15 PM'
  call timestamp()

contains
    subroutine timestamp ()

    !*****************************************************************************80
    !
    !! TIMESTAMP prints the current YMDHMS date as a time stamp.
    !
    !  Example:
    !
    !    31 May 2001   9:45:54.872 AM
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    18 May 2013
    !
    !  Author:
    !
    !    John Burkardt
    !
    !  Parameters:
    !
    !    None
    !
    implicit none

    character ( len = 8 ) ampm
    integer ( kind = 4 ) d
    integer ( kind = 4 ) h
    integer ( kind = 4 ) m
    integer ( kind = 4 ) mm
    character ( len = 9 ), parameter, dimension(12) :: month = (/ &
         'January  ', 'February ', 'March    ', 'April    ', &
         'May      ', 'June     ', 'July     ', 'August   ', &
         'September', 'October  ', 'November ', 'December ' /)
    integer ( kind = 4 ) n
    integer ( kind = 4 ) s
    integer ( kind = 4 ) values(8)
    integer ( kind = 4 ) y

    call date_and_time ( values = values )

    y = values(1)
    m = values(2)
    d = values(3)
    h = values(5)
    n = values(6)
    s = values(7)
    mm = values(8)

    if ( h < 12 ) then
       ampm = 'AM'
    else if ( h == 12 ) then
       if ( n == 0 .and. s == 0 ) then
          ampm = 'Noon'
       else
          ampm = 'PM'
       end if
    else
       h = h - 12
       if ( h < 12 ) then
          ampm = 'PM'
       else if ( h == 12 ) then
          if ( n == 0 .and. s == 0 ) then
             ampm = 'Midnight'
          else
             ampm = 'AM'
          end if
       end if
    end if

    write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
         d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

    return
  end subroutine timestamp
end program main_run
