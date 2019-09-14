module mod_readmom

  use mod_parameter
  use mod_NAG_pd_sym_inverse

  implicit none

contains
  subroutine readmom(asset_mean, asset_median, wage_smoothed_good, wage_smoothed_bad, &
    & labor_hour_smoothed_good, labor_hour_smoothed_bad, labor_part_smoothed_good, labor_part_smoothed_bad, W)
    real(8), intent(out) :: asset_mean(:), asset_median(:), wage_smoothed_good(:), wage_smoothed_bad(:)
    real(8), intent(out) :: labor_hour_smoothed_good(:), labor_hour_smoothed_bad(:)
    real(8), intent(out) :: W(:,:)

    real(8) :: labor_part_smoothed_good(:), labor_part_smoothed_bad(:)
    integer :: ios
    integer(8) :: i, j, length, totobs

    real(8), allocatable:: ind_data(:,:) !datanum, 255) !A matrix consisting of individual data(dasset1.dat)
    real(8), allocatable :: weight_data(:,:) !(datanum, 255) !A matrix consisitng of individual data(weight1.dat)

    real(8), allocatable :: health_sim_data(:,:), death_sim_data(:,:), death_mat(:,:)

    real(8), allocatable :: uhdat(:,:), hhdat(:,:), updat(:,:), hpdat(:,:), assdat(:,:)
    real(8) :: asset_median_mat(1, momnum)
    real(8), allocatable :: VCV1(:,:), VCV2(:,:)

    real(8) :: temp_labor_hour_good(1,momnum), temp_labor_hour_bad(1,momnum), temp_lfp_good(1,momnum), &
    & temp_lfp_bad(1,momnum), temp_asset_mean(1,momnum), temp_asset_median(1,momnum)
    real(8), allocatable :: ones(:,:)
    real(8), allocatable :: mean_matrix(:,:)
    real(8), allocatable :: assobs(:,:), biga(:,:), obsmat(:,:)
    real(8), allocatable :: dev_VCV1(:,:), varcov(:,:)

    if (two_step==0_8 .or. two_step==2_8) then
       allocate(ind_data(datanum, 255))
       allocate(weight_data(datanum, 255))
       allocate(health_sim_data(datanum, momnum))
       allocate(death_sim_data(datanum, momnum))
       allocate(death_mat(datanum, 6*momnum))
       allocate(uhdat(datanum, momnum))
       allocate(hhdat(datanum, momnum))
       allocate(updat(datanum, momnum))
       allocate(hpdat(datanum, momnum))
       allocate(assdat(datanum, momnum))
       allocate(VCV1(datanum, 6*momnum))
       allocate(VCV2(datanum, 6*momnum))
       allocate(ones(datanum, 1))
       allocate(mean_matrix(datanum, 6*momnum))
       allocate(assobs(datanum, momnum))
       allocate(biga(datanum, momnum))
       allocate(obsmat(datanum, 6*momnum))
       allocate(dev_VCV1(datanum, 6*momnum))
       allocate(varcov(6*datanum, 6*momnum))
    else if (two_step==1_8) then
       allocate(ind_data(simnum, momnum*3))
       allocate(weight_data(simnum, momnum))
       allocate(health_sim_data(simnum, momnum))
       allocate(death_sim_data(simnum, momnum))
       allocate(death_mat(simnum, 6*momnum))
       allocate(uhdat(simnum, momnum))
       allocate(hhdat(simnum, momnum))
       allocate(updat(simnum, momnum))
       allocate(hpdat(simnum, momnum))
       allocate(assdat(simnum, momnum))
       allocate(VCV1(simnum, 6*momnum))
       allocate(VCV2(simnum, 6*momnum))
       allocate(ones(simnum, 1))
       allocate(mean_matrix(simnum, 6*momnum))
       allocate(assobs(simnum, momnum))
       allocate(biga(simnum, momnum))
       allocate(obsmat(simnum, 6*momnum))
       allocate(dev_VCV1(simnum, 6*momnum))
       allocate(varcov(6*simnum, 6*momnum))
    else
       write(*,*) 'Specify the way to determine a weight matrix!!'
       allocate(ind_data(simnum, momnum*3))
       allocate(weight_data(simnum, momnum))
       allocate(uhdat(simnum, momnum))
       allocate(hhdat(simnum, momnum))
       allocate(updat(simnum, momnum))
       allocate(hpdat(simnum, momnum))
       allocate(assdat(simnum, momnum))
       allocate(VCV1(simnum, 6*momnum))
       allocate(VCV2(simnum, 6*momnum))
       allocate(ones(simnum, 1))
       allocate(mean_matrix(simnum, 6*momnum))
       allocate(assobs(simnum, momnum))
       allocate(biga(simnum, momnum))
       allocate(obsmat(simnum, 6*momnum))
       allocate(dev_VCV1(simnum, 6*momnum))
       allocate(varcov(6*simnum, 6*momnum))
    end if

    !*********Read data moments
    length= size(asset_mean)

    if (two_step==0_8 .or. two_step==2_8) then
       open(100, file='JP_moments/JP_mean_data.csv',form='formatted',iostat=ios, status='old', action='read')
       !  open(9,file="out.txt")
       do i = 1, length
          read(100,*) labor_part_smoothed_good(i), labor_part_smoothed_bad(i), &
               & labor_hour_smoothed_good(i), labor_hour_smoothed_bad(i), &
               & asset_mean(i), asset_median(i)
          
!          write(*,*) labor_hour_smoothed_good(i), labor_hour_smoothed_bad(i), &
!               & labor_part_smoothed_good(i), labor_part_smoothed_bad(i), &
!               & asset_mean(i), asset_median(i)
!          read*
       end do

    else if (two_step==1_8) then
       !!Note that, in this case,
       !!1: H should not be log value.
       !!2: A should be scaled (devided by 1000000, Note mod_gmm.f90 l206 should be fixed in this case.).

       open(100, file='mean_sim_data.csv',form='formatted',iostat=ios, status='old', action='read')
       do i = 1, length
          read(100,*) asset_mean(i), asset_median(i), labor_part_smoothed_good(i), &
               & labor_part_smoothed_bad(i), labor_hour_smoothed_good(i), labor_hour_smoothed_bad(i)
       end do
    else
       write(*,*) 'Specify the way to determine a weight matrix!!'
    end if

    close(100)

    if (ios /= 0) then
       write(*,*) 'Failed to open'
       stop
    end if

    !****Make them two-dimension matrix for later purpose.
    where (labor_hour_smoothed_good(:)/=0.0_8)
       temp_labor_hour_good(1,:) = log(labor_hour_smoothed_good(:))
    else where
       temp_labor_hour_good(1,:) = 0.0_8
    end where

    where (labor_hour_smoothed_bad(:)/=0.0_8)
       temp_labor_hour_bad(1,:) = log(labor_hour_smoothed_bad(:))
    else where
       temp_labor_hour_bad(1,:) = 0.0_8
    end where
    temp_lfp_good(1,:) = labor_part_smoothed_good(:)
    temp_lfp_bad(1,:) = labor_part_smoothed_bad(:)
    temp_asset_mean(1,:) = asset_mean(:)
    temp_asset_median(1,:) = asset_median(:)

    ones = 1.0_8

    mean_matrix(:,1:momnum) = matmul(ones, temp_labor_hour_good)
    mean_matrix(:,1+momnum:2*momnum) = matmul(ones, temp_labor_hour_bad)
    mean_matrix(:,1+2*momnum:3*momnum) = matmul(ones, temp_lfp_good)
    mean_matrix(:,1+3*momnum:4*momnum) = matmul(ones, temp_lfp_bad)
    mean_matrix(:,1+4*momnum:5*momnum) = matmul(ones, temp_asset_mean)
    mean_matrix(:,1+5*momnum:6*momnum) = matmul(ones, temp_asset_median)

    if (two_step==1_8) then
       open(100, file='mean_data.csv',form='formatted',iostat=ios, status='old', action='read')
       !  open(9,file="out.txt")
       do i = 1, length
          read(100,*) asset_mean(i), asset_median(i), &
               & wage_smoothed_good(i), wage_smoothed_bad(i), labor_part_smoothed_good(i), &
               & labor_part_smoothed_bad(i), labor_hour_smoothed_good(i), labor_hour_smoothed_bad(i)
       end do

    close(100)
    end if
!    write(*,*) 'mean_matrix5', mean_matrix(:,5)
!    write(*,*) 'mean_matrix35', mean_matrix(:,35)
!    write(*,*) 'mean_matrix67', mean_matrix(:,67)
!    write(*,*) 'mean_matrix98', mean_matrix(:,98)
!    write(*,*) 'mean_matrix120', mean_matrix(:,120)
!    write(*,*) 'mean_matrix157', mean_matrix(:,157)

!*****************Compute optimal weight, W*******************
    !***Read Individual Data
    if (two_step==0_8 .or. two_step==2_8) then
       totobs = datanum

       open(99, file='ind_data.csv',form='formatted',iostat=ios, status='old', action='read')
       do i = 1, totobs
          read(99,*) ind_data(i,1:255)
       end do

       !***Only first "momnum"th vectors are what we need.
       !!In the data, the 1st column is "work hours for unhealthy"
       uhdat(:,:) = ind_data(:,1:momnum)
       !!The 2nd column is "work hours for healthy"
       hhdat(:,:) = ind_data(:,1+51:momnum+51)
       !!The 3rd column is "labor force participation for unhealthy"
       updat(:,:) = ind_data(:,1+51*2:momnum+51*2)
       !!The 4th column is "labor force participation for healthy"
       hpdat(:,:) = ind_data(:,1+51*3:momnum+51*3)
       !!The 5th column is "asset"
       assdat(:,:) = ind_data(:,1+51*4:momnum+51*4)

       VCV1(:,1:momnum) = hhdat(:,:)
       VCV1(:,1+momnum:2*momnum) = uhdat(:,:)
       VCV1(:,1+2*momnum:3*momnum) = hpdat(:,:)
       VCV1(:,1+3*momnum:4*momnum) = updat(:,:)
       VCV1(:,1+4*momnum:5*momnum) = assdat(:,:)

       close(99)

       !***Read Weight Data
       open(98, file='weight_data.csv',form='formatted',iostat=ios, status='old', action='read')
       do i = 1, totobs
          read(98,*) weight_data(i,1:255)
       end do

       if (ios /= 0) then
          write(*,*) 'Failed to open'
          stop
       end if

       assobs(:,:) = weight_data(:,1+51*4:momnum+51*4)

       VCV2(:,1:momnum) = weight_data(:,1:momnum)
       VCV2(:,1+momnum:2*momnum) = weight_data(:,1+51:momnum+51)
       VCV2(:,1+2*momnum:3*momnum) = weight_data(:,1+51*2:momnum+51*2)
       VCV2(:,1+3*momnum:4*momnum) = weight_data(:,1+51*3:momnum+51*3)
       VCV2(:,1+4*momnum:5*momnum) = assobs(:,:)

       close(98)

    else if (two_step==1_8) then
       totobs = simnum

       !***Read Individual Level simulated data
       !!Note that, in this case,
       !!1: H should not be log value.
       !!2: A should be scaled (devided by 1000000, Note mod_simulation_prof.f90 l163 should be fixed in this case.).
       open(99, file='ind_sim_data.csv',form='formatted',iostat=ios, status='old', action='read')
       do i = 1, totobs
             read(99,*) ind_data(i,1:momnum*3)
       end do

       !***Only first "momnum"th vectors are what we need.
       uhdat(:,:) = ind_data(:,1:momnum)
       updat(:,:) = ind_data(:,1+momnum:momnum*2)
       assdat(:,:) = ind_data(:,1+momnum*2:momnum*3)

       where (ind_data(:,1:momnum)/=0.0_8)
          VCV1(:,1:momnum) = log(ind_data(:,1:momnum))
          VCV1(:,1+momnum:momnum*2) = log(ind_data(:,1:momnum))
       else where
          VCV1(:,1:momnum) = 0
          VCV1(:,1+momnum:momnum*2) = 0
       end where
       VCV1(:,1+2*momnum:3*momnum) = ind_data(:,1+momnum:momnum*2)
       VCV1(:,1+3*momnum:4*momnum) = ind_data(:,1+momnum:momnum*2)
       VCV1(:,1+4*momnum:5*momnum) = assdat(:,:)
       VCV1(:,1+5*momnum:6*momnum) = assdat(:,:)

       close(99)

       !***Read Health Data       
       open(98, file='healsim.csv',form='formatted',iostat=ios, status='old', action='read')
       do i = 1, totobs
          read(98,*) health_sim_data(i,1:momnum)
       end do

       if (ios /= 0) then
          write(*,*) 'Failed to open'
          stop
       end if

       assobs(:,:) = 1.0_8

       VCV2(:,1:momnum) = health_sim_data(:,1:momnum)*updat
       VCV2(:,1+momnum:2*momnum) = (1.0_8-health_sim_data(:,1:momnum))*updat
       VCV2(:,1+2*momnum:3*momnum) = health_sim_data(:,1:momnum)
       VCV2(:,1+3*momnum:4*momnum) = 1.0_8-health_sim_data(:,1:momnum)
       VCV2(:,1+4*momnum:6*momnum) = 1.0_8

       !***Read Death status Data for observation matrix
       open(97, file='deathsim.csv',form='formatted',iostat=ios, status='old', action='read')
       do i = 1, totobs
          read(97,*) death_sim_data(i,1:momnum)
       end do

       death_mat(:,1:momnum) = death_sim_data
       death_mat(:,1+momnum:2*momnum) = death_sim_data
       death_mat(:,1+2*momnum:3*momnum) = death_sim_data
       death_mat(:,1+3*momnum:4*momnum) = death_sim_data
       death_mat(:,1+4*momnum:5*momnum) = death_sim_data
       death_mat(:,1+5*momnum:6*momnum) = death_sim_data

       close(97)
    else
       write(*,*) 'Specify the way to determine a weight matrix!!'
       totobs = 1_8
    end if

    if (ios /= 0) then
       write(*,*) 'Failed to open'
       stop
    end if


    where (assdat>mean_matrix(:,1+5*momnum:6*momnum))
       biga = 1.0_8*assobs + 0.0000001_8*assobs
    else where
       biga = 0.0_8 + 0.0000001_8*assobs
    end where

    VCV1(:,1+5*momnum:6*momnum) = biga(:,:)
    VCV2(:,1+5*momnum:6*momnum) = assobs(:,:)

    if (two_step==1_8) then
       where (death_mat==0.0_8)
          obsmat = 1.0_8
       else where
          obsmat = 0.0_8
       end where
    else
       where (VCV1/=0.0_8)
          obsmat = 1.0_8
       else where
          obsmat = 0.0_8
       end where
    end if

    !***Compute Optimal Diagonal Matrix
    dev_VCV1 = VCV1 - mean_matrix*obsmat
    dev_VCV1 = dev_VCV1*VCV2

    varcov = matmul(transpose(dev_VCV1), dev_VCV1)

    if (French_weight==1_8) then
       varcov = 0.0_8
       W = 0.0_8

       !****Read an weighting matrix French actually used
       open(43, file='JP_moments/JP_weight_data.csv', form='formatted', iostat=ios, status='old', action='read')
       do i = 1, momnum
          !!!!Data: hour_good, hour_bad, lfp_good, lfp_bad, a_mean, a_med
          !!!!They should be ordered as follows: hour_good, hour_bad, lfp_good, lfp_bad, a_mean, a_med
          read(43,*) varcov(i,i), varcov(i+momnum, i+momnum), varcov(i+2*momnum, i+2*momnum), &
               & varcov(i+3*momnum, i+3*momnum), varcov(i+4*momnum, i+4*momnum), varcov(i+5*momnum, i+5*momnum)
          do j = 0, 5
             W(i+momnum*j,i+momnum*j) = 1/varcov(i+momnum*j,i+momnum*j)**2
!             write(*,*) W(i+momnum*j,i+momnum*j)
          end do
       end do
       close(43)
    else
       if (two_step==1_8) then
          !*****Use the inverse of varcov as an weighting matrix
          varcov = varcov/totobs
          call NAG_pd_sym_inverse(varcov, W)
          open(41, file='w_opt.csv')
          do i = 1, 6*momnum
             write(41,*) W(i, 1:6*momnum)
          end do
          close(41)
       else if (two_step==2_8) then
          open(40, file='w_opt.csv', form='formatted', iostat=ios, status='old', action='read')
          do i = 1, 6*momnum
             read(40,*) W(i, 1:6*momnum)
             write(*,*) i, W(i,i)
          end do
          close(40)
       else
          !*****Use the diagonal elements of varcov as weighting matrix
          varcov = varcov/totobs
          W = 0
          do j = 1, 6*momnum
             W(j,j) = 1/varcov(j,j)
          end do
       end if
    end if

    deallocate(ind_data)
    deallocate(weight_data)
    deallocate(health_sim_data)
    deallocate(death_sim_data)
    deallocate(death_mat)
    deallocate(uhdat)
    deallocate(hhdat)
    deallocate(updat)
    deallocate(hpdat)
    deallocate(assdat)
    deallocate(VCV1)
    deallocate(VCV2)
    deallocate(ones)
    deallocate(mean_matrix)
    deallocate(assobs)
    deallocate(biga)
    deallocate(obsmat)
    deallocate(dev_VCV1)
    deallocate(varcov)

  end subroutine readmom
end module mod_readmom
