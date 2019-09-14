module mod_gmm
  
  use nag_library, only : nag_wp
  use mod_setPoI
  use mod_parameter
  use mod_profwage
  use mod_health_mortality
  use mod_ageshifter
  use mod_sprob
  use mod_initialdist
  use mod_simulation_prof
  use mod_decrule
  use mod_doquant
  use mod_condmean
  use mod_readmom
  use mod_policy_params
  use mod_AIME_params
  use mod_JP_pension

  implicit none

contains

  subroutine gmm(n, params, obj, iuser, ruser)

    IMPLICIT NONE
    real(KIND=nag_wp), intent(out) :: obj
    integer, intent(in) :: n
    
    real(KIND=nag_wp), intent(inout) :: ruser(*)
    real(KIND=nag_wp), intent(in) :: params(n)

    integer, intent(inout) :: iuser(*)

    !real(8) :: p_conspref
    !real(8) :: p_gamc
    !real(8) :: p_gamh
    !real(8) :: p_leispref
    !real(8) :: p_leisprefbad
    !real(8) :: p_fixcost
    !real(8) :: p_L
    !real(8) :: p_bequest
    !real(8) :: p_onemgamc
    !real(8) :: p_onemgamh
    !real(8) :: p_beqk

    !real(8):: rhow     = 0.977
    !real(8):: stderr = 0.12_8

    real(8) :: A_dist(simnum), M_dist(simnum), W_dist(simnum), AIME_dist(simnum)

    real(8) :: Astate(Asnum)
    real(8) :: Wstate(Wnum)

    !real(8) :: mortality_good(dieage-bornage+1), mortality_bad(dieage-bornage+1), good_to_bad(dieage-bornage+1), bad_to_bad(dieage-bornage+1)
    !real(8) :: hlogwage(dieage-bornage+1), ulogwage(dieage-bornage+1)
    !real(8) :: hhgr(dieage-bornage+1), hugr(dieage-bornage+1), uhgr(dieage-bornage+1), uugr(dieage-bornage+1)
    !real(8) :: gvec(dieage-bornage+1), ageshift(dieage-bornage+1)

    real(8) :: mortality_good(66_8), mortality_bad(66_8), good_to_bad(66_8), bad_to_bad(66_8)
    real(8) :: ulogwage(65_8), hlogwage(65_8)
    real(8) :: hhgr(65_8), hugr(65_8), uhgr(65_8), uugr(65_8)
    real(8) :: gvec(66_8), ageshift(66_8)
    real(8) :: AIMErate(52_8), AIMEbrackets(30_8), AIMEstd(31_8)
    real(8), allocatable :: low_brackets(:), up_brackets(:)
    real(8) :: HI_payments(50_8), HI_payments_elderly(50_8), HI_brackets(49_8), pension_brackets(32_8), pension_payments(32_8)

    real(8) :: optC_good(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum), optC_bad(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum)
    real(8) :: optA_good(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum), optA_bad(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum)
    real(8) :: optH_good(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum), optH_bad(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum)
    integer(8) :: optP_good(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum), optP_bad(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum)
    integer(8) :: optB_good(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum), optB_bad(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum)
    real(8) :: optW_good_good(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum),&
                & optW_good_bad(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum)
    real(8) :: optW_bad_good(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum), &
                & optW_bad_bad(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum)
    real(8) :: optAIME_good(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum),&
                & optAIME_bad(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum)
    real(8) :: optI_good(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum), optI_bad(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum)
    real(8) :: optpb_good(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum), &
                & optpb_bad(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum)
    real(8) :: optss_good(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum), &
                & optss_bad(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum)

    real(8) :: mean_prof_C(dieage-bornage+1), mean_prof_A(dieage-bornage+1), med_prof_A(dieage-bornage+1)
    real(8) :: mean_prof_H_good(dieage-bornage+1), mean_prof_H_bad(dieage-bornage+1),&
               & mean_prof_H(dieage-bornage+1), mean_prof_H_work_good(dieage-bornage+1),&
               & mean_prof_H_work_bad(dieage-bornage+1), mean_prof_H_work(dieage-bornage+1)
    real(8) :: mean_prof_P_good(dieage-bornage+1), mean_prof_P_bad(dieage-bornage+1), &
               & mean_prof_P(dieage-bornage+1)
    real(8) :: mean_prof_B(dieage-bornage+1), mean_prof_AIME(dieage-bornage+1)

    integer(8) :: simdeath(simnum, dieage-bornage+1)
    real(8) :: simhealth(simnum, dieage-bornage+1), simA(simnum, dieage-bornage+1), &
                & simC(simnum, dieage-bornage+1)
    real(8) :: simH(simnum, dieage-bornage+1), simAIME(simnum, dieage-bornage+1), &
                & simP(simnum, dieage-bornage+1), simB(simnum, dieage-bornage+1)

    !real(8), allocatable :: temp_death(:)
    real(8) :: temp_adata(simnum)

    real(8) :: data_mean_A(momage2-momage+1), data_median_A(momage2-momage+1)
    real(8) :: data_mean_H_good(momage2-momage+1), data_mean_H_bad(momage2-momage+1)
    real(8) :: data_mean_P_good(momage2-momage+1), data_mean_P_bad(momage2-momage+1)

    real(8) :: data_mean_W_good(momage2-momage+1), data_mean_W_bad(momage2-momage+1)

    real(8) :: smoments(momnum*6, 1), dmoments(momnum*6, 1), distance(momnum*6, 1), part_dist(momnum,1)
    real(8) :: temp_mat(1, momnum*6), gmm_mat(1,1), temp_part(1,momnum), gmm_part_mat(1,1), obj_part(6,1)

    real(8) :: W(momnum*6,momnum*6)
    real(8) :: W_part(momnum,momnum)
    integer(8) :: age, indx, i, j, ios, m

    real(8) :: AIMEstate(AIMEnum)

    INTRINSIC matmul

    write(*,*) 'params', params
    
    !************
    !****We use parameters of interest as global variable, which is not good. Any way to fix this?
    !write(*,*) params(1)
    call setPoI(params)
    !write(*,*) params
    !write(*,*) 'here', p_conspref, p_leispref-p_fixcost-500.0_8
    !write(*,*) 'p_leispref', p_leispref
    !write(*,*) 'p_fixcost', p_fixcost

    write(*,*) 'params unscaled', p_gamh, p_gamc, p_leispref, p_leisprefbad, p_fixcost, p_beta, p_bequest

    if (p_leispref-p_fixcost-500.0_8<=0.0_8) then
       write(*,*) 'leisure parameter is too low!!'
!       read*
       !In this case, p_conspref is not defined.
       obj = 10000000000000000.0_8
       return
    end if

    call readAIMEparams(AIMErate, AIMEbrackets, AIMEstd)

    m = size(AIMEbrackets)
    allocate(low_brackets(m+1))
    allocate(up_brackets(m+1))
    call createAIMEbrackets(AIMEbrackets, low_brackets, up_brackets)

    call readTaxparams(HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments)
    call health_mortality_2(mortality_good, mortality_bad, good_to_bad, bad_to_bad)
    call profwage(hlogwage, ulogwage, hhgr, hugr, uhgr, uugr)
    call computeg_2(gvec)
    call ageshifter(ageshift)

    call make_A(Astate)
    !print*, "Asset Grid", Astate
    !call make_H(Hstate)
    !print*, "H Grid", Hstate
    call make_W(Wstate)
    !print*, "Wage Grid", Wstate
    call make_AIME(AIMEstate)
    !print*, "AIME Grid", AIMEstate
    !call make_C2(Cstate)
    !  print*, "C Grid", Cstate
    !call make_AIME_2(95_8, AIMEstate)
    !write(*,*) AIMEstate

    !********Construct data moments
    call readmom(data_mean_A, data_median_A, data_mean_W_good, data_mean_W_bad, data_mean_H_good, &
    & data_mean_H_bad, data_mean_P_good, data_mean_P_bad, W)

    dmoments(1:momnum, 1) = log(data_mean_H_good(1:momnum))
    dmoments(1+momnum:momnum*2, 1) = log(data_mean_H_bad(1:momnum))
    dmoments(1+momnum*2:momnum*3, 1) = data_mean_P_good(1:momnum)
    dmoments(1+momnum*3:momnum*4, 1) = data_mean_P_bad(1:momnum)
    dmoments(1+momnum*4:momnum*5, 1) = data_mean_A(1:momnum)
    dmoments(1+momnum*5:momnum*6, 1) = data_median_A(1:momnum)

    call initialdist(A_dist, M_dist, W_dist, AIME_dist)

    write(*,*) 'Calculating Decision rules...'
    call decrule(Astate, Wstate, AIMEstate, mortality_good, mortality_bad, good_to_bad, &
      & bad_to_bad, HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
      & AIMErate, AIMEstd, low_brackets, up_brackets, &
      & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, &
      & optC_good, optC_bad, optA_good, optA_bad, optH_good, optH_bad, optP_good, optP_bad, &
      & optB_good, optB_bad, optW_good_good, optW_good_bad, optW_bad_good, optW_bad_bad, &
      & optAIME_good, optAIME_bad, optI_good, optI_bad, optpb_good, optpb_bad, optss_good, optss_bad)

    !write(*,*) 'Hopt_good', optH_good

    write(*,*) 'Simulating individual profiles from the model...'
    call simulation_prof(A_dist, M_dist, W_dist, AIME_dist, mortality_good, mortality_bad, &
         good_to_bad, bad_to_bad, &
         HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
         AIMErate, AIMEstd, low_brackets, up_brackets, &
         hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, &
         optC_good, optC_bad, optA_good, optA_bad, optH_good, optH_bad, &
         optP_good, optP_bad, optB_good, optB_bad, &
         optW_good_good, optW_good_bad, optW_bad_good, optW_bad_bad, optAIME_good, optAIME_bad, &
         optI_good, optI_bad, optpb_good, optpb_bad, optss_good, optss_bad, &
         & Astate, Wstate, AIMEstate, simdeath, simhealth, simC, simA, simH, simP, simB, simAIME)
    write(*,*) 'Computing Simulated moments...'
    call condmean(simdeath, simhealth, simC, simA, simH, simP, simB, simAIME, mean_prof_C, mean_prof_A,&
    & mean_prof_H_good, mean_prof_H_bad, mean_prof_H, mean_prof_H_work_good, mean_prof_H_work_bad,&
    & mean_prof_H_work, mean_prof_P_good, mean_prof_P_bad, mean_prof_P, mean_prof_B, mean_prof_AIME)

    do age = bornage, dieage-1, 1
       indx = age-bornage+1
       temp_adata = simA(:,indx)
       do i = 1, simnum
          if (simdeath(i, indx)==1_8) then
             temp_adata(i) = dpanish-1.0_8
          end if
       end do
       call doquant(temp_adata, simnum, 0.5_8, med_prof_A(indx))
    end do

    if (PrintResults==1_8) then
       open(unit=22, file='simulated_moments.csv')

       write(22, "(A)") 'age, C, mean_A, med_A, H, H_bad, H_good, H_work, H_work_bad, H_work_good,&
       & P, P_bad, P_good, B'

       do age = bornage, dieage-1, 1
          indx = age - bornage + 1
          write(22, '(i3, a, f18.10, a, f18.10, a, f18.10, a, f18.10, a, f18.10, a, f18.10, a, f18.10, &
          & a, f18.10, a, f18.10, a, f18.10, a, f18.10, a, f18.10, a, f18.10)') age, &
           ',', mean_prof_C(indx), ',', mean_prof_A(indx), ',', med_prof_A(indx), &
           ',', mean_prof_H(indx), ',', mean_prof_H_bad(indx), ',', mean_prof_H_good(indx), &
           ',', mean_prof_H_work(indx), ',', mean_prof_H_work_bad(indx), ',', mean_prof_H_work_good(indx), &
           ',', mean_prof_P(indx), ',', mean_prof_P_bad(indx), ',', mean_prof_P_good(indx), &
           ',', mean_prof_B(indx)
       end do
       close(22)

       open(unit=23, file='mean_sim_data.csv')
       
       do age = bornage, dieage-1, 1
          indx = age - bornage + 1
          write(23,*) mean_prof_A(indx), ',', med_prof_A(indx), &
               ',', mean_prof_P_good(indx), ',', mean_prof_P_bad(indx), &
               ',', mean_prof_H_work_good(indx), ',', mean_prof_H_work_bad(indx)
       end do
       close(23)
    end if

    do age = bornage, dieage, 1
       indx = age - bornage + 1
       if (mean_prof_H_good(indx)==0.0_8) then
          mean_prof_H_good(indx) = 1.0_8
          mean_prof_H_bad(indx) = 1.0_8
       end if
    end do

    where (mean_prof_H_work_good(30-bornage+1:69-bornage+1)/=0.0_8)
    smoments(1:momnum,1) = log(mean_prof_H_work_good(30-bornage+1:69-bornage+1))
    else where
    smoments(1:momnum,1) =  0.0_8
    end where
    where (mean_prof_H_work_bad(30-bornage+1:69-bornage+1)/=0.0_8)
    smoments(momnum+1:momnum*2,1) = log(mean_prof_H_work_bad(30-bornage+1:69-bornage+1))
    else where
    smoments(momnum+1:momnum*2,1) = 0.0_8
    end where

    smoments(momnum*2+1:momnum*3,1) = mean_prof_P_good(30-bornage+1:69-bornage+1)
    smoments(momnum*3+1:momnum*4, 1) =mean_prof_P_bad(30-bornage+1:69-bornage+1)
    smoments(momnum*4+1:momnum*5,1) = mean_prof_A(30-bornage+1:69-bornage+1)
    smoments(momnum*5+1:momnum*6,1) = med_prof_A(30-bornage+1:69-bornage+1)

!    write(*,*) 'sim A_mean'
!    write(*,*) smoments(momnum*4+1:momnum*5,1)

!    write(*,*) 'data A_mean'
!    write(*,*) dmoments(momnum*4+1:momnum*5,1)

!    write(*,*) 'weight A_mean'
!    do age = momnum*4+1, momnum*5
!       write(*,*) W(age,age)
!    end do

!    write(*,*) 'sim A_mean'
!    write(*,*) smoments(momnum*2+1:momnum*3,1)

!    write(*,*) 'data A_mean'
!    write(*,*) dmoments(momnum*2+1:momnum*3,1)

!    write(*,*) 'weight H_good'
!    do age = momnum*2+1, momnum*3
!       write(*,*) W(age,age)
!    end do

    if (objective_part==1_8) then
       do i = 1, 6
          W_part(1:momnum, 1:momnum) = W(1+momnum*(i-1):momnum*i, 1+momnum*(i-1):momnum*i)
!          write(*,*) 'W_part'
!          write(*,*) W_part
          part_dist(1:momnum, 1) = dmoments(1+momnum*(i-1):momnum*i, 1) - smoments(1+momnum*(i-1):momnum*i, 1)
!          write(*,*) 'part_dist'
!          write(*,*) part_dist
          temp_part = matmul(transpose(part_dist), W_part)
!          write(*,*) 'temp_part'
!          write(*,*) temp_part
          gmm_part_mat = matmul(temp_part, part_dist)
          obj_part(i,1) = gmm_part_mat(1,1)
          !write(*,*) 'W_part', W_part(1,1), W_part(20,20)
          !write(*,*) 'W', W(momnum*(i-1)+1, momnum*(i-1)+1), W(momnum*(i-1)+20, momnum*(i-1)+20)
       end do
       write(*,*) 'H_good, H_bad, P_good, P_bad, mean_A, median_A'
       write(*,*) obj_part
    end if
    
    !********Compute distance and gmm objective function
    distance = smoments - dmoments

    temp_mat = matmul(transpose(distance), W)
    gmm_mat = matmul(temp_mat, distance)
    obj = gmm_mat(1, 1)

    write(*,*) 'objective function', obj

  end subroutine gmm

end module mod_gmm
