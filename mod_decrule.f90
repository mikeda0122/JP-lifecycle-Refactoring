module mod_decrule

  use mod_parameter
  use mod_makegrids
  use mod_optmum0
  use mod_optmum1
  use mod_optmum2
  use mod_optmum3_gsearch
  !use mod_simulation
  use mod_health_mortality
  use mod_profwage
  use mod_ageshifter

  implicit none

contains

  subroutine decrule(Astate, Wstate, AIMEstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
       HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
       AIMErate, AIMEstd, low_brackets, up_brackets, &
       hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, &
       optC_good, optC_bad, optA_good, optA_bad, optH_good, optH_bad, optP_good, optP_bad, &
       optB_good, optB_bad, optW_good_good, optW_good_bad, optW_bad_good, optW_bad_bad, optAIME_good, optAIME_bad, &
       optI_good, optI_bad, optpb_good, optpb_bad, optss_good, optss_bad)

    !Exogenous grids
    real(8), intent(in) :: Astate(:), Wstate(:), AIMEstate(:)

    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)
    real(8), intent(in) :: HI_payments(:), HI_payments_elderly(:), HI_brackets(:)
    real(8), intent(in) :: pension_brackets(:), pension_payments(:)
    real(8), intent(in) :: AIMErate(:), AIMEstd(:), low_brackets(:), up_brackets(:)
    real(8), intent(in) :: hlogwage(:), ulogwage(:)
    real(8), intent(in) :: hhgr(:), hugr(:), uhgr(:), uugr(:)
    real(8), intent(in) :: gvec(:), ageshift(:)

    real(8), intent(out) :: optC_good(:,:,:,:,:), optC_bad(:,:,:,:,:)
    real(8), intent(out) :: optA_good(:,:,:,:,:), optA_bad(:,:,:,:,:)
    real(8), intent(out) :: optH_good(:,:,:,:,:), optH_bad(:,:,:,:,:)
    integer(8), intent(out) :: optP_good(:,:,:,:,:), optP_bad(:,:,:,:,:)
    integer(8), intent(out) :: optB_good(:,:,:,:,:), optB_bad(:,:,:,:,:)
    real(8), intent(out) :: optW_good_good(:,:,:,:,:), optW_good_bad(:,:,:,:,:)
    real(8), intent(out) :: optW_bad_good(:,:,:,:,:), optW_bad_bad(:,:,:,:,:)
    real(8), intent(out) :: optAIME_good(:,:,:,:,:), optAIME_bad(:,:,:,:,:)
    real(8), intent(out) :: optI_good(:,:,:,:,:), optI_bad(:,:,:,:,:)
    real(8), intent(out) :: optpb_good(:,:,:,:,:), optpb_bad(:,:,:,:,:)
    real(8), intent(out) :: optss_good(:,:,:,:,:), optss_bad(:,:,:,:,:)


    real(8) :: Hstate(Hnum), Cstate(Cnum)
    real(8) :: Copt, Aopt, Hopt, Wopt_good, Wopt_bad, AIMEopt, Iopt, pbopt, ssopt, valopt
    integer(8) :: Bopt
    real(8) :: M

    real(8) :: Vgood(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum), Vbad(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum)

    integer(8) :: Ai, Wi, AIMEi, Bi, Bidx
    integer(8) :: age
    real(8) :: Cinit, Hinit

    real(8), allocatable :: A_dist(:), M_dist(:), W_dist(:), AIME_dist(:)

    integer(8) :: sim_length
    integer(8) :: j
    integer, dimension(3) :: time_start
    integer, dimension(3) :: time_end
    call itime(time_start)

    call make_H(Hstate)
    call make_C2(Cstate)

    !  do age=dieage, bornage, -1
    !     write(*,*) 'age:', age
    !     write(*,*) 'gvec:', gvec(age-bornage+1)
    !     write(*,*) 'ageshift:', ageshift(age-bornage+1)
    !  end do

    if (PrintResults==1_8) then
       open(unit=15, file='valuesopt.csv')
       write(15,"(A)") "age, M, A, Aindex, AIME, AIMEindex, Wage, Windex, Aopt, Copt, Hopt, &
            & Bopt, nextAIMEopt, Iopt, pbopt, ssopt, value"
    end if

    do Ai = 1, Asnum
       do AIMEi = 1, AIMEnum

          call optmum3_gsearch(95_8, Astate(Ai), AIMEstate(AIMEi), Astate, AIMEstate, Cstate, &
               & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
               & 0.0_8, Copt, Aopt, Iopt, pbopt, ssopt, valopt)
          do Bi = 1, 2
             Vgood(95_8-bornage+1_8, AIMEi, 1_8, Ai, Bi)=valopt
             optC_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Copt
             optA_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Aopt
             optH_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=0.0_8
             optB_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=1_8
             optW_good_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=0.0_8
             optW_good_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=0.0_8
             optAIME_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=AIMEstate(AIMEi)
             optI_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Iopt
             optpb_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=pbopt
             optss_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=ssopt
          end do
          if (PrintResults==1_8) then
             write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a,&
                  &  f10.2, a, f10.2, a, i2, a, f10.2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
                  95, ',', 0.0_8, ',', Astate(Ai), ',', Ai, ',', AIMEstate(AIMEi), ',', AIMEi, ',', &
                  & 0.0_8, ',', 1_8, ',', Aopt, ',', Copt, ',', 0.0_8, ',', 1_8, ',', AIMEstate(AIMEi),',', &
                  & Iopt,',', pbopt,',', ssopt,',', valopt
          end if

          call optmum3_gsearch(95_8, Astate(Ai), AIMEstate(AIMEi), Astate, AIMEstate, Cstate, &
               & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
               & 1.0_8, Copt, Aopt, Iopt, pbopt, ssopt, valopt)
          do Bi = 1, 2
             Vbad(95_8-bornage+1_8, AIMEi, 1_8, Ai, Bi)=valopt
             optC_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Copt
             optA_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Aopt
             optH_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=0.0_8
             optB_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=1_8
             optW_bad_good(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=0.0_8
             optW_bad_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=0.0_8
             optAIME_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=AIMEstate(AIMEi)
             optI_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Iopt
             optpb_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=pbopt
             optss_bad(95-bornage+1_8, AIMEi, 1_8, Ai, Bi)=ssopt
          end do

          if (PrintResults==1_8) then
             write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a,&
                  & f10.2, a, i2, a, f10.2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
                  &     95, ',', 1.0_8, ',', Astate(Ai), ',', Ai, ',', AIMEstate(AIMEi), ',', AIMEi, ',',  0.0_8, ',',&
                  &     1_8, ',', Aopt, ',', Copt, ',', 0.0_8, ',', 1_8, ',', AIMEstate(AIMEi),',', Iopt,',', pbopt,',', ssopt,',', valopt
          end if

          do Bi = 1, 2
             do Wi = 2, Wnum
                Vgood(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = Vgood(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optC_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optC_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optA_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optA_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optH_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optH_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optB_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optB_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optW_good_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optW_good_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optW_good_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optW_good_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optAIME_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optAIME_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optI_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optI_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optpb_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optpb_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optss_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optss_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)

                Vbad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = Vbad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optC_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optC_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optA_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optA_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optH_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optH_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optB_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optB_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optW_bad_good(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optW_bad_good(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optW_bad_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optW_bad_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optAIME_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optAIME_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optI_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optI_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optpb_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optpb_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
                optss_bad(95_8-bornage+1_8, AIMEi, Wi, Ai, Bi) = optss_bad(95_8-bornage+1, AIMEi, 1_8, Ai, Bi)
             end do
          end do

       end do !AIME loop
    end do !A loop

    do age = dieage-1, retage+1, -1 !age 94-71
       !write(*,*) age
       !call make_AIME_2(age, AIMEstate)
       do Ai = 1, Asnum
          do AIMEi = 1, AIMEnum
             call optmum2(age, Astate(Ai), AIMEstate(AIMEi), 0.0_8, Vgood, Vbad, Astate, Wstate, AIMEstate, Cstate, &
                  & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
                  & mortality_good, mortality_bad, good_to_bad, bad_to_bad, Copt, Aopt, Iopt, pbopt, ssopt, valopt)
             do Bi =1, 2
                Vgood(age-bornage+1_8, AIMEi, 1_8, Ai, Bi) = valopt
                optC_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Copt
                optA_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Aopt
                optH_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=0.0_8
                optB_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=1_8
                optW_good_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=3.0_8
                optW_good_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=3.0_8
                optAIME_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=AIMEstate(AIMEi)
                optI_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Iopt
                optpb_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=pbopt
                optss_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=ssopt
             end do

             if (PrintResults==1_8) then
                write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a,&
                     & i2, a, f10.2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
                     age,',', 0.0_8,',', Astate(Ai),',', Ai,',', AIMEstate(AIMEi), ',', AIMEi,',', 3.0_8,',', 1_8,',', Aopt,',',&
                     & Copt,',', 0.0_8,',', 1_8,',', AIMEstate(AIMEi),',', Iopt,',', pbopt,',', ssopt, ',', valopt
             end if

             call optmum2(age, Astate(Ai), AIMEstate(AIMEi), 1.0_8, Vgood, Vbad, Astate, Wstate, AIMEstate, Cstate, &
             & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
             & mortality_good, mortality_bad, good_to_bad, bad_to_bad, Copt, Aopt, Iopt, pbopt, ssopt, valopt)
             do Bi =1, 2
                Vbad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi) = valopt
                optC_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Copt
                optA_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Aopt
                optH_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=0.0_8
                optB_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=1_8
                optW_bad_good(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=3.0_8
                optW_bad_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=3.0_8
                optAIME_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=AIMEstate(AIMEi)
                optI_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=Iopt
                optpb_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=pbopt
                optss_bad(age-bornage+1_8, AIMEi, 1_8, Ai, Bi)=ssopt
             end do

             if (PrintResults==1_8) then
                write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, &
                     & f10.2, a, i2, a, f10.2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
                     & age,',', 1.0_8, ',', Astate(Ai),',', Ai,',', AIMEstate(AIMEi),',', AIMEi,',',  3.0_8,',', 1_8,',',&
                     & Aopt,',', Copt,',', 0.0_8,',', 1_8,',', AIMEstate(AIMEi),',', Iopt,',', pbopt,',', ssopt,',', valopt
             end if

             do Bi = 1, 2
                do Wi = 2, Wnum
                   Vgood(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = Vgood(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optC_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optC_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optA_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optA_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optH_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optH_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optB_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optB_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optW_good_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optW_good_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optW_good_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optW_good_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optAIME_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optAIME_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optI_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optI_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optpb_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optpb_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optss_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optss_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)

                   Vbad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = Vbad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optC_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optC_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optA_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optA_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optH_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optH_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optB_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optB_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optW_bad_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optW_bad_good(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optW_bad_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optW_bad_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optAIME_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optAIME_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optI_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optI_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optpb_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optpb_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                   optss_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = optss_bad(age-bornage+1, AIMEi, 1_8, Ai, Bi)
                end do
             end do

          end do ! AIME loop
       end do ! A loop
    end do ! age loop

    do age = retage, eretage, -1 !age 70-60
       !call make_AIME_2(age, AIMEstate)
       !write(*,*) age
       do Wi = 1, Wnum
          !      if (Wi==7_8) then
          !         write(*,*) 'stop1'
          !      else if (Wi==8_8) then
          !         write(*,*) 'stop2'
          !      else if (Wi==9_8) then
          !         write(*,*) 'stop3'
          !      end if
          !      if (Wi==Wnum) then
          !         write(*,*) 'stop4'
          !      end if
          do Ai = 1, Asnum
             !        if (Wi==7_8) then
             !           write(*,*) Ai
             !        end if

             do AIMEi = 1, AIMEnum
                do Bi = 0, 1
                   Bidx = Bi+1_8

                   !              if (Wi==7_8 .and. Ai==8_8 .and. AIMEi==10_8) then
                   !                 write(*,*) 'optmum1 done'
                   !                 write(*,*) AIMEi, Bidx, Bi
                   !              end if

                   Cinit = optC_good(age-bornage+2, AIMEi, Wi, Ai, 1_8)
                   Hinit = optH_good(age-bornage+2, AIMEi, Wi, Ai, Bidx)+200
                   if (Hinit>Hmax) then
                      Hinit = optH_good(age-bornage+2, AIMEi, Wi, Ai, 1_8)
                   end if

               call optmum1(age, Astate(Ai), AIMEstate(AIMEi), Bi, Wstate(Wi), 0.0_8, Cinit, Hinit, Vgood, Vbad,&
               & Astate, AIMEstate, Wstate, Cstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
               & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
               & AIMErate, AIMEstd, low_brackets, up_brackets, &
               & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, Copt, Hopt, Aopt, Bopt, &
               & Wopt_good, Wopt_bad, AIMEopt, Iopt, pbopt, ssopt, valopt)

                   Vgood(age-bornage+1_8, AIMEi, Wi, Ai, Bidx) = valopt
                   optC_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Copt
                   optA_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Aopt
                   optH_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Hopt
                   optB_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Bopt
                   optW_good_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Wopt_good
                   optW_good_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Wopt_bad
                   optAIME_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=AIMEopt
                   optI_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Iopt
                   optpb_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=pbopt
                   optss_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=ssopt

                   if (PrintResults==1_8) then
                      write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, &
                           & f10.2, a, f10.2, a, i2, a, f10.2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
                           & age,',', 0.0_8,',', Astate(Ai),',', Ai,',', AIMEstate(AIMEi),',', AIMEi,',', Wstate(Wi),',',&
                           & Wi,',', Aopt,',', Copt,',', Hopt,',', Bopt,',', AIMEopt,',', Iopt,',', pbopt,',', ssopt,',', valopt
                   end if

                   Cinit = optC_bad(age-bornage+2, AIMEi, Wi, Ai, 1_8)
                   Hinit = optH_good(age-bornage+2, AIMEi, Wi, Ai, Bidx)

                   call optmum1(age, Astate(Ai), AIMEstate(AIMEi), Bi, Wstate(Wi), 1.0_8, Cinit, Hinit, Vgood, Vbad, &
                   & Astate, AIMEstate, Wstate, Cstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                   & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
                   & AIMErate, AIMEstd, low_brackets, up_brackets, &
                   & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, Copt, Hopt, Aopt, Bopt, Wopt_good, Wopt_bad, &
                   & AIMEopt, Iopt, pbopt, ssopt, valopt)
                   Vbad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx) = valopt
                   optC_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Copt
                   optA_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Aopt
                   optH_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Hopt
                   optB_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Bopt
                   optW_bad_good(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Wopt_good
                   optW_bad_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Wopt_bad
                   optAIME_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=AIMEopt
                   optI_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=Iopt
                   optpb_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=pbopt
                   optss_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bidx)=ssopt

                   if (PrintResults==1_8) then                   
                      write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a,&
                           & f10.2, a, i2, a, f10.2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
                           & age,',', 1.0_8,',', Astate(Ai),',', Ai,',', AIMEstate(AIMEi),',', AIMEi,',', Wstate(Wi),',',&
                           & Wi,',', Aopt,',', Copt,',', Hopt,',', Bopt,',', AIMEopt,',', Iopt,',', pbopt,',', ssopt,',', valopt
                   end if

                end do !End B loop
             end do!End AIME loop
          end do!End A loop
       end do !End W loop
    end do !End age loop

    do age = eretage-1, bornage, -1 !age 59-30
       !call make_AIME_2(age, AIMEstate)
       !write(*,*) age
       do Wi = 1, Wnum
          do Ai = 1, Asnum
             do AIMEi = 1, AIMEnum
                Cinit = optC_good(age-bornage+2, AIMEi, Wi, Ai, 1_8)
                Hinit = optH_good(age-bornage+2, AIMEi, Wi, Ai, 1_8)

                call optmum0(age, Astate(Ai), AIMEstate(AIMEi), Wstate(Wi), Cinit, Hinit, 0.0_8, Vgood, Vbad, Astate, &
                & AIMEstate, Wstate, Cstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
                & AIMErate, AIMEstd, low_brackets, up_brackets, &
                & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, Copt, Hopt, Aopt, Wopt_good, Wopt_bad, &
                & AIMEopt, Iopt, pbopt, ssopt, valopt)
                !write(*,*) 'Copt=', Copt
                !write(*,*) 'Aopt=', Aopt
                !write(*,*) 'Hopt=', Hopt
                do Bi = 1, 2
                   Vgood(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = valopt
                   optC_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Copt
                   optA_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Aopt
                   optH_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Hopt
                   optB_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=0_8
                   optW_good_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Wopt_good
                   optW_good_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Wopt_bad
                   optAIME_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=AIMEopt
                   optI_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Iopt
                   optpb_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=0.0_8
                   optss_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=0.0_8
                end do

                if (PrintResults==1_8) then
                   write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a,&
                        & i2, a, f10.2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
                        & age,',', 0.0_8,',', Astate(Ai),',', Ai,',', AIMEstate(AIMEi),',', AIMEi,',', Wstate(Wi),',', Wi,',', Aopt,',', &
                        & Copt,',', Hopt,',', 0_8,',', AIMEopt,',', Iopt,',', pbopt,',', ssopt,',', valopt
                end if

                Cinit = optC_bad(age-bornage+2, AIMEi, Wi, Ai, 1_8)
                Hinit = optH_bad(age-bornage+2, AIMEi, Wi, Ai, 1_8)

                call optmum0(age, Astate(Ai), AIMEstate(AIMEi), Wstate(Wi), Cinit, Hinit, 1.0_8, Vgood, Vbad, Astate,&
                & AIMEstate, Wstate, Cstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
                & AIMErate, AIMEstd, low_brackets, up_brackets, &
                & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, Copt, Hopt, Aopt, Wopt_good, Wopt_bad, &
                & AIMEopt, Iopt, pbopt, ssopt, valopt)
                do Bi = 1,2
                   Vbad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = valopt
                   optC_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Copt
                   optA_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Aopt
                   optH_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Hopt
                   optB_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=0_8
                   optW_bad_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Wopt_good
                   optW_bad_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Wopt_bad
                   optAIME_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=AIMEopt
                   optI_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=Iopt
                   optpb_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=0.0_8
                   optss_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)=0.0_8
                end do

                if (PrintResults==1_8) then
                   write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a,&
                        & i2, a, f10.2, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
                        & age,',', 1.0_8,',', Astate(Ai),',', Ai,',', AIMEstate(AIMEi),',', AIMEi,',', Wstate(Wi),',', Wi,',', Aopt,',',&
                        & Copt,',', Hopt,',', 0_8,',', AIMEopt,',', Iopt,',', pbopt,',', ssopt,',', valopt
                end if

             end do !AIME loop
          end do ! A loop
       end do !W loop
    end do !age loop

    if (PrintResults==1_8) then
       close(15)
    end if

    !Participation choice <<< This should be incorporated into optimization routines. (Masato)
    do age = bornage, dieage
       do AIMEi = 1, AIMEnum
          do Wi = 1, Wnum
             do Ai = 1, Asnum
                do Bi = 1, 2
                   if (optH_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)==0.0_8) then
                      optP_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = 0_8
                   else if (optH_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)>0) then
                      optP_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = 1_8
                   end if
                   if (optH_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)==0.0_8) then
                      optP_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = 0_8
                   else if (optH_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)>0) then
                      optP_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = 1_8
                   end if
                end do
             end do
          end do
       end do
    end do

    call itime(time_end)
    print *, 'Time spent(Min):  ', (time_end(1)*3600.0d0 + time_end(2) * 60.0d0 +time_end(3) - &
    & (time_start(1)*3600.0d0 + time_start(2) * 60.0d0 +time_start(3)))/60.0d0

  end subroutine decrule
end module mod_decrule
