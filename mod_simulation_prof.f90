module mod_simulation_prof

  use mod_parameter
  use mod_computelaborincome
  use mod_computeAfterTaxIncome
  use mod_JP_pension
  use mod_computePIA
  use mod_pension
  use mod_ass
  use mod_interp
  use mod_integral
  use mod_getadj
  use mod_makegrids

  implicit none

contains

  subroutine simulation_prof(A_dist, M_dist, W_dist, AIME_dist, mortality_good, mortality_bad, &
       & good_to_bad, bad_to_bad, HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
       & AIMErate, AIMEstd, low_brackets, up_brackets, &
       & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift,  &
       & optC_good, optC_bad, optA_good, optA_bad, optH_good, optH_bad, temp_optP_good, &
       & temp_optP_bad, temp_optB_good, temp_optB_bad, optW_good_good, optW_good_bad, &
       & optW_bad_good, optW_bad_bad, optAIME_good, optAIME_bad, optI_good, optI_bad, &
       & optpb_good, optpb_bad, optss_good, optss_bad, Astate, Wstate, AIMEstate, simdeath,&
       & simhealth, simC, simA, simH, simP, simB, simAIME)

    implicit none

    real(8), intent(in) :: A_dist(:), M_dist(:), W_dist(:), AIME_dist(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)
    real(8), intent(in) :: HI_payments(:), HI_payments_elderly(:), HI_brackets(:)
    real(8), intent(in) :: pension_brackets(:), pension_payments(:)
    real(8), intent(in) :: AIMErate(:), AIMEstd(:), low_brackets(:), up_brackets(:)
    real(8), intent(in) :: hlogwage(:), ulogwage(:), hhgr(:), hugr(:), uhgr(:), uugr(:)
    real(8), intent(in) :: gvec(:), ageshift(:)
    real(8), intent(in) :: optC_good(:,:,:,:,:), optC_bad(:,:,:,:,:)
    real(8), intent(in) :: optA_good(:,:,:,:,:),  optA_bad(:,:,:,:,:)
    real(8), intent(in) :: optH_good(:,:,:,:,:),  optH_bad(:,:,:,:,:)
    integer(8), intent(in) :: temp_optP_good(:,:,:,:,:),  temp_optP_bad(:,:,:,:,:)
    integer(8), intent(in) :: temp_optB_good(:,:,:,:,:),  temp_optB_bad(:,:,:,:,:)
    real(8), intent(in) :: optW_good_good(:,:,:,:,:),  optW_good_bad(:,:,:,:,:)
    real(8), intent(in) :: optW_bad_good(:,:,:,:,:),  optW_bad_bad(:,:,:,:,:)
    real(8), intent(in) :: optAIME_good(:,:,:,:,:),  optAIME_bad(:,:,:,:,:)
    real(8), intent(in) :: optI_good(:,:,:,:,:),  optI_bad(:,:,:,:,:)
    real(8), intent(in) :: optpb_good(:,:,:,:,:),  optpb_bad(:,:,:,:,:)
    real(8), intent(in) :: optss_good(:,:,:,:,:),  optss_bad(:,:,:,:,:)

    real(8), intent(in) :: Astate(:), Wstate(:), AIMEstate(:)

    integer(8), intent(out) :: simdeath(:,:)
    real(8), intent(out) :: simhealth(:,:)
    real(8), intent(out) :: simC(:,:)
    real(8), intent(out) :: simA(:,:)
    real(8), intent(out) :: simH(:,:)
    real(8), intent(out) :: simP(:,:)
    real(8), intent(out) :: simB(:,:)
    real(8), intent(out) :: simAIME(:,:)

    real(8) :: optP_good(dieage-bornage+1, AIMEnum, Wnum , Asnum, Bnum),  optP_bad(dieage-bornage+1, AIMEnum, Wnum , Asnum, Bnum)
    real(8) :: optB_good(dieage-bornage+1, AIMEnum, Wnum , Asnum, Bnum),  optB_bad(dieage-bornage+1, AIMEnum, Wnum , Asnum, Bnum)

    real(8) :: prof_C(dieage-bornage+1), prof_A(dieage-bornage+1)
    real(8) :: prof_H(dieage-bornage+1), prof_W(dieage-bornage+1)
    real(8) :: prof_I(dieage-bornage+1), prof_pb(dieage-bornage+1), prof_ss(dieage-bornage+1), prof_AIME(dieage-bornage+1)
    real(8) :: prof_B(dieage-bornage+1), prof_P(dieage-bornage+1)

    real(8) :: health(dieage-bornage+1)
    real(8), allocatable :: wshock_vector(:)
    integer(8) :: death_age
    integer(8) :: i, j, age, n
    integer(8) :: AIMEi, Wi, Ai, Bi

    real(8) :: ind_data(simnum,momnum*3)

    n = size(A_dist)

    allocate(wshock_vector(n*(dieage-bornage+1_8)))
    open(unit=47, file='wage_shock.csv')
    do i = 1, n*(dieage-bornage+1_8)
       read(47,*) wshock_vector(i)
    end do
    close(47)
!    do age = bornage, dieage
!       do AIMEi = 1, AIMEnum
!          do Wi = 1, Wnum
!             do Ai = 1, Asnum
!                do Bi = 1, 2
!                   if (temp_optP_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)==0_8) then
!                      optP_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = 0.0_8
!                   else if (temp_optP_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)==1_8) then
!                      optP_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = 1.0_8
!                   end if
!                   if (temp_optP_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)==0_8) then
!                      optP_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = 0.0_8
!                   else if (temp_optP_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)==1_8) then
!                      optP_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = 1.0_8
!                   end if
!
!                   if (temp_optB_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)==0_8) then
!                      optB_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = 0.0_8
!                   else if (temp_optB_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi)==1_8) then
!                      optB_good(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = 1.0_8
!                   end if
!                   if (temp_optB_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)==0_8) then
!                      optB_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = 0.0_8
!                   else if (temp_optB_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi)==1_8) then
!                      optB_bad(age-bornage+1_8, AIMEi, Wi, Ai, Bi) = 1.0_8
!                   end if
!                end do
!             end do
!          end do
!       end do
!    end do

    optP_good = real(temp_optP_good)
    optP_bad = real(temp_optP_bad)
    optB_good = real(temp_optB_good)
    optB_bad = real(temp_optB_bad)

    if (PrintResults==1_8) then
       open(unit=54, file='simulated_prof_ind.csv')
       write(54, "(A)") "id, age, death, M, C, A, H, B, I, pb, ss"
       open(unit=55, file='wsim.csv')
       write(55, "(A)") "id, age, wage"
       open(unit=56, file='ind_sim_data.csv')
       open(unit=57, file='healsim.csv')
       open(unit=58, file='deathsim.csv')
    end if

    do i = 1, n


       call trac_lifecycle(A_dist(i), M_dist(i), W_dist(i), AIME_dist(i), i, n, mortality_good,&
       & mortality_bad, good_to_bad, bad_to_bad, &
       & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
       & AIMErate, AIMEstd, low_brackets, up_brackets, &
       & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, &
       & ageshift, optC_good, optC_bad, optA_good, optA_bad, optH_good, optH_bad, optP_good, optP_bad, &
       & optB_good, optB_bad, optW_good_good, optW_good_bad, optW_bad_good, optW_bad_bad, optAIME_good,&
       & optAIME_bad, optI_good, optI_bad, optpb_good, optpb_bad, optss_good, optss_bad, &
       & Astate, Wstate, AIMEstate, wshock_vector, death_age, health, prof_C, prof_A, prof_H, &
       & prof_P, prof_B, prof_W, prof_I, prof_pb, prof_ss, prof_AIME)

       do age = 1, dieage-bornage+1
          if (age+bornage-1<death_age) then
             simdeath(i, age) = 0_8
          else
             simdeath(i, age) = 1_8
          end if
          simhealth(i, age) = health(age)
          simC(i, age) = prof_C(age)
          simA(i, age) = prof_A(age)
          simH(i, age) = prof_H(age)
          simP(i, age) = prof_P(age)
          simB(i, age) = prof_B(age)
          simAIME(i, age) = prof_AIME(age)

          if (PrintResults==1_8) then
             write(54,'(i5, a, i2, a, i2, a, f4.2, a, f18.5, a, f18.5, a, f18.5, a, f4.2, a, f18.5, a,&
                  & f18.5, a, f18.5, a, f18.5)') &
                  i, ',', age+bornage-1, ',', simdeath(i, age), ',', health(age), ',', prof_C(age), ',',&
                  &  prof_A(age), ',', prof_H(age), ',', prof_B(age) &
                  & , ',', prof_I(age), ',', prof_pb(age), ',', prof_ss(age), ',', prof_AIME(age)

             write(55,'(i5, a, i2, a, f18.5)') i, ',', age+bornage-1, ',', prof_W(age)
          end if

       end do
    end do

    ind_data(:,1:momnum) = simH(:,1:momnum)
    ind_data(:,momnum+1:momnum*2) = simP(:,1:momnum)
    ind_data(:,momnum*2+1:momnum*3) = simA(:,1:momnum)/1000000.0_8

    if (PrintResults==1_8) then
       do i = 1, simnum
          write(56,*) ind_data(i,1:momnum*3)
          write(57,*) simhealth(i,1:momnum)
          write(58,*) simdeath(i,1:momnum) 
       end do

       close(54)
       close(55)
       close(56)
       close(57)
       close(58)
    end if

  end subroutine simulation_prof

  subroutine trac_lifecycle(A0, M0, W0, AIME0, id, numind, mortality_good, mortality_bad, &
    & good_to_bad, bad_to_bad, &
    & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
    & AIMErate, AIMEstd, low_brackets, up_brackets, &
    & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, &
    & optC_good, optC_bad, optA_good, optA_bad, optH_good, optH_bad, optP_good, optP_bad, &
    & optB_good, optB_bad, optW_good_good, optW_good_bad, optW_bad_good, optW_bad_bad, &
    & optAIME_good, optAIME_bad, optI_good, optI_bad, optpb_good, optpb_bad, optss_good, optss_bad, &
    & Astate, Wstate, AIMEstate, wshock_vector, death_age, health_out, prof_C, prof_A, prof_H, &
    & prof_P, prof_B, prof_W, prof_I, prof_pb, prof_ss, prof_AIME)

    implicit none

    real(8), intent(in) :: A0, M0, W0, AIME0
    integer(8), intent(in) :: id, numind
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)
    real(8), intent(in) :: HI_payments(:), HI_payments_elderly(:), HI_brackets(:)
    real(8), intent(in) :: pension_brackets(:), pension_payments(:)
    real(8), intent(in) :: AIMErate(:), AIMEstd(:), low_brackets(:), up_brackets(:)
    real(8), intent(in) :: hlogwage(:), ulogwage(:), hhgr(:), hugr(:), uhgr(:), uugr(:)
    real(8), intent(in) :: gvec(:), ageshift(:)
    real(8), intent(in) :: optC_good(:,:,:,:,:), optC_bad(:,:,:,:,:)
    real(8), intent(in) :: optA_good(:,:,:,:,:), optA_bad(:,:,:,:,:)
    real(8), intent(in) :: optH_good(:,:,:,:,:), optH_bad(:,:,:,:,:)
    real(8), intent(in) :: optP_good(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum), &
    & optP_bad(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum)
    real(8), intent(in) :: optB_good(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum), &
    & optB_bad(dieage-bornage+1, AIMEnum, Wnum, Asnum, Bnum)
    real(8), intent(in) :: optW_good_good(:,:,:,:,:),  optW_good_bad(:,:,:,:,:)
    real(8), intent(in) :: optW_bad_good(:,:,:,:,:),  optW_bad_bad(:,:,:,:,:)
    real(8), intent(in) :: optAIME_good(:,:,:,:,:),  optAIME_bad(:,:,:,:,:)
    real(8), intent(in) :: optI_good(:,:,:,:,:),  optI_bad(:,:,:,:,:)
    real(8), intent(in) :: optpb_good(:,:,:,:,:),  optpb_bad(:,:,:,:,:)
    real(8), intent(in) :: optss_good(:,:,:,:,:),  optss_bad(:,:,:,:,:)
    real(8), intent(in) :: Astate(:), Wstate(:), AIMEstate(:)
    real(8), intent(in) :: wshock_vector(:)

    integer(8), intent(out) :: death_age
    real(8), intent(out) :: health_out(:)
    real(8), intent(out) :: prof_C(:), prof_A(:), prof_H(:), prof_P(:), prof_W(:)
    real(8), intent(out) :: prof_B(:)
    real(8), intent(out) :: prof_I(:), prof_pb(:), prof_ss(:), prof_AIME(:)

    real(8) :: death(dieage-bornage+1), rwage(dieage-bornage+1), health(dieage-bornage+1)
    real(8) :: dwage, aftertaxincome, laborincome, PIA, pb, ss
    real(8) :: cumadj2, eretadj, bigcred, cumeretadj, lretadj
    real(8) :: penacc1, penacc2, nextpenbenpred, penbenpred, nextPIA
    real(8) :: tempnextPIA, earlyretirement, makeadjust
    real(8) :: prvA, prvW, prvAIME
    real(8) :: nextasset, nextAIME
    real(8) :: wtpogood, wtpobad
    real(8) :: reduc, MTR
    integer(8) :: prvB, apply
    integer(8) :: age, i
    integer(8) :: Aindex, Windex, AIMEindex, Bindex


    call wage_draw(wshock_vector, id, rwage)
    call health_draw(M0, good_to_bad, bad_to_bad, id, numind, health)
    call death_draw(health, mortality_good, mortality_bad, id, numind, death)

    prvA = A0
    prvW = W0
    prvAIME = AIME0
    prvB = 0_8
    apply = 0_8
    death_age = 30_8

    if (prvW<Wmin) then
       prvW = Wmin
    end if

    if (prvAIME<AIMEmin) then
       prvAIME = AIMEmin
    end if

    prof_W(1) = prvW

    do age = bornage, dieage
       if (death(age-bornage+1)==1.0_8) then !the case where this individual has been already dead.
          prvA = 0.0_8
          prvW = 0.0_8
          prvAIME = 0.0_8
          prvB = 0_8
          prof_C(age-bornage+1) = 0.0_8
          prof_A(age-bornage+1) = 0.0_8
          prof_H(age-bornage+1) = 0.0_8
          prof_B(age-bornage+1) = 0_8
          prof_I(age-bornage+1) = 0.0_8
          prof_pb(age-bornage+1) = 0.0_8
          prof_ss(age-bornage+1) = 0.0_8
          prof_AIME(age-bornage+1) = 0.0_8
          if (age/=dieage) then
             prof_W(age-bornage+1+1) = 0.0_8
          end if
       else if (death(age-bornage+1)==0.0_8) then

          Bindex = prvB + 1_8

          if(health(age-bornage+1)==0.0_8) then !Simulation for Healthy people
             !Optimal Choice (with interpolation)

             prof_C(age-bornage+1) = interp(age-1, prvA, prvW, prvAIME, optC_good, Astate, Wstate,&
             & AIMEstate, Asnum, Wnum, AIMEnum, prvB)
             prof_P(age-bornage+1) = interp(age-1, prvA, prvW, prvAIME, optP_good, Astate, Wstate,&
             & AIMEstate, Asnum, Wnum, AIMEnum, prvB)
             if (prof_P(age-bornage+1)>= 0.0_8 .and. prof_P(age-bornage+1)<0.5_8) then
                prof_H(age-bornage+1) = 0.0_8
                prof_P(age-bornage+1) = 0.0_8
             else if (prof_P(age-bornage+1) >= 0.5_8 ) then
                prof_H(age-bornage+1) = interp(age-1, prvA, prvW, prvAIME, optH_good, Astate, Wstate,&
                & AIMEstate, Asnum, Wnum, AIMEnum, prvB)
                prof_P(age-bornage+1) = 1.0_8
             else
                write(*,*) 'something is wrong with trac_lifecycle in mod_simulation_interp.f90'
                write(*,*) prof_P(age-bornage+1)
                read*
             end if

             !Optimal choice of B is the state variable for the next period.
             prof_B(age-bornage+1) = interp(age-1, prvA, prvW, prvAIME, optB_good, Astate, Wstate, &
             & AIMEstate, Asnum, Wnum, AIMEnum, prvB)
             if (prof_B(age-bornage+1)>= 0.0_8 .and. prof_B(age-bornage+1)<0.5_8) then
                prof_B(age-bornage+1) = 0.0_8
                prvB = 0_8
             else if (prof_B(age-bornage+1) >= 0.5_8) then
                prof_B(age-bornage+1) = 1.0_8
                
                if (prvB==0_8) then
                   apply = 1_8
                end if
                
                prvB = 1_8
             else
                write(*,*) 'something is wrong with trac_lifecycle in mod_simulation_interp.f90'
                write(*,*) prof_B(age-bornage+1)
                read*
             end if

             !Caluculate state variables (except prvB) for the next period.
             laborincome = computelaborincome(prvW, prof_H(age-bornage+1))
!             PIA = computePIA(prvAIME)
!             if (age<62) then
!                pb = 0.0_8
!                ss = 0.0_8
!             if (age<=dieage) then
!                pb = predictpensionbenefits(PIA, age)
!                ss = PIA
!                call getadj(age, prvB, cumadj2, eretadj, bigcred, cumeretadj, litcred)
!
!                if (prvB == 1_8 .and. age>=62 .and. age<nret) then
!                   ss = PIA *cumeretadj
!                else
!                   ss = PIA !*prvB
!                end if
!             else
!                write(*,*) 'something is wrong with age!!'
!             end if
             if (age<62) then
                pb = 0.0_8
                ss = 0.0_8
             else if (age>=62 .and. age<=dieage) then
                pb = 0.0_8 ! pb = predictpensionbenefits(PIA, age)
                call getadj(age, eretadj, lretadj)

                if (apply==1_8 .and. age<penage) then
                   prvAIME = prvAIME*(1.0_8-eretadj)
                else if (apply==1_8 .and. penage<age) then
                   prvAIME = prvAIME*(1.0_8+lretadj)
                else
                   prvAIME = prvAIME
                end if
                
                ss = prvAIME*prvB

             else
                write(*,*) 'something is wrong with age!!'
             end if

             aftertaxincome = computeAfterTaxIncome(laborincome, prvA, MTR, prvW, pb, taxtype, age, &
                  & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments)

             call computeAIME(age, prvAIME, laborincome, AIMErate, low_brackets, up_brackets, AIMEstd, nextAIME)

             call ass(age, prvB, aftertaxincome, prof_C(age-bornage+1), laborincome, prvA, ss, nextasset)

             !Adjustment due to ss application happens only for those in their 62 to 69.
!             if (age>=62_8 .and. age <=69_8) then
!                tempnextPIA = computePIA(nextAIME)
!
!                earlyretirement = 1.0_8
!                makeadjust = 0.0_8
!
!                if(prvB==1_8 .and. age<nret) then
!                   earlyretirement = earlyretirement*eretadj
!                   makeadjust = 1.0_8
!                else if (prvB==0_8 .and. age>=nret) then
!                   earlyretirement = earlyretirement*(1+litcred)
!                   makeadjust = 1.0_8
!                end if
!
!                if (prvB==1_8) then
!                   if(age<nret) then
!                      earlyretirement = earlyretirement*(1+bigcred*reduc)
!                   else if (age>=nret) then
!                      earlyretirement = earlyretirement*(1+litcred*reduc)
!                   else
!                      write(*,*) 'something wrong with earlyretirement!!'
!                   end if
!                   makeadjust = 1.0_8
!                end if
!
!                if (makeadjust==1.0_8) then
!                   call getnextPIA(tempnextPIA, earlyretirement, nextPIA)
!                   nextAIME = findAIME(nextPIA)
!                end if
!
!             end if

             !adjust next period assets based on pension accrue
!             call computepenaccrue(age, ageshift, laborincome, penacc1)
!             nextPIA = computePIA(nextAIME)
!             nextpenbenpred = predictpensionbenefits(nextPIA, penbensstart+1)
!             penbenpred = predictpensionbenefits(PIA, penbensstart+1)
!             penacc2 = nextpenbenpred - penbenpred
!             penacc2=penacc2*gvec(age + 1-bornage)
!             nextasset=nextasset+penacc1-penacc2

             !Update prvW
             call nextwage(age, prvW, 0.0_8, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, wtpogood, wtpobad)
             if (health(age-bornage+1+1)==0.0_8) then
                prvW = wtpogood*exp(stderr*rwage(age-bornage+1))
!                prvW = wtpogood
             else if (health(age-bornage+1+1)==1.0_8) then
                prvW = wtpobad*exp(stderr*rwage(age-bornage+1))
!                prvW = wtpobad
             else if (age==95_8) then
                prvW = 0
             else
                write(*,*) 'something is wrong with sim health!!'
                write(*,*) id, age, health(age-bornage+1+1)
                read*
             end if

             !Update prvA
             prvA = nextasset

             !Update prvAIME
             prvAIME = nextAIME

          else if ((health(age-bornage+1)==1.0_8)) then !Simulation for unhealthy people
             !Optimal Choice (with interpolation)
             prof_C(age-bornage+1) = interp(age-1, prvA, prvW, prvAIME, optC_bad, Astate, Wstate, &
             & AIMEstate, Asnum, Wnum, AIMEnum, prvB)

             prof_P(age-bornage+1) = interp(age-1, prvA, prvW, prvAIME, optP_bad, Astate, Wstate, &
            & AIMEstate, Asnum, Wnum, AIMEnum, prvB)
             if (prof_P(age-bornage+1)>= 0.0_8 .and. prof_P(age-bornage+1)<0.5_8) then
                prof_H(age-bornage+1) = 0.0_8
                prof_P(age-bornage+1) = 0.0_8
             else if (prof_P(age-bornage+1) >= 0.5_8) then
                prof_H(age-bornage+1) = interp(age-1, prvA, prvW, prvAIME, optH_bad, Astate, Wstate, &
            & AIMEstate, Asnum, Wnum, AIMEnum, prvB)
                prof_P(age-bornage+1) = 1.0_8
             else
                write(*,*) 'something is wrong with trac_lifecycle in mod_simulation_interp.f90'
                write(*,*) prof_P(age-bornage+1)
                read*
             end if

             !Optimal choice of B is the state variable for the next period.
             prof_B(age-bornage+1) = interp(age-1, prvA, prvW, prvAIME, optB_bad, Astate, Wstate, &
             & AIMEstate, Asnum, Wnum, AIMEnum, prvB)
             if (prof_B(age-bornage+1)>= 0.0_8 .and. prof_B(age-bornage+1)<0.5_8) then
                prof_B(age-bornage+1) = 0.0_8
                prvB = 0_8
             else if (prof_B(age-bornage+1) >= 0.5_8 ) then
                prof_B(age-bornage+1) = 1.0_8
                
                if (prvB==0_8) then
                   apply = 1_8
                end if

                prvB = 1_8
             else
                write(*,*) 'something is wrong with trac_lifecycle in mod_simulation_interp.f90'
                write(*,*) prof_B(age-bornage+1)
                read*
             end if


             !Caluculate state variables (except prvB) for the next period.
             laborincome = computelaborincome(prvW, prof_H(age-bornage+1))
             PIA = computePIA(prvAIME)
             pb = 0.0_8  !pb =  predictpensionbenefits(PIA, age)
             !             if (age<62) then
             !                pb = 0.0_8
             !                ss = 0.0_8
!              if (age<=dieage) then
!                 pb = predictpensionbenefits(PIA, age)
!                 ss = PIA
!                 call getadj(age, prvB, cumadj2, eretadj, bigcred, cumeretadj, litcred)
!
!                 if (prvB == 1_8 .and. age>=62 .and. age<nret) then
!                    ss = PIA !*cumeretadj
!                 else
!                    ss = PIA ! *prvB
!                 end if
!              else
!                 write(*,*) 'something is wrong with age!!'
!              end if
             if (age<62) then
                pb = 0.0_8
                ss = 0.0_8
             else if (age>=62 .and. age<=dieage) then
                pb = 0.0_8 !predictpensionbenefits(PIA, age)
                call getadj(age, eretadj, lretadj)

                if (apply==1_8 .and. age<penage) then
                   prvAIME = prvAIME*(1.0_8-eretadj)
                else if (apply==1_8 .and. penage<age) then
                   prvAIME = prvAIME*(1.0_8+lretadj)
                else
                   prvAIME = prvAIME
                end if
                
                ss = prvAIME*prvB

             else
                write(*,*) 'something is wrong with age!!'
             end if

             aftertaxincome = computeAfterTaxIncome(laborincome, prvA, MTR, prvW, pb, taxtype, age, &
                  & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments)

             call computeAIME(age, prvAIME, laborincome, AIMErate, low_brackets, up_brackets, AIMEstd, nextAIME)

             call ass(age, prvB, aftertaxincome, prof_C(age-bornage+1), laborincome, prvA, ss, nextasset)

             !Adjustment due to ss application happens only for those in their 62 to 69.
!             if (age>=62_8 .and. age <=69_8) then
!                tempnextPIA = computePIA(nextAIME)
!
!                earlyretirement = 1.0_8
!                makeadjust = 0.0_8
!
!                if(prvB==1_8 .and. age<nret) then
!                   earlyretirement = earlyretirement*eretadj
!                   makeadjust = 1.0_8
!                else if (prvB==0_8 .and. age>=nret) then
!                   earlyretirement = earlyretirement*(1+litcred)
!                   makeadjust = 1.0_8
!                end if
!
!                if (prvB==1_8) then
!                   if(age<nret) then
!                      earlyretirement = earlyretirement*(1+bigcred*reduc)
!                   else if (age>=nret) then
!                      earlyretirement = earlyretirement*(1+litcred*reduc)
!                   else
!                      write(*,*) 'something wrong with earlyretirement!!'
!                   end if
!                   makeadjust = 1.0_8
!                end if
!
!                if (makeadjust==1.0_8) then
!                   call getnextPIA(tempnextPIA, earlyretirement, nextPIA)
!                   nextAIME = findAIME(nextPIA)
!                end if
!             end if

             !adjust next period assets based on pension accrue
!             call computepenaccrue(age, ageshift, laborincome, penacc1)
!             nextPIA = computePIA(nextAIME)
!             nextpenbenpred = predictpensionbenefits(nextPIA, penbensstart+1)
!             penbenpred = predictpensionbenefits(PIA, penbensstart+1)
!             penacc2 = nextpenbenpred - penbenpred
!             penacc2=penacc2*gvec(age + 1-bornage)
!             nextasset=nextasset+penacc1-penacc2

             !Update prvW
             call nextwage(age, prvW, 1.0_8, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, wtpogood, wtpobad)
             if (health(age-bornage+1+1)==0.0_8) then
                prvW = wtpogood*exp(stderr*rwage(age-bornage+1))
             else if (health(age-bornage+1+1)==1.0_8) then
                prvW = wtpobad*exp(stderr*rwage(age-bornage+1))
             else if (age==95_8) then
                prvW = 0
             else
                write(*,*) 'something is wrong with sim health!!'
                write(*,*) id, age, health(age-bornage+1+1)
                read*
             end if

             !Update prvA
             prvA = nextasset

             !Update prvAIME
             prvAIME = nextAIME

          else
             write(*,*) 'Something wrong with trac_lifecycle!!'
             read*
          end if

          prof_I(age-bornage+1) = aftertaxincome
          prof_pb(age-bornage+1) = pb
          prof_ss(age-bornage+1) = ss

          prof_A(age-bornage+1) = prvA
          prof_B(age-bornage+1) = prvB
          prof_AIME(age-bornage+1) = prvAIME
          if (age/=dieage) then
             prof_W(age-bornage+1+1) = prvW
          end if

          death_age = death_age+1_8
       else
          write(*,*) 'This is not what you want! death'
          read*
       end if
    end do

    health_out = health

  end subroutine trac_lifecycle

  subroutine locate_indx(prv, state, num, indx)

    implicit none

    real(8), intent(in) :: prv
    real(8), intent(in) :: state(:)
    integer(8), intent(in) :: num
    integer(8), intent(out) :: indx

    integer(8) :: i

    if (prv < (state(1)+state(2))/2) then
       indx = 1_8
    else if ((state(num-1)+state(num))/2 <= prv) then
       indx = num
    else if ((state(1)+state(2))/2 <= prv .and. prv < (state(num-1)+state(num))/2) then
       do i = 2, num-1
          if ((state(i-1)+state(i))/2 <= prv .and. prv < (state(i)+state(i+1))/2) then
             indx = i
             exit
          end if
       end do
    else
       print*, 'This is not what you want!! indx'
       read*
    end if

  end subroutine locate_indx

  subroutine locate_AIMEindx(age, prv, num, indx)

    implicit none

    real(8), intent(in) :: prv
    integer(8), intent(in) :: num, age
    integer(8), intent(out) :: indx
    real(8) :: state(num)

    integer(8) :: i

    call make_AIME_2(age, state)

    if (prv < (state(1)+state(2))/2) then
       indx = 1_8
    else if ((state(num-1)+state(num))/2 <= prv) then
       indx = num
    else if ((state(1)+state(2))/2 <= prv .and. prv < (state(num-1)+state(num))/2) then
       do i = 2, num-1
          if ((state(i-1)+state(i))/2 <= prv .and. prv < (state(i)+state(i+1))/2) then
             indx = i
             exit
          end if
       end do
    else
       print*, 'This is not what you want!! indx'
       read*
    end if

  end subroutine locate_AIMEindx


  subroutine death_draw(health, mortality_good, mortality_bad, id, numind, death)

    implicit none

    real(8), intent(in) :: health(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:)
    integer(8), intent(in) :: id, numind
    real(8), intent(out) :: death(:)

    real(8), allocatable :: rarray(:)
    integer(8) :: i, rlength
    integer(4) :: n
    integer(4), allocatable :: seed(:)

    rlength = numind*(dieage-bornage+1_8)
    allocate(rarray(rlength))

    call random_seed(size=n)
    allocate(seed(n))

    seed = 62738_4 !6273884937_8

    call random_seed(put=seed)
    call random_number(rarray)

!    if (health(1)==0.0_8) then
!       if (rarray((id-1_8)*66_8+1)<mortality_good(1)) then
!          death(1) = 1.0_8
!       else if (rarray((id-1_8)*66_8+1) >= mortality_good(1)) then
!          death(1) = 0.0_8
!       else
!          write(*,*) 'Something is wrong with death_draw!!'
!          read*
!       end if
!    else if (health(1)==1.0_8) then
!       if (rarray((id-1_8)*66_8+1)<mortality_bad(1)) then
!          death(1) = 1.0_8
!       else if (rarray((id-1_8)*66_8+1) >= mortality_bad(1)) then
!          death(1) = 0.0_8
!       else
!          write(*,*) 'Something is wrong with death_draw!!'
!          read*
!       end if
!    else
!       write(*,*) 'Something is wrong with health_draw!!'
!       read*
!    end if

    death(1) = 0.0_8

    do i = 2, dieage-bornage+1
       if (health(i)==0.0_8) then
          if (rarray((id-1_8)*66+i)<mortality_good(i) .or. death(i-1)==1.0_8) then
             death(i) = 1.0_8
          else if (rarray((id-1_8)*66+i)>=mortality_good(i)) then
             death(i) = 0.0_8
          else
             write(*,*) 'Something is wrong with death_draw!!'
             read*
          end if
       else if (health(i)==1.0_8) then
          if (rarray((id-1_8)*66+i)<mortality_bad(i) .or. death(i-1)==1.0_8) then
             death(i) = 1.0_8
          else if (rarray((id-1_8)*66+i)>=mortality_bad(i)) then
             death(i) = 0.0_8
          else
             write(*,*) 'Something is wrong with death_draw!!'
             read*
          end if
       else
          write(*,*) 'Something is wrong with health_draw!!'
       end if
    end do

    deallocate(rarray)

  end subroutine death_draw

  subroutine health_draw(M0, good_to_bad, bad_to_bad, id, numind, health)

    implicit none

    real(8), intent(in) :: M0
    real(8), intent(in) :: good_to_bad(:), bad_to_bad(:)
    integer(8), intent(in) :: id, numind
    real(8), intent(out) :: health(:)

    real(8), allocatable :: rarray(:)
    integer(8) :: i, rlength
    integer(4) :: n
    integer(4), allocatable :: seed(:)

    rlength = numind*(dieage-bornage+1_8)
    allocate(rarray(rlength))

    call random_seed(size=n)
    allocate(seed(n))

    seed = 32586_4 !325867443345_8

    call random_seed(put=seed)
    call random_number(rarray)

    health(1) = M0

    do i = 2, dieage-bornage+1
       if (i==dieage-bornage+2) then
          if(health(i-1)==0.0_8) then
             if (rarray((id-1_8)*66+i)<good_to_bad(i)) then
                health(i) = 1.0_8
             else if (rarray((id-1_8)*66+i)>=good_to_bad(i)) then
                health(i) = 0.0_8
             else
                write(*,*) 'Something is wrong with health_draw!!'
                read*
             end if
          else if (health(i-1)==1.0_8) then
             if (rarray((id-1_8)*66+i)<bad_to_bad(i)) then
                health(i) = 1.0_8
             else if (rarray((id-1_8)*66+i)>=bad_to_bad(i)) then
                health(i) = 0.0_8
             else
                write(*,*) 'Something is wrong with health_draw!!'
                read*
             end if
          else
             write(*,*) 'Something is wrong with health_draw!!'
             read*
          end if
       elseif(health(i-1)==0.0_8) then
          if (rarray((id-1_8)*66+i)<good_to_bad(i)) then
             health(i) = 1.0_8
          else if (rarray((id-1_8)*66+i)>=good_to_bad(i)) then
             health(i) = 0.0_8
          else
             write(*,*) 'Something is wrong with health_draw!!'
             read*
          end if
       else if (health(i-1)==1.0_8) then
          if (rarray((id-1_8)*66+i)<bad_to_bad(i)) then
             health(i) = 1.0_8
          else if (rarray((id-1_8)*66+i)>=bad_to_bad(i)) then
             health(i) = 0.0_8
          else
             write(*,*) 'Something is wrong with health_draw!!'
             read*
          end if
       else
          write(*,*) 'Something is wrong with health_draw!!'
          read*
       end if
    end do

    deallocate(rarray)

  end subroutine health_draw

  subroutine wage_draw(random, id, rwage)

    implicit none

    real(8), intent(in) :: random(:)
    integer(8), intent(in) :: id
    real(8), intent(out) :: rwage(:)

    integer(8) :: i

    do i = 1, dieage-bornage+1

       rwage(i) = random((id-1_8)*66+i)

    end do

  end subroutine wage_draw


!  subroutine random_seed_clock()

!    implicit none

!    integer :: nseed, clock
!    integer, allocatable :: seed(:)

!    integer :: i

!    call random_seed(size=nseed)
!    allocate(seed(nseed))

!    call random_seed(put=seed)

!    deallocate(seed)

!  end subroutine random_seed_clock

end module mod_simulation_prof
