module mod_optmum0

  use mod_parameter
  !use mod_makegrids
  !use mod_computelaborincome
  !use mod_computeAfterTaxIncome
  !use mod_computeaime
  !use mod_computePIA
  !use mod_computeval0
  !use mod_ass
  !use mod_pension
  !use mod_utility
  !use mod_interp
  !use mod_integral
  !use mod_sprob
  use mod_workval0

  implicit none

contains

  subroutine optmum0(age, A, AIME, W, Cinit, Hinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate, &
    & inCstate, inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
    & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
    & AIMErate, AIMEstd, low_brackets, up_brackets, &
    & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, Copt, Hopt, Aopt, &
    & Wopt_good, Wopt_bad, AIMEopt, Iopt, pbopt, ssopt, valopt)

    implicit none


    integer(8), intent(in) :: age
    real(8), intent(in) :: A, AIME, W
    real(8), intent(in) :: Cinit, Hinit
    real(8), intent(in) :: M
    real(8), intent(in) :: Vgood(:,:,:,:,:), Vbad(:,:,:,:,:)
    real(8), intent(in) :: Astate(:) , AIMEstate(:), Wstate(:), inCstate(:), inHstate(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)
    real(8), intent(in) :: HI_payments(:), HI_payments_elderly(:), HI_brackets(:)
    real(8), intent(in) :: pension_brackets(:), pension_payments(:)
    real(8), intent(in) :: AIMErate(:), AIMEstd(:), low_brackets(:), up_brackets(:)
    real(8), intent(in) :: hlogwage(:), ulogwage(:), hhgr(:), hugr(:), uhgr(:), uugr(:)
    real(8), intent(in) :: gvec(:), ageshift(:)

    real(8), intent(out) :: valopt, Copt, Hopt, Aopt, Wopt_good, Wopt_bad, AIMEopt, Iopt,pbopt, ssopt

    integer(1) :: flag
    integer(8) :: Hi, initHi, Hlen
    integer(8) :: lfpi, i, Wi

    real(8) ::  C, Cmin, Cmax
    real(8), allocatable :: Hstate(:), p_Hstate(:), m_Hstate(:)
    real(8) :: H
    integer(8) :: currentB
    integer(1) :: particip
    real(8) :: laborincome, income, cashonhand, pb, PIA, ss
    !real(8) :: penacc1, penacc2, nextpenbenpred, penbenpred, nextPIA
    !real(8) :: MTR, reduc
    !real(8) :: nextperiodassets, nextperiodAIME, utils, bequestutils, borrowamount
    !real(8) :: wtpogood, wtpobad
    !real(8) :: Evtgood, Evtbad, Evtpo
    real(8) :: utilsp, utilspp, utilsm, utilsmm
    real(8) :: Evtpop, Evtpopp, Evtpom, Evtpomm
    real(8) :: valp, valpp, valm, valmm, val

    real(8) :: temp_Cinit

    real(8) :: temp_valopt, temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, &
    & temp_AIMEopt, temp_Iopt, temp_pbopt, temp_ssopt
    real(8) :: lazy_valopt, lazy_Copt, lazy_Hopt, lazy_Aopt, lazy_Wopt_good, lazy_Wopt_bad, &
    & lazy_AIMEopt, lazy_Iopt, lazy_pbopt, lazy_ssopt

    integer(8) :: intval = 1_8
    real(8) :: plot_Hstate(180), plot_val(180)

    valopt = vpanish

    lazy_Copt = -1.0_8
    lazy_Hopt = 0.0_8
    lazy_Aopt = 0.0_8
    lazy_Wopt_good = 0.0_8    
    lazy_Wopt_bad = 0.0_8    
    lazy_AIMEopt = 0.0_8
    lazy_Iopt =  0.0_8
    lazy_pbopt = 0.0_8
    lazy_ssopt = 0.0_8
    lazy_valopt = vpanish

    currentB = 0_8
    PIA = computePIA(AIME)
    pb = 0.0_8 !predictpensionbenefits(PIA, age)
    ss = 0.0_8 !PIA

    do lfpi = 0, 1
       if (lfpi==0_8) then
          particip = 0_1
          H = 0.0_8

          if (nonsep==1_1) then
             if (p_leispref - H - ((p_fixcost*particip) + (p_leisprefbad*M))<=0) then
                cycle
             end if
          else
             if (5280.0_8 - H - ((p_fixcost*particip) + (p_leisprefbad*M))<=0) then
                cycle
             end if
          end if

          call workval0(age, A, AIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate, inCstate, &
          & inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
          & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
          & AIMErate, AIMEstd, low_brackets, up_brackets, &
          & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift,  H, currentB, PIA, pb, ss, particip, &
          & temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, temp_AIMEopt, &
          & temp_Iopt, temp_pbopt, temp_ssopt, temp_valopt)

          lazy_Copt = temp_Copt
          lazy_Hopt = temp_Hopt
          lazy_Aopt = temp_Aopt
          lazy_Wopt_good = temp_Wopt_good
          lazy_Wopt_bad = temp_Wopt_bad
          lazy_AIMEopt = temp_AIMEopt
          lazy_pbopt = temp_pbopt
          lazy_ssopt = temp_ssopt
          lazy_valopt = temp_valopt
       else if (lfpi==1_8) then
          particip = 1_1

          if (intval==1_8) then

             if (Hinit<inHstate(2)) then
                initHi = 1
                Hlen = size(inHstate)
                allocate(Hstate(Hlen))
                Hstate = inHstate
             else if (Hinit>=inHstate(Hnum2-1)) then
                initHi = Hnum2
                Hlen = size(inHstate)
                allocate(Hstate(Hlen))
                do i = Hnum2, 1, -1
                   Hstate(Hnum2-i+1) = inHstate(i)
                end do
             else if (inHstate(2)<=Hinit .and. Hinit<inHstate(Hnum2-1)) then
                initHi = Hnum+1_8

                do i = 2, Hnum2-1
                   if (inHstate(i)<=Hinit .and. Hinit<inHstate(i+1)) then
                      initHi = i
                      exit
                   end if
                end do

                if (initHi==Hnum+1_8) then
                   write(*,*) 'something is wrong with initHi in optmum1'
                end if

                call workval0(age, A, AIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate,&
                & inCstate,inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
                & AIMErate, AIMEstd, low_brackets, up_brackets, &
                & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, inHstate(initHi+1),&
                & currentB, PIA, pb, ss, particip, temp_Copt, temp_Hopt,  temp_Aopt,&
                & temp_Wopt_good, temp_Wopt_bad, temp_AIMEopt,temp_Iopt, temp_pbopt, temp_ssopt, valp)
                if (initHi<=Hnum2-2) then
                    call workval0(age, A, AIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate,&
                    & inCstate,inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                    & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
                    & AIMErate, AIMEstd, low_brackets, up_brackets, &
                    & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, inHstate(initHi+1),&
                    & currentB, PIA, pb, ss, particip, temp_Copt, temp_Hopt,  temp_Aopt,&
                    & temp_Wopt_good, temp_Wopt_bad, temp_AIMEopt,temp_Iopt, temp_pbopt, temp_ssopt, valpp)
                else if (initHi==Hnum2-1) then
                   valpp = vpanish
                else
                   write(*,*) 'something is wrong with initHi!!'
                end if

                call workval0(age, A, AIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate, inCstate, &
                & inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
                & AIMErate, AIMEstd, low_brackets, up_brackets, &
                & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, inHstate(initHi), currentB, PIA, pb, ss, particip, &
                & temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, temp_AIMEopt, temp_Iopt, &
                & temp_pbopt, temp_ssopt, valm)
                call workval0(age, A, AIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate, inCstate, &
                & inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
                & AIMErate, AIMEstd, low_brackets, up_brackets, &
                & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, inHstate(initHi-1), currentB, PIA, pb, ss, particip, &
                & temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, temp_AIMEopt, temp_Iopt, &
                & temp_pbopt, temp_ssopt, valmm)

                allocate(p_Hstate(Hnum2-initHi))
                allocate(m_Hstate(initHi))

                do i = initHi+1, Hnum2
                   p_Hstate(i-initHi) = inHstate(i)
                end do

                do i = 1, initHi
                   m_Hstate(i) = inHstate(initHi-i+1)
                end do

                if (valmm>=valm)then
                   Hlen = size(m_Hstate)
                   allocate(Hstate(Hlen))
                   Hstate = m_Hstate
                else if (valp>=valm) then
                   Hlen = size(p_Hstate)
                   allocate(Hstate(Hlen))
                   Hstate = p_Hstate
                else if (valm>valp .and. valm>valmm) then
                   Hlen = size(m_Hstate)
                   allocate(Hstate(Hlen))
                   Hstate = m_Hstate
!                else if (valp>=valm .or. (valpp<vpanish/100 .and. valm<valp)) then
!                   val = valp
!                   Hlen = 1_8
!                   allocate(Hstate(Hlen))
!                   Hstate(1) = inHstate(initHi+1)
!                else if (valm>valp) then
!                   val = valm
!                   Hlen = 1_8
!                   allocate(Hstate(Hlen))
!                   Hstate(1) = inHstate(initHi-1)
                else
                   write(*,*) 'Something is wrong with optmum0!!!'
                   write(*,*) 'Value function is not concave in work hours!!!'
                   write(*,*) 'valpp=', valpp
                   write(*,*) 'valp=', valp
                   write(*,*) 'valm=', valm
                   write(*,*) 'valmm=', valmm
                   allocate(Hstate(1))
                   Hstate(1) = 0.0_8
                end if
             else
                write(*,*) 'error in mod_optmum1.f90'
                allocate(Hstate(1))
                Hstate(1) = 0.0_8
             end if
          else
             initHi = Hnum
             Hlen = size(inHstate)
             allocate(Hstate(Hlen))
             Hstate = inHstate
          end if

          do Hi = 1, Hlen

             H = Hstate(Hi)

             if (nonsep==1_1) then
                if (p_leispref - H - ((p_fixcost*particip) + (p_leisprefbad*M))<=0) then
                   cycle
                end if
             else
                if (5280.0_8 - H - ((p_fixcost*particip) + (p_leisprefbad*M))<=0) then
                   cycle
                end if
             end if

             call workval0(age, A, AIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate, inCstate, &
             & inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
             & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
             & AIMErate, AIMEstd, low_brackets, up_brackets, &
             & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, H, currentB, PIA, pb, ss, particip, &
             & temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, temp_AIMEopt, temp_Iopt, &
             & temp_pbopt, temp_ssopt, temp_valopt)

             if (temp_valopt > valopt) then
                Copt = temp_Copt
                Hopt = temp_Hopt
                Aopt = temp_Aopt
                Wopt_good = temp_Wopt_good
                Wopt_bad = temp_Wopt_bad
                AIMEopt = temp_AIMEopt
                Iopt = temp_Iopt
                pbopt = temp_pbopt
                ssopt = temp_ssopt
                valopt = temp_valopt
             else if (valopt > temp_valopt) then
                exit
             end if

             !if (intval==0_8) then
             !
             !   plot_val(Hi) = temp_valopt
             !   plot_Hstate(Hi) = H
             !
             !   write(37, '(i2, a, f18.5, a, f20.10)') Hi, ',', plot_Hstate(Hi), ',', plot_val(Hi)
             !end if

          end do !End Hi loop

          close(37)
          !if (intval==0_8) then
          !   write(*,*) 'Hlen=', Hlen
          !   write(*,*) plot_val
          !   open(unit=37, file='Hval.csv')
          !   write(37, "(A)") "Hi, H, val"
          !   do Hi = 1, Hlen
          !      write(37, '(i3, a, f18.5, a, f20.10)') Hi, ',', plot_Hstate(Hi), ',', plot_val(Hi)
          !   end do
          !   close(37)
          !   read*
          !end if
       end if

    end do !End lfpi loop

    if (lazy_valopt > valopt) then
       Copt = lazy_Copt
       Hopt = lazy_Hopt
       Aopt = lazy_Aopt
       Wopt_good = lazy_Wopt_good
       Wopt_bad = lazy_Wopt_bad
       AIMEopt = lazy_AIMEopt
       pbopt = lazy_pbopt
       ssopt = lazy_ssopt
       valopt = lazy_valopt
    end if

    if (inHstate(2)<=Hinit .and. Hinit<inHstate(Hnum2-1) .and. intval==1_8) then
       deallocate(p_Hstate)
       deallocate(m_Hstate)
       deallocate(Hstate)
    else if (Hinit<inHstate(2) .or. Hinit>=inHstate(Hnum2-1) .or. intval==0_8) then
       deallocate(Hstate)
    end if

     end subroutine optmum0
   end module mod_optmum0
