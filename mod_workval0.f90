module mod_workval0

  use mod_parameter
  use mod_makegrids
  use mod_computelaborincome
  use mod_computeAfterTaxIncome
  use mod_JP_pension
  use mod_computePIA
  use mod_computeval0
  use mod_ass
  use mod_pension
  use mod_utility
  use mod_interp
  use mod_integral
  use mod_sprob

  implicit none

contains

  subroutine workval0(age, A, AIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate,&
    & inCstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
    & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
    & AIMErate, AIMEstd, low_brackets, up_brackets, &
    & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, H, currentB, PIA,&
    & pb, ss, particip, temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad,&
    & temp_AIMEopt, temp_Iopt, temp_pbopt, temp_ssopt, temp_valopt)

    integer(8), intent(in) :: age
    real(8), intent(in) :: A, AIME, W
    real(8), intent(in) :: Cinit
    real(8), intent(in) :: M
    real(8), intent(in) :: Vgood(:,:,:,:,:), Vbad(:,:,:,:,:)
    real(8), intent(in) :: Astate(:) , AIMEstate(:), Wstate(:), inCstate(:), Hstate(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)
    real(8), intent(in) :: HI_payments(:), HI_payments_elderly(:), HI_brackets(:)
    real(8), intent(in) :: pension_brackets(:), pension_payments(:)
    real(8), intent(in) :: AIMErate(:), AIMEstd(:), low_brackets(:), up_brackets(:)
    real(8), intent(in) :: hlogwage(:), ulogwage(:), hhgr(:), hugr(:), uhgr(:), uugr(:)
    real(8), intent(in) :: gvec(:), ageshift(:)
    real(8), intent(in) :: H
    integer(8), intent(in) :: currentB
    real(8), intent(in) :: PIA, pb, ss
    integer(1), intent(in) :: particip

    real(8), intent(out) :: temp_valopt, temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, &
    & temp_AIMEopt, temp_Iopt, temp_pbopt, temp_ssopt

    integer(1) :: flag
    integer(8) :: Ci, initCi, Clen
    integer(8) :: Hi, i, Wi

    real(8) ::  C, Cmin, Cmax
    real(8), allocatable :: Cstate(:), p_Cstate(:), m_Cstate(:)
    real(8) :: laborincome, income, cashonhand
    real(8) :: penacc1, penacc2, nextpenbenpred, penbenpred, nextPIA
    real(8) :: MTR, reduc
    real(8) :: nextperiodassets, nextperiodAIME, utils, bequestutils, borrowamount
    real(8) :: wtpogood, wtpobad
    real(8) :: Evtgood, Evtbad, Evtpo
    real(8) :: utilsp, utilspp, utilsm, utilsmm
    real(8) :: Evtpop, Evtpopp, Evtpom, Evtpomm
    real(8) :: valp, valpp, valm, valmm, val

    real(8) :: temp_Cinit

    integer(8) :: fixC = 0_8

    temp_valopt = vpanish

    !!The value of utils is NaN in this case
    if (nonsep==1_1) then
       if (p_leispref - H - ((p_fixcost*particip) + (p_leisprefbad*M))<=0) then
          return
       end if
    else
       if (5280.0_8 - H - ((p_fixcost*particip) + (p_leisprefbad*M))<=0) then
          return
       end if
    end if

    laborincome = computelaborincome(W, H)

    income = computeaftertaxincome(laborincome, A, MTR, W, pb, taxtype, age, &
         & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments)

    call computeAIME(age, AIME, laborincome, AIMErate, low_brackets, up_brackets, AIMEstd, nextperiodAIME)

    borrowamount = 0.0_8
    if(liquid==0_1) then
       borrowamount = 0.6*nextperiodAIME*gvec(age-bornage+1)
    endif
    cashonhand = ss + income + A !+ borrowamount

    if (fixC==0_8) then
       allocate(Cstate(Cnum2))
       allocate(p_Cstate(Cnum2))
       allocate(m_Cstate(Cnum2))
       Clen = Cnum2

       Cmin = cfloor
       Cmax = cashonhand

       if (Cmin>Cmax) then
          Cmax=Cmin
       end if

       if (Cinit>Cmax) then
          temp_Cinit=Cmax
       else if (Cinit<Cmin) then
          temp_Cinit = Cmin
       else
          temp_Cinit=Cinit
       end if

       do i = 1, Cnum2
          p_Cstate(i) = temp_Cinit + (i-1)*(Cmax-temp_Cinit)/(Cnum2-1)
       end do

       do i = 1, Cnum2
          m_Cstate(i) = temp_Cinit + (i-1)*(Cmin-temp_Cinit)/(Cnum2-1)
       end do

       call computeval0(age, A, W, M, temp_Cinit, H, particip, currentB, nextperiodAIME, income, ss,&
            laborincome, PIA, Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, &
            ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
            AIMErate, AIMEstd, low_brackets, up_brackets, &
            Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utils, Evtpo, val)
       call computeval0(age, A, W, M, p_Cstate(2), H, particip, currentB, nextperiodAIME, income, ss,&
            laborincome, PIA, Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, &
            ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
            AIMErate, AIMEstd, low_brackets, up_brackets, &
            Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utils, Evtpo, valp)
       !call computeval0(age, A, W, M, p_Cstate(3), H, particip, currentB, nextperiodAIME, income, ss, laborincome, PIA, &
       !     Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utils, Evtpo, valpp)
       call computeval0(age, A, W, M, m_Cstate(2), H, particip, currentB, nextperiodAIME, income, ss,&
            laborincome, PIA, Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, &
            ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
            AIMErate, AIMEstd, low_brackets, up_brackets, &
            Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utils, Evtpo, valm)
       !call computeval0(age, A, W, M, m_Cstate(3), H, particip, currentB, nextperiodAIME, income, ss, laborincome, PIA, &
       !     Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utils, Evtpo, valm)

       if (valp>val) then
          Cstate = p_Cstate
       else if (valm>val) then
          Cstate = m_Cstate
       else if (val>=valm .and. val>=valp) then
          Cstate = p_Cstate
       else
          write(*,*) 'Value function may not be concave for C grids!!'
       end if
    else if (fixC==1_8) then

       if (Cinit<inCstate(2)) then
          initCi = 1
          Clen = size(inCstate)
          allocate(Cstate(Clen))
          Cstate = inCstate
       else if (Cinit>=inCstate(Cnum-1)) then
          initCi = Cnum
          Clen = size(inCstate)
          allocate(Cstate(Clen))
          do i = Cnum, 1, -1
             Cstate(Cnum-i+1) = inCstate(i)
          end do
       else if (inCstate(2)<=Cinit .and. Cinit<inCstate(Cnum-1)) then
          do i = 2, Cnum-1
             if (inCstate(i)<=Cinit .and. Cinit<inCstate(i+1)) then
                initCi = i
                exit
             end if
          end do

          call computeval0(age, A, W, M, inCstate(initCi+1), H, particip, currentB, nextperiodAIME, income, ss,&
          & laborincome, PIA, Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec,&
          & mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
          & AIMErate, AIMEstd, low_brackets, up_brackets, &
          & Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utils, Evtpo, valp)
          if (initCi<=Cnum-2) then
             call computeval0(age, A, W, M, inCstate(initCi+2), H, particip, currentB, nextperiodAIME, income, ss,&
              & laborincome, PIA, Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, &
              & mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
              & AIMErate, AIMEstd, low_brackets, up_brackets, &
              & Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, &
              & utils, Evtpo, valpp)
          else if (initCi==Cnum-1) then
             valpp=vpanish
          else
             write(*,*) 'something is wrong with initCi!!'
          end if

          call computeval0(age, A, W, M, inCstate(initCi), H, particip, currentB, nextperiodAIME, income, ss,&
          & laborincome, PIA, Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, &
          & mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
          & AIMErate, AIMEstd, low_brackets, up_brackets, &
          &Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, &
          & utils, Evtpo, valm)
          call computeval0(age, A, W, M, inCstate(initCi-1), H, particip, currentB, nextperiodAIME, income, ss,&
          &  laborincome, PIA, Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift, gvec, &
          & mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
          & AIMErate, AIMEstd, low_brackets, up_brackets, &
          & Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, &
          & utils, Evtpo, valmm)
          allocate(p_Cstate(Cnum-initCi))
          allocate(m_Cstate(initCi))

          do i = initCi+1, Cnum
             p_Cstate(i-initCi) = inCstate(i)
          end do

          do i = 1, initCi
             m_Cstate(i) = inCstate(initCi-i+1)
          end do

          if (valmm>=valm .or. valm<vpanish/100 .or. valp<vpanish/100 .or. (valpp<vpanish/100 .and. valm>=valp))then
             Clen = size(m_Cstate)
             allocate(Cstate(Clen))
             Cstate = m_Cstate
          else if (valpp>=valp) then
             Clen = size(p_Cstate)
             allocate(Cstate(Clen))
             Cstate = p_Cstate
          else if (valp>=valm .or. (valpp<vpanish/100 .and. valm<valp)) then
             val = valp
             Clen = 1_8
             allocate(Cstate(Clen))
             Cstate(1) = inCstate(initCi+1)
          else if (valm>valp) then
             val = valm
             Clen = 1_8
             allocate(Cstate(Clen))
             Cstate(1) = inCstate(initCi-1)
          else
             write(*,*) 'Something is wrong with optmum1!!!'
             write(*,*) 'valpp=', valpp
             write(*,*) 'valp=', valp
             write(*,*) 'valm=', valm
             write(*,*) 'valmm=', valmm
             read*
          end if
       end if
    else
       write(*,*) 'fixC is not defined!!'
    end if

    do Ci = 1, Clen

       if (Cstate(Ci)>cashonhand) then
          cycle
       end if

       C = Cstate(Ci)

       if (cashonhand - Astate(1) < cfloor) then
          C = cfloor
          nextperiodassets = Astate(1)
       end if

       call computeval0(age, A, W, M, C, H, particip, currentB, nextperiodAIME, income, ss,&
       & laborincome, PIA, Astate, Wstate, AIMEstate, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, ageshift,&
       & gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
       & AIMErate, AIMEstd, low_brackets, up_brackets, &
       & Vgood, Vbad, wtpogood, wtpobad, &
       & nextperiodassets, utils, Evtpo, val)

       if (val > temp_valopt) then
          temp_Copt = C
          temp_Hopt = H
          temp_Aopt = nextperiodassets
          temp_Wopt_good = wtpogood
          temp_Wopt_bad = wtpobad
          temp_AIMEopt = nextperiodAIME
          temp_Iopt = income
          temp_pbopt = pb
          temp_ssopt = ss
          temp_valopt = val
       else if (val < temp_valopt) then
          exit
       end if

    end do !End Ci loop

    if (fixC==0_8) then
       deallocate(p_Cstate)
       deallocate(m_Cstate)
       deallocate(Cstate)
    else
       if (inCstate(2)<=Cinit .and. Cinit<inCstate(Cnum-1)) then
          deallocate(p_Cstate)
          deallocate(m_Cstate)
          deallocate(Cstate)
       else if (Cinit<inCstate(2) .or. Cinit>=inCstate(Cnum-1)) then
          deallocate(Cstate)
       end if
    end if

  end subroutine workval0
end module mod_workval0