module mod_computeval1

  use mod_parameter
  use mod_ass
  use mod_pension
  use mod_JP_pension
  use mod_computePIA
  use mod_utility
  use mod_integral
  use mod_compexp
  
  implicit none

contains

  subroutine computeval1(age, A, W, AIME, M, C, H, particip, currentB, tempnextPIA,&
    & income, ss, laborincome, PIA, Astate, Wstate, AIMEstate, hlogwage, ulogwage, &
    & hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, &
    & good_to_bad, bad_to_bad, &
    & AIMErate, AIMEstd, low_brackets, up_brackets, &
    & eretadj, bigcred, cumeretadj, litcred, Vgood, Vbad, &
    & nextperiodassets, nextperiodAIME, wtpogood, wtpobad, val)

    integer(8), intent(in) :: age
    real(8), intent(in) :: A, W, AIME
    real(8), intent(in) :: M

    real(8), intent(in) :: C, H

    integer(1), intent(in) :: particip
    integer(8), intent(in) :: currentB

    real(8), intent(in) :: tempnextPIA
    real(8), intent(in) :: income, ss, laborincome, PIA

    real(8), intent(in) :: Astate(:), Wstate(:), AIMEstate(:)
    real(8), intent(in) :: hlogwage(:), ulogwage(:), hhgr(:), hugr(:), uhgr(:), uugr(:)
    real(8), intent(in) :: ageshift(:), gvec(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)
    real(8), intent(in) :: AIMErate(:), AIMEstd(:), low_brackets(:), up_brackets(:)
    real(8), intent(in) :: eretadj, bigcred, cumeretadj, litcred
    real(8), intent(in) :: Vgood(:,:,:,:,:), Vbad(:,:,:,:,:)

    real(8), intent(out) :: nextperiodassets, nextperiodAIME
    real(8), intent(out) :: wtpogood, wtpobad
    real(8), intent(out) :: val

    real(8) :: cashonhand
    integer(1) :: flag

    real(8) :: reduc
    real(8) :: earlyretirement, makeadjust
    real(8) :: penacc1, penacc2, nextpenbenpred, penbenpred, nextPIA

    real(8) :: utils
    real(8) :: Evtpo

    cashonhand = ss + income + A
    flag = 0_1

    if (C>cashonhand) then
       flag = 1_1
    end if

    call ass(age, currentB, income, C, laborincome, A, ss, nextperiodassets)

    earlyretirement = 1.0_8
    makeadjust = 0.0_8

    if (currentB==1_8 .and. age<nret) then
       earlyretirement = earlyretirement*eretadj
       !                earlyretirement = earlyretirement*cumeretadj
       makeadjust = 1.0_8
    else if (currentB==0_8 .and. age>=nret) then
       earlyretirement = earlyretirement*(1+litcred)
       makeadjust = 1.0_8
    end if

    if (currentB==1_8) then
       if(age<nret) then
          earlyretirement = earlyretirement*(1+bigcred*reduc)
          !                   earlyretirement = earlyretirement*(1+bigcred)
       else if (age>=nret) then
          earlyretirement = earlyretirement*(1+litcred*reduc)
          !                   earlyretirement = earlyretirement*(1+litcred)
       else
          write(*,*) 'something wrong with earlyretirement!!'
       end if
       makeadjust = 1.0_8
    end if

    call computeAIME(age, AIME, laborincome, AIMErate, low_brackets, up_brackets, AIMEstd, nextperiodAIME)

    if (makeadjust==1.0_8) then
       nextperiodAIME = nextperiodAIME*earlyretirement
    end if

    utils = U(C, H, particip, M, nonsep)

    call nextwage(age, W, M, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, wtpogood, wtpobad)

    call compexp(age, M, nextperiodassets, wtpogood, wtpobad, nextperiodAIME, &
         & Vgood, Vbad, Astate, Wstate, AIMEstate, currentB, &
         & mortality_good, mortality_bad, good_to_bad, bad_to_bad, Evtpo)
    
    val = utils + p_beta*Evtpo

    if (flag==1_1) then
       val = vpanish
    else if (val > vpanish) then
       return
    else if (utils>-vpanish) then
       write(*,*) 'something other than utils went wrong!!!!'
       write(*,*) 'val', val
       write(*,*) 'utils', utils, 'Evtpo', Evtpo
       val = vpanish
       write(*,*) 'asset', nextperiodassets, 'C', C
       write(*,*) 'H', H, 'particip', particip
       write(*,*) 'M', M, 'nonsep', nonsep
       write(*,*) 'You are not supposed to be here!!'
       !read*
    else
       !For those who works more than their time endowment.
       !write(*,*) 'val', val
       !write(*,*) 'utils', utils, 'Evtpo', Evtpo
       val = vpanish
       !write(*,*) 'asset', nextperiodassets, 'C', C
       !write(*,*) 'H', H, 'particip', particip
       !write(*,*) 'M', M, 'nonsep', nonsep
       !write(*,*) 'You are not supposed to be here!!'
    end if

  end subroutine computeval1
end module mod_computeval1
