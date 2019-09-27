module mod_computeval0

  use mod_parameter
  use mod_ass
  use mod_pension
  use mod_JP_pension
  use mod_utility
  use mod_integral
  use mod_compexp
  
  implicit none

contains

  subroutine computeval0(age, A, W, M, C, H, particip, currentB, nextperiodAIME, &
       & income, ss, laborincome, PIA, Astate, Wstate, AIMEstate, hlogwage, ulogwage, &
       & hhgr, hugr, uhgr, uugr, ageshift, gvec, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
       & AIMErate, AIMEstd, low_brackets, up_brackets, &
       & Vgood, Vbad, wtpogood, wtpobad, nextperiodassets, utils, Evtpo, val)

    implicit none

    integer(8), intent(in) :: age
    real(8), intent(in) :: A, W
    real(8), intent(in) :: M

    real(8), intent(in) :: C, H

    integer(1), intent(in) :: particip
    integer(8), intent(in) :: currentB

    real(8), intent(in) :: nextperiodAIME
    real(8), intent(in) :: income, ss, laborincome, PIA

    real(8), intent(in) :: Astate(:), Wstate(:), AIMEstate(:)
    real(8), intent(in) :: hlogwage(:), ulogwage(:), hhgr(:), hugr(:), uhgr(:), uugr(:)
    real(8), intent(in) :: ageshift(:), gvec(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)
    real(8), intent(in) :: AIMErate(:), AIMEstd(:), low_brackets(:), up_brackets(:)

    real(8), intent(in) :: Vgood(:,:,:,:,:), Vbad(:,:,:,:,:)

    real(8), intent(out) :: wtpogood, wtpobad
    real(8), intent(out) :: nextperiodassets
    real(8), intent(out) :: utils, Evtpo
    real(8), intent(out) :: val

    real(8) :: cashonhand
    integer(1) :: flag

    real(8) :: reduc

    real(8) :: penacc1, penacc2
    real(8) :: nextPIA, nextpenbenpred, penbenpred

    integer(8) ::checkAIME, checkW, checkA, i

    cashonhand = ss + income + A
    flag = 0_1

    if (C>cashonhand) then
       flag = 1_1
    end if

    call ass(age, currentB, income, C, laborincome, A, ss, nextperiodassets)

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
    else
       !For those who works more than their time endowment.
       write(*,*) 'mod_computeval0.f90'
       write(*,*) 'val', val
       write(*,*) 'vpanish', vpanish
       write(*,*) 'utils', utils, 'Evtpo', Evtpo
       write(*,*) p_leispref - H - ((p_fixcost*particip) + (p_leisprefbad*M))
       write(*,*) C**p_gamh
       write(*,*) p_conspref
       write(*,*) (p_leispref-p_fixcost-500.0d0)**(1.0d0-p_gamh)
       write(*,*)  p_onemgamc
       write(*,*) age
       write(*,*) '******'
       write(*,*) nextperiodassets
       write(*,*) '******'
       write(*,*) wtpobad
       write(*,*) '******'
       write(*,*) nextperiodAIME !!Astate, Wstate, AIMEstate, currentB
       val = vpanish
       write(*,*) mortality_good(age-30+1)
       write(*,*) 'asset', nextperiodassets, 'C', C
       write(*,*) 'H', H, 'particip', particip, 'currentB=', currentB
       write(*,*) 'M', M, 'nonsep', nonsep
       write(*,*) 'You are not supposed to be here!!'
!       read*
    end if

  end subroutine computeval0
end module mod_computeval0
