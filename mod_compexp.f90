module mod_compexp

  use mod_integral
  use mod_interp
  use mod_utility

  implicit none

contains

  subroutine compexp(age, M, nextperiodassets, wtpogood, wtpobad, nextperiodAIME, &
       & Vgood, Vbad, Astate, Wstate, AIMEstate, currentB, &
       & mortality_good, mortality_bad, good_to_bad, bad_to_bad, Evtpo)

    implicit none
    
    integer(8), intent(in) :: age
    real(8), intent(in) :: M
    
    real(8), intent(in) :: nextperiodassets, wtpogood, wtpobad, nextperiodAIME
    real(8), intent(in) :: Vgood(:,:,:,:,:), Vbad(:,:,:,:,:)
    real(8), intent(in) :: Astate(:), Wstate(:), AIMEstate(:)

    integer(8), intent(in) :: currentB

    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)

    real(8), intent(out) :: Evtpo

    real(8) :: bequestutils, Evtgood, Evtbad

    bequestutils = beq(nextperiodassets, nonsep)

    if (age < retage) then
    !!Here, mod_integral used to be applied up to retage (e.g. age 70).
    !!This, however, should not be an issue because, in his age retage+1, wage doesn't matter.
       Evtgood = integral(age, nextperiodassets, wtpogood, nextperiodAIME, Vgood, Astate, Wstate, AIMEstate, currentB)
       Evtbad = integral(age, nextperiodassets, wtpobad, nextperiodAIME, Vbad, Astate, Wstate, AIMEstate, currentB)

    else if (age < dieage) then
       Evtgood = interp(age, nextperiodassets, 0.0_8, nextperiodAIME, Vgood, Astate, Wstate, AIMEstate, Asnum, Wnum, AIMEnum, currentB)
       Evtbad = interp(age, nextperiodassets, 0.0_8, nextperiodAIME, Vbad, Astate, Wstate, AIMEstate, Asnum, Wnum, AIMEnum, currentB)

    else if (age==dieage) then
       Evtpo = bequestutils
       return

    else
       write(*,*) 'age is out of the appropriate range!!'
    end if
    
    if(M == 0.0_8) then
       Evtpo = ((1.0_8-mortality_good(age-30+1))*((1.0_8-good_to_bad(age-30+1))*Evtgood &
            + good_to_bad(age-30+1)*Evtbad) + mortality_good(age-30+1)*bequestutils)
    else if (M==1.0_8) then
       Evtpo = ((1.0_8-mortality_bad(age-30+1))*((1.0_8-bad_to_bad(age-30+1))*Evtgood &
            +bad_to_bad(age-30+1)*Evtbad) + mortality_bad(age-30+1)*bequestutils)
    else
       write(*,*) 'Health is neither 0 nor 1!!'
    end if
  end subroutine compexp
end module mod_compexp

    
