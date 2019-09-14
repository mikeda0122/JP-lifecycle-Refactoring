module mod_optmum3_gsearch

  use mod_parameter
  use mod_makegrids
  use mod_computeAfterTaxIncome
  use mod_computePIA
  use mod_ass
  use mod_pension
  use mod_utility
  use mod_compexp
  
  implicit none

contains

  subroutine optmum3_gsearch(age, A, AIME, Astate, AIMEstate, Cstate, &
       & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
       & M, Copt, Aopt, Iopt, pbopt, ssopt, valopt)

    implicit none


    integer(8), intent(in) :: age
    real(8), intent(in) :: A, AIME
    real(8), intent(in) :: Astate(:), AIMEstate(:), Cstate(:)
    real(8), intent(in) :: HI_payments(:), HI_payments_elderly(:), HI_brackets(:)
    real(8), intent(in) :: pension_brackets(:), pension_payments(:)
    real(8), intent(in) :: M

    real(8), intent(out) :: valopt, Copt, Aopt, Iopt, pbopt, ssopt

    integer(1) :: flag
    integer(8) :: Ci, i

!    real(8) :: Cstate(Cnum), C, Cmin, Cmax
    real(8) :: C, Cmin, Cmax
    real(8) :: PIA, ss, pb, laborincome, income, cashonhand
    real(8) :: nextperiodassets, utils, bequestutils
    real(8) :: MTR, reduc
    real(8) :: Evtpo, val

    valopt = -10000000000.0_8

    ss = AIME
!    pb = predictpensionbenefits(PIA, age)
!    pb = pb*2
    pb = 0.0_8
    laborincome = 0.0_8

    income = computeaftertaxincome(laborincome, A, MTR, 3.0_8, pb, taxtype, age, HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments)

    cashonhand = ss + income + A
    flag = 0_1

    Cmin = cfloor
    Cmax = cashonhand

    do Ci = 1, Cnum
      if (Cstate(Ci)>cashonhand) exit

       C = Cstate(Ci)

       if (cashonhand - Astate(1) < cfloor) then
          C = cfloor
       end if

       call ass(age, 1_8, income, C, laborincome, A, ss, nextperiodassets)

       utils = U(C, 3.0_8, 0_1, M, nonsep)

       call compexp(age, M, nextperiodassets, 0.0_8, 0.0_8, 0.0_8, &
       & 0.0_8, 0.0_8, Astate, Wstate, AIMEstate, 0.0_8, &
       & 0.0_8, 0.0_8, 0.0_8, 0.0_8, Evtpo)
       !!We need to make sure whether the way I dealt with Vgood and Vbad is correct.

       val = utils + p_beta*Evtpo

       if (val > valopt) then
          Copt = C
          Aopt = nextperiodassets
          Iopt = income
          pbopt = pb
          ssopt = ss
          valopt = val
       else if (val < valopt) then
          exit
       end if

    end do !End Ci loop

  end subroutine optmum3_gsearch
end module mod_optmum3_gsearch
