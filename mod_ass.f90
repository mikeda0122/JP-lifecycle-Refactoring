module mod_ass
  use mod_parameter


  implicit none

contains

  subroutine ass(age, B, AfterTaxIncome, c, laborincome, A, ss, nextass)
  ! Finds next period's assets, while checking earnings test (ret30.cpp: line 243-287)
  ! Arguments:
  !    intent(in)
  !       apply: vector of integer(1).
  !       AfterTaxIncome: vector of real(8).
  !       c: vector of real(8). consumption
  !       laborincome: vector of real(8). laborincome = working hours * wage
  !       assets: vector of real(8). current asset
  !    intent(out)
  !       reduc: vector of integer(8). how much benefits are reduced
  !       ss: vector of real(8). Social Security Benefit
  !       nextass: vector of real(8). next period asset

    implicit none
    integer(8), intent(in) :: age, B
    real(8), intent(in) :: AfterTaxIncome, c, laborincome, A
    real(8), intent(in) :: ss
    real(8), intent(out) :: nextass
    real(8) :: ssearntest, redss

    nextass = A + ss + AfterTaxIncome - c

    if (nextass > Asmax) then
       nextass = Asmax
    end if

    return
  end subroutine ass

end module mod_ass
