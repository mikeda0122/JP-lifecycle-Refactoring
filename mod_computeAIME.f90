!  COMPUTEAIME
! AIME means Average Indexed Monthly Income
! which is average earnings in the 35 highest earning years
module mod_computeAIME
  implicit none

! have to find out AIMEmax

integer, parameter :: AIMEage = 60
real(8),parameter :: AIMEmax = 43800.0_8
real(8),parameter :: accrue = 1.0_8 / 35.0_8

contains
  subroutine computeAIME(AIME, laborincome, age, apply, nextAIME)
  implicit none
    real(8), intent(in) :: AIME
    real(8), intent(in) :: laborincome
    integer(8), intent(in) :: age
    integer(8), intent(in) :: apply
    real(8), intent(out) :: nextAIME

    integer(8) :: i


!      allocate(AIME(n))
!      allocate(laborincome(n))
!      allocate(age(n))
!      allocate(accure(n))
!      allocate(nextAIME(n))

!   n = size(AIME)
!   do i = 1, n, 1

    if (age<AIMEage) then
        nextAIME = AIME + laborincome * accrue
        if (nextAIME>AIMEmax) then
          nextAIME = AIMEmax
        end if

!  If age > 60, income affects AIME only if causes an improvement
!  AND the worker has not applied for SS benefits

 !   if ((laborincome>AIME) && (apply==0))
    else if (laborincome >AIME) then !Sungwan: I added apply==0 part
        nextAIME = AIME + (laborincome - AIME)*accrue
        if (nextAIME > AIMEmax) then
          nextAIME = AIMEmax
        end if

    else
      nextAIME = AIME

    end if
!  end do

  end subroutine computeAIME
end module mod_computeAIME
