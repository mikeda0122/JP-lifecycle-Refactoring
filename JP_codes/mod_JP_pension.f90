module mod_JP_pension

  use mod_parameter

  implicit none

contains

  !*******************Note***************************************
  !
  !Although this is called AIME, it's just an accumulation of laborincome.
  !Thus, we need to adjust the range of AIME grids.
  !
  !
  !
  
  subroutine computeAIME(age, AIME, laborincome, AIMErate, low_brackets, up_brackets, AIMEstd, nextAIME)

    implicit none

    integer(8), intent(in) :: age
    real(8), intent(in) :: AIME
    real(8), intent(in) :: laborincome
    real(8), intent(in) :: AIMErate(:)
    real(8), intent(in) :: AIMEbrackets(:)
    real(8), intent(in) :: AIMEstd(:)
    real(8), intent(out) :: nextAIME

    real(8) :: adjrate

    real(8) :: b_laborincome
    
    adjrate = AIMErate(age-bornage+1)

    call computeAIMEbrackets(laborincome, low_brackets, up_brackets, AIMEstd, b_laborincome)
    
    if (age<=AIMEage) then
       nextAIME = AIME + b_laborincome * adjrate

       if (nextAIME>AIMEmax) then
          nextAIME = AIMEmax
       end if
    else
       nextAIME = AIME
    end if

  end subroutine computeAIME

  subroutine computeAIMEbrackets(laborincome, low_brackets, up_brackets, AIMEstd, b_laborincome)

    implicit none

    real(8), intent(in) :: laborincome
    real(8), intent(in) :: AIMErate(:)
    real(8), intent(in) :: low_brackets(:), up_brackets(:)
    real(8), intent(in) :: AIMEstd(:)
    real(8), intent(out) :: b_laborincome

    real(8), allocatable :: AIMEvec(1,:), AIMEloc(1,:)
    real(8) :: n

    real(8) :: laborincome_mat(1,1)

    !***Laborincome is not directly utilized. Instead, the values in the AIME brackets are added.
    n = size(AIMEbrackets)
    allocate(AIMEvec(1,n+1))
    allocate(AIMEloc(1,n+1))
    allocate(low_brackets(n+1))
    allocate(up_brackets(n+1))

    AIMEvec(1,:) = laborincome
    AIMEloc(:,1) = 0.0_8
    low_brackets(1) = 0.0_8
    up_brackets(n+1) = AIMEmax
    low_brackets(2:n+1) = AIMEbrackets
    up_brackets(1:n) = AIMEbrackets

    where (low_brackets(:)<=AIMEvec(1,:) .and. AIMEvec(1,:)<=up_brackets(:))
       AIMEloc(:,1) = 1.0_8
    end where

    AIMEvec(1,:) = AIMEstd

    laborincome_mat = matmul(AIMEstd, AIMEloc)

    b_laborincome = 12.0_8 * laborincome_mat(1,1)

  end subroutine computeAIMEbrackets
  
  subroutine computepension(age, B, AIME, laborincome, low_brackets, up_brackets, AIMEstd, pension)

    implicit none

    integer(8), intent(in) :: age, B
    real(8), intent(in) :: AIME, laborincome, low_brackets(:), up_brackets(:), AIMEstd(:)
    real(8), intent(out) :: pension

    real(8) :: b_laborincome

    if (age>etstage-1) then
       taxfrac = 0.0_8
       earnlev = 99999999.0_8
    else if (age>eret-1 .and. age<etstage .and. etest==1_8) then
       taxfrac = 0.5_8
       earnlev = 4700 !!470,000 yen
    else
       taxfrac = 0.0_8
       earnlev = 99999999.0_8
    end if
    
    call computeAIMEbrackets(laborincome, low_brackets, up_brackets, AIMEstd, b_laborincome)
    
    if (B==0_8) then
       pension = 0_8
    else if (laborincome < earnlev .and. B==1_8) then
       pension = const_pen + AIME * pen_rate
    else if (laborincome >= earnlev .and. B==1_8) then
       pension = const_pen + (AIME - (AIME + b_laborincome - earnlev)*taxfrac)*pen_rate
    else
       write(*,*) 'application status is not defined!!!'
    end if

  end subroutine computepension
  
end module mod_JP_pension
