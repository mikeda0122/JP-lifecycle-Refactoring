module mod_computeAfterTaxIncome

  use mod_parameter
  implicit none
  !real(8) :: spicns, spiage1, spiage2, spiage3, spiage4, spiage5, spilnw, tbk1, tbk2, tbk3, tbk4, tbk5, tbk6,&
  !           mtr1, mtr2, mtr3, mtr4, mtr5, mtr6, mtr7, ati1, ati2, ati3, ati4, ati5, ati6, ror,&
  !          mtr1a, mtr2a, mtr3a, mtr4a, mtr5a, mtr6a, mtr7a, ati1a, ati2a, ati3a, ati4a, ati5a, ati6a
!rate of return given in msm16.gau

contains

  real(8) function computeAfterTaxIncome(annual_laborincome, assets, MTR, wage, penbenefits, taxtype, age, & 
       & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments)

    implicit none

    real(8), intent(in) :: annual_laborincome
    real(8), intent(in) :: assets
    real(8), intent(inout) :: mtr
    real(8), intent(in) :: wage
    real(8), intent(in) :: penbenefits
    integer(1), intent(in) :: taxtype
    integer(8), intent(in) :: age
    real(8), intent(in) :: HI_payments(:), HI_payments_elderly(:), HI_brackets(:)
    real(8), intent(in) :: pension_brackets(:), pension_payments(:)
    
    real(8) :: laborincome, insurancepay, Pensionpay
    real(8) :: taxableY_j, taxableY_s
    real(8) :: kojyo_j, Kojyo_s
    real(8):: mtr_s(1:7) = (/ 0.05d0, 0.10d0, 0.20d0, 0.23d0, 0.33d0, 0.40d0, 0.45d0 /) !H28~ marginal tax ratio (shotokuzei)
    !real(8):: mtr_s(1:7) = (/ 0.05d0, 0.10d0, 0.20d0, 0.23d0, 0.33d0, 0.40d0, 0.40d0 /) !~H27 income tax (shotokuzei)
    real(8) :: mtr_j = 0.10d0
    real(8):: deduct_s(1:7) = (/ 0.0d0, 975.0d0, 4275.0d0, 6360.0d0, 15360.0d0, 27960.0d0, 47960.0d0 /) ! H28~
    !real(8):: deduct_s(1:12) = (/ 0.0d0, 97.50d0, 427.5d0, 636.0d0, 1536.0d0, 2796.0d0 /) !~H27 income tax deduction
    real(8):: tbk_s(1:6) = (/ 19500.0d0, 33000.0d0, 69500.0d0, 90000.0d0, 180000.0d0, 400000.0d0 /) ! H28~
    real(8):: tbk_h(49) !Health insurance payment (kokuminkenkohokenryo)
    real(8):: tbk_p(32) !Health insurance payment (kokuminkenkohokenryo)
    real(8):: HI(50)  !H28 Tokyo standard
    real(8):: HI_e(50)
    real(8):: Pen(32)
    real(8) :: incometax, residencetax
!    real(8) :: tbk_h(12), tbk_s(7), deduct_s(7), HI(12), Pen(12), HI_e(12)
    !  real(8), intent(out) :: computeaftertaxincome
    real(8) :: Y
    real(8) :: spinc
    integer :: ios, i
    character(1) :: comma

    laborincome = annual_laborincome/12.0_8
    
      ! Find spousal income
      spinc = .7202*(spicns+spiage1*age + spiage2*(age**2.0_8) + spiage3*(age**3.0_8) &
      & + spiage4*(age**4.0_8) + spiage5*(age**5.0_8) + spilnw*log(wage))

      if (spinc < 0) then
         spinc = 0 ! spousal income can't be negative
      end if

     Y = ror*assets + annual_laborincome + spinc + penbenefits

!     write(*,*) 'Y'
!     write(*,*) Y
!     read*

     insurancepay = 0.0d0
     pensionpay = 0.0d0

      if (age <40) then
        if (laborincome < tbk_h(1)) then
            insurancepay = HI(1)
        else if (laborincome >= tbk_h(49)) then
            insurancepay = HI(50)
        else
          do i = 1, 48
            if  (tbk_h(i) <= Laborincome .and. Laborincome < tbk_h(i+1))  then
               insurancepay = HI(i+1)
            Endif
          enddo
        endif
      else
        if (laborincome < tbk_h(1)) then
          insurancepay = HI_e(1)
        else if (laborincome >= tbk_h(49)) then
          insurancepay = HI_e(50)
        else
          do i = 1, 48
            if  (tbk_h(i) <= Laborincome .and. Laborincome < tbk_h(i+1))  then
              insurancepay = HI_e(i+1)
            Endif
          enddo
        endif
      endif

      if (laborincome >0 .and. laborincome < tbk_p(1)) then
        pensionpay = pen(1)                     !Check whether he belongs to kokuminnenkin or koseinenkin
      else if (laborincome >= tbk_p(32)) then
        pensionpay = pen(32)
      else
        do i = 1, 31
          if (tbk_p(i) <= laborincome .and. laborincome < tbk_p(i+1))  then
            pensionpay = pen(i+1)
          endif
        enddo
      endif


!   Deductions
    kojyo_s = 3800.0d0 ! Standard Deduction for incometax
    kojyo_j = 3300.0d0 ! Standard Deduction for residencetax
    If (spinc < 3800.0d0) then
      if  (age <70) then
        Kojyo_s = Kojyo_s + 3800.0d0 
        Kojyo_j = Kojyo_j + 3300.0d0
      else
        Kojyo_s = kojyo_s + 4800.0d0
        Kojyo_j = kojyo_j + 3800.0d0
      endif
    end if
    kojyo_s = kojyo_s + insurancepay * 12.0d0 + pensionpay * 12.0d0 
    kojyo_j = kojyo_j + insurancepay * 12.0d0 + pensionpay * 12.0d0

    taxableY_s = Y - kojyo_s
    taxableY_j = Y - kojyo_j
!    taxableY_s = Y - kojyo_s 
!    taxableY_j = Y - kojyo_j 


    if (taxableY_s <= tbk_s(1) ) then
      incometax = (taxableY_s - deduct_s(1)) * mtr_s(1)
    else if (taxableY_s > tbk_s(6)) then
      incometax = (taxableY_s - deduct_s(7)) * mtr_s(7)
    else
      do i = 1, 5
        if  (tbk_s(i) < taxableY_s .and. taxableY_s <= tbk_s(i+1))  then
          incometax = (taxableY_s - deduct_s(i+1)) * mtr_s(i+1)
        Endif
      enddo
    endif

      residencetax = (taxableY_j) * mtr_j
    
      computeaftertaxincome = Y - 12*(insurancepay + pensionpay) - incometax - residencetax

!     write(*,*) 'insurance'
!     write(*,*) insurancepay
!     read*
!     write(*,*) 'pension'
!     write(*,*) pensionpay
!     read*
!     write(*,*) 'incometax'
!     write(*,*) incometax
!     read*
!     write(*,*) 'residencetax'
!     write(*,*) residencetax
!     read*

  end function computeAfterTaxIncome

end module  mod_computeAfterTaxIncome
