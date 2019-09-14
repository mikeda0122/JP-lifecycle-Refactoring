module mod_policy_params

  use mod_parameter

  implicit none

contains

  subroutine readTaxparams(HI, HI_e, tbk_h, tbk_p, pen)

    implicit none

    real(8), intent(out) :: HI(:), HI_e(:), tbk_p(:), tbk_h(:), pen(:)

    integer(8) :: ios, i

    open(unit = 10, iostat = ios, file = 'ss_params/tax_brackets_h_H28.csv', action = 'read', form = 'formatted', &
         & status = 'old')
    if (ios /= 0) then
       write(*,*) 'Failed to open!'
       stop
    end if
    do i = 1,49,1
       read(10, '(f10.4)') tbk_h(i)
       !    write(*, '(i2, X, f10.8, X, f10.8)') age, good_to_bad(i), bad_to_bad(i)
    end do
    close(10)

    open(unit = 11, iostat = ios, file = 'ss_params/health_insurance_payment.csv', action = 'read', form = 'formatted', &
         & status = 'old')
    if (ios /= 0) then
       write(*,*) 'Failed to open!'
       stop
    end if
    do i = 1,50,1
       read(11, '(f10.4)') HI(i)
       !    write(*, '(i2, X, f10.8, X, f10.8)') age, good_to_bad(i), bad_to_bad(i)
    end do
    close(11)

    open(unit = 12, iostat = ios, file = 'ss_params/health_insurance_payment_elderly.csv', action = 'read', form = 'formatted', &
         & status = 'old')
    if (ios /= 0) then
       write(*,*) 'Failed to open!'
       stop
    end if
    do i = 1,50,1
       read(12, '(f10.4)') HI_e(i)
       !    write(*, '(i2, X, f10.8, X, f10.8)') age, good_to_bad(i), bad_to_bad(i)
    end do
    close(12)

    open(unit = 13, iostat = ios, file = 'ss_params/pension_payment.csv', action = 'read', form = 'formatted', &
         & status = 'old')
    if (ios /= 0) then
       write(*,*) 'Failed to open!'
       stop
    end if
    do i = 1,32,1
       read(13, *) tbk_p(i), pen(i)
       !    write(*, '(i2, X, f10.8, X, f10.8)') age, good_to_bad(i), bad_to_bad(i)
    end do
    close(13)

    !      print*, 'tax brackets'
    !      print*, tbk_h
    !      print*, 'HI'
    !      print*, HI
    !      print*, 'HI_e'
    !      print*, HI_e
    !      print*, 'tbk_p'
    !      print*, tbk_p
    !      print*, 'pen'
    !      print*, pen

    !      read*
  end subroutine readTaxparams

  subroutine readAIMEparams(AIMErate, AIMEbrackets, AIMEstd)

    implicit none
    
    real(8), intent(out) ::  AIMErate(:), AIMEbrackets(:), AIMEstd(:)

    integer(8) :: ios, i

    open(unit = 40, iostat = ios, file = 'ss_params/pension_incomebracket.csv', &
         & action = 'read', form = 'formatted', status = 'old')
    if (ios/=0) then
       write(*,*) 'Failed to open!!'
       stop
    end if
    do i = 1, 30
       read(40, *) AIMEbrackets(i)
    end do
    close(40)

    open(unit = 41, iostat = ios, file = 'ss_params/pension_standard.csv', &
         & action = 'read', form = 'formatted', status = 'old')
    if (ios/=0) then
       write(*,*) 'Failed to open!!'
       stop
    end if
    do i = 1, 31
       read(41, *) AIMEstd(i)
    end do
    close(41)

    open(unit = 42, iostat = ios, file = 'ss_params/pension_adjrate.csv', &
         & action = 'read', form = 'formatted', status = 'old')
    if (ios/=0) then
       write(*,*) 'Failed to open!!'
       stop
    end if
    do i = 1, 52
       read(42, *) AIMErate(i)
    end do
    close(42)

  end subroutine readAIMEparams
  
end module mod_policy_params
