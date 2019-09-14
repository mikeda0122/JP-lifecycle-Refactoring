module mod_AIME_params

  use mod_parameter
  
  implicit none

contains

  subroutine readAIMEparams(AIMErate, AIMEbrackets, AIMEstd)

    implicit none
    
    real(8), intent(out) ::  AIMErate(:), AIMEbrackets(:), AIMEstd(:)

    integer(8) :: ios, i, n

    open(unit = 40, iostat = ios, file = 'pension_incomebracket.csv', &
         & action = 'read', form = 'formatted', status = 'old')
    if (ios/=0) then
       write(*,*) 'Failed to open!!'
       stop
    end if
    do i = 1, 30
       read(40, *) AIMEbrackets(i)
    end do

    open(unit = 41, iostat = ios, file = 'pension_standard.csv', &
         & action = 'read', form = 'formatted', status = 'old')
    if (ios/=0) then
       write(*,*) 'Failed to open!!'
       stop
    end if
    do i = 1, 31
       read(41, *) AIMEstd(i)
    end do

    open(unit = 42, iostat = ios, file = 'pension_adjrate.csv', &
         & action = 'read', form = 'formatted', status = 'old')
    if (ios/=0) then
       write(*,*) 'Failed to open!!'
       stop
    end if
    do i = 1, 52
       read(42, *) AIMErate(i)
    end do

  end subroutine readAIMEparams

  subroutine createAIMEbrackets(AIMEbrackets, low_brackets, up_brackets)

    implicit none
    
    real(8), intent(in) :: AIMEbrackets(:)
    
    real(8), intent(out) :: low_brackets(:), up_brackets(:)

    integer(8) :: n
    
    n = size(AIMEbrackets)
    
    low_brackets(1) = 0.0_8
    up_brackets(n+1) = AIMEmax

    low_brackets(2:n+1) = AIMEbrackets
    up_brackets(1:n) = AIMEbrackets

  end subroutine createAIMEbrackets
  
end module mod_AIME_params
