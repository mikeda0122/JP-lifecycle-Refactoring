module mod_AIME_params

  use mod_parameter
  
  implicit none

contains

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
