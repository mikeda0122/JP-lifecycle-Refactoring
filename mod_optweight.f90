module mod_optweight

  implicit none

contains
  subroutine optweight(W)

    real(8), intent(in) :: sim(:,:)
    real(8), intent(out) :: W(:,:)
    
    


