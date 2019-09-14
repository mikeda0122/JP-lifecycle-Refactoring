!non-vectorized version
module mod_findAIME

  implicit none
  real(8) :: mpr1, mpr2, mpr3, PIA1, PIA2, AIMEbk1, AIMEbk2, AIMEmax

!mpr1 = 0.9_8
!mpr2 = 0.32_8
!mpr3 = 0.15_8
!AIMEbk1 = 3720.0_8
!AIMEbk2 = 22392.0_8
!AIMEmax = 43800.0_8  !Eric(2003) appendix B "the maximum AIME level was $43,800 in 1987 dollars."
!PIA1 = 3348 ! AIMEbk1*mpr1
!PIA2 = 9323 !  PIA1 + (AIMEbk2-AIMEbk1)*mpr2 

contains

  
  subroutine findAIME (PIA, AIMEstar) !FINDAIME:  find the AIME associated with a given PIA
! see the working paper version (Appendix B, equation (29)) (ret30.cpp: line 672-692)
    ! Arguments:
    !   intent(in)
    !       PIA: vector of real(8). 
    !   intent(out)
    !       AIMEstar: vector of real(8). calculated AIME.
 
  implicit none
     real(8), intent(in) :: PIA
     real(8), intent(out) :: AIMEstar

  if ((PIA / mpr1) < AIMEbk1) then
     AIMEstar = PIA / mpr1
       
  else if ((AIMEbk1 + ((PIA - PIA1) / mpr2)) < AIMEbk2) then
           AIMEstar = (AIMEbk1 + (PIA - PIA1) / mpr2)
           
  
  else if ((AIMEbk2 + ((PIA(i) - PIA2) / mpr3)) < (AIMEmax)) then
          AIMEstar(i) = (AIMEbk2 + (PIA(i) - PIA2) / mpr3)
                
       
  else if ((AIMEbk2 + ((PIA - PIA2) / mpr3)) > AIMEmax) then
           AIMEstar = AIMEmax
  end if
   
    
  end subroutine findAIME

end module mod_findAIME
