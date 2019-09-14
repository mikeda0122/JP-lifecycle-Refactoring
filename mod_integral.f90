module mod_integral

  use mod_parameter
  use mod_interp
  use mod_profwage

  implicit none

contains

!msm16.gau line 221
!stderrw  =  0.12;
!rhow     = 0.977;
!ndnumw   =      5;
!{2,3,4,5,7,10}

!algs55.src Line 60
!intmat5={2.02018287    0.01995624205,
!             0.9585724646  0.3936193231,
!             0.0           0.9453087204};
!    intmat7={2.651961356   0.000971781245,
!             1.673551628   0.05451558281,
!             0.8162878828  0.4256072526,
!             0.0           0.8102646175};
!    intmat10={3.436159118  0.000007640432855,
!              2.532731674  0.001343645746,
!              1.756683649  0.03387439445,
!              1.036610829  0.240138611,
!              0.3429013272 0.6108626337};
!nodenum==5;
!      nodes=intmat5[.,1]; wgts=intmat5[.,2]; !since it is symmetric, intmat5 actually provides 5 nodes and weights

real(8) function integral(age, nextperiodassets, W, nextperiodAIME, V, Astate, Wstate, AIMEstate, Bi)
      implicit none
      integer(8), intent(in) :: age
      real(8), intent(in) :: nextperiodassets, W, nextperiodAIME, V(:,:,:,:,:), Astate(:), Wstate(:), AIMEstate(:)
      integer(8), intent(in) :: Bi
    !  real(8), intent(out):: integral
      real(8) :: ev, wage
      integer :: i
      real(8) :: nodes(wnodenum)

    wage=0.0_8
    integral=0.0_8
    do i = 1, wnodenum
       !wage= W+sqrt(2*0.0141)*wnodes(i) !wage = (sqrt(2) * sigma * wnodes(i) + mu) ,where (mu, sigma^2) = (0, 0.0141)
       nodes(i) = exp(sqrt(2.0_8)*stderr*wnodes(i))
       wage= W*nodes(i)
       ev=interp(age, nextperiodassets, wage, nextperiodAIME, V, Astate, Wstate, AIMEstate, Asnum, Wnum, AIMEnum, Bi)
       integral=integral+ev*wwgts(i)
    enddo
    integral = integral/sqrt(pi)
    !if (integral>vpanish) then
    !   return
    !else
    !   write(*,*) 'age=', age
    !   write(*,*) 'A=', nextperiodassets
    !   write(*,*) 'W=', W
    !   write(*,*) 'AIME=', nextperiodAIME
    !   write(*,*) 'B=', Bi
    !   write(*,*) 'pi=', pi
    !   write(*,*) 'ev=', ev
    !   write(*,*) 'integral=', integral
    !   !write(*,*) 'wwgts=', wwgts
    !end if

  end function


subroutine nextwage(age, W, M, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, wtpogood, wtpobad)
      implicit none
      integer(8) :: rn
      integer(8), intent(in) :: age
      real(8), intent(in) :: W
      real(8), intent(in) :: M
      real(8), intent(in) :: hlogwage(:), ulogwage(:), hhgr(:), hugr(:), uhgr(:), uugr(:)
      real(8), intent(out) :: wtpogood
      real(8), intent(out) :: wtpobad



  rn = age - bornage + 1 ! rn = index # cn in proc rule.  See constr. of hlogwage & hhgr in makewage


  if ( M == 0.0_8 ) then
     wtpogood = exp( (rhow*log(W)) + ((1.0d0 - rhow)*hlogwage(rn))) * (exp(hhgr(rn)))
     wtpobad = exp( (rhow*log(W)) + ((1.0d0 - rhow)*hlogwage(rn))) * (exp(hugr(rn)))
     !print*, "hlogwage",hlogwage(rn-1), "log(W)", log(W)
     !print*, "hhgr",hhgr(rn-1), "hugr", hugr(rn-1)
     !print*, "wtpogood",wtpogood,"wtpobad",wtpobad
  else
     wtpogood = exp( (rhow*log(W)) + ((1.0d0 - rhow)*ulogwage(rn))) * (exp(uhgr(rn)))
     wtpobad = exp( (rhow*log(W)) + ((1.0d0 - rhow)*ulogwage(rn))) * (exp(uugr(rn)))

  end if


end subroutine

end module mod_integral
