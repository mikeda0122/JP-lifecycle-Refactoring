!for age30~69
module mod_interp
  use mod_parameter
  implicit none

contains

  real(8) function interp(age, A, W, AIME, V, Astate, Wstate, AIMEstate, Anum, Wnum, AIMEnum, Bi)

    implicit none
    integer(8), intent(in) :: age
    real(8), intent(in) :: A, W, AIME
    integer(8), intent(in) :: Anum, Wnum, AIMEnum, Bi
    real(8), intent(in) :: V(:,:,:,:,:)
    real(8), intent(in) :: Astate(:), Wstate(:), AIMEstate(:)
  !  real(8), intent(out) :: interp

    ! adjusted age index: age - mappage + 1
    integer(8) :: ageadj

    integer(8) :: Aidx, Widx, AIMEidx, Bidx
    integer(8) :: Ai, Wi, AIMEi
    real(8) :: Afrac, Wfrac, AIMEfrac

    Bidx = Bi + 1_8
    ! starting from age workage=30
    ageadj = age - momage + 1

    ! locate A in terms of Astate(:)
    ! how to deal with extrapolation?
    Aidx = Anum + 1_8
    if (A >= Astate(Anum)) then
        Aidx = Anum
    else if (A <= Astate(1)) then
       Aidx = 1
    else
       do Ai = 1, Anum-1
          if(A< Astate(Ai+1) .and. A >= Astate(Ai)) then
             Aidx = Ai
             exit
          end if
       end do
    end if

    if (Aidx==Anum+1_8) then
       write(*,*) 'Aidx is not defined!!'
    end if


    ! locate W in terms of Wstate(:)
    Widx = Wnum + 1_8
    if (W >= Wstate(Wnum)) then
        Widx = Wnum
    else if (W <= Wstate(1)) then
       Widx = 1
    else
       do Wi = 1, Wnum-1
          if(W< Wstate(Wi+1) .and. W >= Wstate(Wi)) then
             Widx = Wi
             exit
          end if
       end do
    end if

    if (Widx==Wnum+1_8) then
       write(*,*) 'Widx is not defined!!'
    end if

    ! locate AIME in terms of AIMEstate
    ! how to deal with extrapolation?
    AIMEidx = AIMEnum + 1_8
    if (AIME >= AIMEstate(AIMEnum)) then
        AIMEidx = AIMEnum
    else if (AIME <= AIMEstate(1)) then
       AIMEidx = 1_8
    else
       do AIMEi = 1, AIMEnum-1
          if(AIME< AIMEstate(AIMEi+1) .and. AIME >= AIMEstate(AIMEi)) then
             AIMEidx = AIMEi
             exit
          end if
       end do
    end if

    if (AIMEidx==AIMEnum+1_8) then
       write(*,*) 'AIMEidx is not defined!!'
    end if
    

    ! compute weights: Afrac, AIMEfrac, and Wfrac
    if (Aidx == Anum) then
       Afrac = 1.0d0
    else if (A < Astate(1)) then
       Afrac = 0.0d0
    else
       Afrac = (A - Astate(Aidx))/(Astate(Aidx+1) - Astate(Aidx))
    end if

    if (Widx == Wnum) then
       Wfrac = 1.0d0
    else if (W < Wstate(1)) then
       Wfrac = 0.0d0
    else
       Wfrac = (W - Wstate(Widx))/(Wstate(Widx+1) - Wstate(Widx))
    end if

    if (AIMEidx == AIMEnum) then
       AIMEfrac = 1.0d0
    else if (AIME < AIMEstate(1)) then
       AIMEfrac = 0.0d0
    else
       AIMEfrac = (AIME - AIMEstate(AIMEidx))/(AIMEstate(AIMEidx+1) - AIMEstate(AIMEidx))
    end if

    if (Afrac > 1.0_8 .or. Afrac < 0.0_8 .or. Wfrac > 1.0_8 .or. Wfrac < 0.0_8 .or. AIMEfrac > 1.0_8 .or. AIMEfrac < 0.0_8) then
       print*, 'something wrong with interpolation'
       print*, Afrac, AIMEfrac
        read*
    end if

    ! compute the weighted average
    if (Aidx == Anum) then
       if (AIMEidx == AIMEnum) then
          if (Widx == Wnum) then
             interp =Afrac*AIMEfrac*Wfrac*V(ageadj+1, AIMEidx, Widx, Aidx, Bidx)
          else
             interp = Afrac*AIMEfrac*((1.0_8-Wfrac)*V(ageadj+1, AIMEidx, Widx, Aidx, Bidx)&
             & + Wfrac*V(ageadj+1, AIMEidx, Widx+1, Aidx, Bidx))
          end if
       else
          if (Widx == Wnum) then
             interp = Afrac*(1.0_8-AIMEfrac)*Wfrac*V(ageadj+1, AIMEidx, Widx, Aidx, Bidx) &
                  + Afrac*AIMEfrac*Wfrac*V(ageadj+1, AIMEidx+1, Widx, Aidx, Bidx)
          else
             interp = Afrac*(1.0d0-AIMEfrac)*((1.0d0-Wfrac)*V(ageadj+1, AIMEidx, Widx, Aidx, Bidx)&
             & + Wfrac*V(ageadj+1, AIMEidx, Widx+1, Aidx, Bidx))&
             & + Afrac*AIMEfrac*((1.0d0-Wfrac)*V(ageadj+1, AIMEidx+1, Widx, Aidx, Bidx) &
             & + Wfrac*V(ageadj+1, AIMEidx+1, Widx+1, Aidx, Bidx))
          end if
       end if
    else
       if (AIMEidx == AIMEnum) then
          if (Widx == Wnum) then
             interp = (1.0d0-Afrac)*AIMEfrac*Wfrac*V(ageadj+1, AIMEidx, Widx, Aidx, Bidx) &
                  +Afrac*AIMEfrac*Wfrac*V(ageadj+1, AIMEidx, Widx, Aidx+1, Bidx)
          else
             interp =(1.0d0-Afrac)*AIMEfrac*((1.0_8-Wfrac)*V(ageadj+1, AIMEidx, Widx, Aidx, Bidx) &
             & + Wfrac*V(ageadj+1, AIMEidx, Widx+1, Aidx, Bidx)) &
             &     +Afrac*AIMEfrac*((1.0_8-Wfrac)*V(ageadj+1, AIMEidx, Widx, Aidx+1, Bidx)&
             & + Wfrac*V(ageadj+1, AIMEidx, Widx+1, Aidx+1, Bidx))
          end if
       else
          if (Widx == Wnum) then
             interp = (1.0d0-Afrac)*(1.0d0-AIMEfrac)*Wfrac*V(ageadj+1, AIMEidx, Widx, Aidx, Bidx) &
                  + (1.0d0-Afrac)*AIMEfrac*Wfrac*V(ageadj+1, AIMEidx+1, Widx, Aidx, Bidx)&
                  + Afrac*(1.0d0-AIMEfrac)*Wfrac*V(ageadj+1, AIMEidx, Widx, Aidx+1, Bidx)&
                  + Afrac*AIMEfrac*Wfrac*V(ageadj+1, AIMEidx+1, Widx, Aidx+1, Bidx)
          else
             interp = (1.0d0-Afrac)*(1.0d0-AIMEfrac)*((1.0d0-Wfrac)*V(ageadj+1, AIMEidx, Widx, Aidx, Bidx) &
                & + Wfrac*V(ageadj+1, AIMEidx, Widx+1, Aidx, Bidx)) &
                  + (1.0d0-Afrac)*AIMEfrac*((1.0d0-Wfrac)*V(ageadj+1, AIMEidx+1, Widx, Aidx, Bidx) &
                  & + Wfrac*V(ageadj+1, AIMEidx+1, Widx+1, Aidx, Bidx)) &
                  & + Afrac*(1.0_8-AIMEfrac)*((1.0_8-Wfrac)*V(ageadj+1, AIMEidx, Widx, Aidx+1, Bidx) & 
                  & + Wfrac*V(ageadj+1, AIMEidx, Widx+1, Aidx+1, Bidx)) &
                  & + Afrac*AIMEfrac*((1.0_8-Wfrac)*V(ageadj+1, AIMEidx+1, Widx, Aidx+1, Bidx) &
                  & + Wfrac*V(ageadj+1, AIMEidx+1, Widx+1, Aidx+1, Bidx))
          end if
       end if
    end if

    if (interp > vpanish*2) then
       return
    else
       write(*,*) 'interp is less than vpanish!!'
       write(*,*) 'A=', A
       write(*,*) 'Afrac=', Afrac
       write(*,*) 'Aidx=', Aidx
       !write(*,*) 'W=', W
       !write(*,*) 'AIME=', nextperiodAIME
       !write(*,*) 'B=', Bi
       write(*,*) 'V=', V(ageadj+1, AIMEidx, Widx, Aidx, Bidx)
       write(*,*) 'interp=', interp
    end if

end function

end module mod_interp
