module mod_sprob


  use mod_parameter
implicit none
  contains

  subroutine computeg(gvec)
    implicit none

    !real(8), intent(in)  :: ror
    !integer(8), intent(in) :: firstpen, dieage, penage
    real(8), intent(out) :: gvec(66)
    real(8) :: age_temp(66), sprob(66), discount(66), rev_disc1(66)
    real(8) :: tseq(66), disc1(66), d(66), d2(66), z(66), cumsumc1(66), cumsumc(66)
    integer(8) :: i_i
    integer(8) :: temp, temp2
    integer :: ios


    open(unit = 67, iostat = ios, file = 'sprob.txt', &
      & action = 'read', form = 'formatted', status = 'old')

      do i_i = 1,66,1
        read(67, '(f5.2, 15X, f6.4)') age_temp(i_i), sprob(i_i)
      end do
      !print*, "sprob(29)", sprob(29)
      close(67)
!   @--Gvec is computed assuming that no pension payments are made at dieage---@
!/* "rows(sprob)= " rows(sprob) ;
!"cols(sprob) = " cols(sprob);
!"firstpen = " firstpen;
!" bornage= " bornage;
!" _tr =" _tr; */

    sprob(1) = 1.0_8
    sprob(66) = 0.0_8 ! dead for sure in the final time period
    gvec = 0.0_8 * sprob
    discount = gvec
    !rn = dieage - firstpen + 1
    do i_i = 1, 66, 1
      tseq(i_i) = i_i -1.0_8
    end do
    z = 0.0_8
    d(1) = sprob(1) + z(1) !@ Use sprob_t to discount x_t+1 @
    do i_i = 2, 66, 1
        d(i_i) = d(i_i-1)*sprob(i_i) + z(i_i)
    end do

    d2 = (1.0_8 / (1.0_8 + ror)) ** tseq

    do i_i = 1, 66, 1
      discount(i_i) = d(i_i) * d2(i_i)       ! @ Survival-weighted cumulative discount factors @
    end do
    temp = penage - bornage
    temp2 = temp + 1
    do i_i = 1, temp, 1
      disc1(i_i) = 0.0_8
    end do
    do i_i = temp2, dieage-30, 1
      disc1(i_i) = 1
    end do
    !do i_i = dieage-30, 66, 1 !66=dieage-bornage+1
    !  disc1(i_i) = 0
    !end do
    ! Here, original code does NOT seem to define this vector after dieage.


!	"rows of disc1 and discount = "    rows(disc1)~rows(discount);
    do i_i = 1, 66, 1
      disc1(i_i) = disc1(i_i)*discount(i_i)
    end do
    do i_i = 1, 66 ,1
      rev_disc1(i_i) = disc1(67 - i_i) !Reverse
    end do
    cumsumc1(1) = rev_disc1(1)
    do i_i = 2, 66, 1
      cumsumc1(i_i) = rev_disc1(i_i) + rev_disc1(i_i - 1)
    end do
    do i_i = 1, 66, 1
      gvec(i_i) = cumsumc1(67 - i_i)
    end do
    do i_i = 1, 66, 1
      gvec(i_i) = gvec(i_i)/discount(i_i)
    end do
    gvec = (1.0_8 / (1.0_8 + ror)) * gvec
    gvec(66) = 0.01_8 !/*this overcomes the division by zero problem */

  end subroutine computeg

  subroutine computeg_2(gvec)
    implicit none

    !real(8), intent(in)  :: ror
    !integer(8), intent(in) :: firstpen, dieage, penage
    real(8), intent(out) :: gvec(66)
    integer(8) :: i_i
    integer(8) :: temp, temp2
    integer :: ios

    open(unit = 68, iostat = ios, file = 'gvec.csv', &
      & action = 'read', form = 'formatted', status = 'old')

      do i_i = 1,66,1
         read(68, *) gvec(i_i)
         !write(*,*) i_i, ',', gvec(i_i)
      end do
      !print*, "sprob(29)", sprob(29)
      close(68)
  end subroutine computeg_2

end module mod_sprob
