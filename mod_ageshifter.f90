module mod_ageshifter
  implicit none

contains

subroutine ageshifter(ageshift)
  implicit none

  real(8), intent(out) :: ageshift(66)
  integer(8) :: i
  integer(4) :: ios

  open(unit =65, iostat = ios, file = 'ageshift.csv', action = 'read', form = 'formatted', &
       & status = 'old')
       if (ios /= 0) then
         write(*,*) 'Failed to open!'
         stop
       end if
  do i = 1,66,1
    read(65, *) ageshift(i)
  end do
  !print*, ageshift(7)
  close(65)

  end subroutine ageshifter
end module mod_ageshifter
