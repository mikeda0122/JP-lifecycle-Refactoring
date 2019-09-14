module mod_GETADJ

  use mod_parameter
  implicit none

contains

  subroutine getadj(age, eretadj, lretadj)
    integer(8), intent(in) :: age
    real(8), intent(out) :: eretadj, lretadj

    eretadj = 0.0_8
    lretadj = 0.0_8

    if ((age < penage)) then
      if (age > 64) then
         eretadj = eretadj64
      end if
      if (age == 64) then
         eretadj = eretadj64
      end if
      if (age == 63) then
         eretadj = eretadj63
      end if
      if (age == 62) then
         eretadj = eretadj62
      end if
      if (age == 61) then
         eretadj = eretadj61
      endif
      if (age == 60) then
         eretadj = eretadj60
      end if
   end if

   if ((age > penage)) then
      if (age > 65) then
         lretadj = lretadj66
      else if (age == 66) then
         lretadj = lretadj66
      else if (age == 67) then
         lretadj = lretadj67
      else if (age == 68) then
         lretadj = lretadj68
      else if (age == 69) then
         lretadj = lretadj69
      else if (age == 70) then
         lretadj = lretadj70
      end if
   end if

   return
 end subroutine getadj
end module mod_GETADJ
