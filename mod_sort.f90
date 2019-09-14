module mod_sort

  implicit none
  
contains

  subroutine sort(data, dnum, dpanish, mvcode, sdata)
    !!!!Note this sort subroutine works for an "one dimension" "non-negative" vector.!!!!!!!!!

    real(8), intent(in) :: data(:)
    integer(8), intent(in) :: dnum
    real(8), intent(in) :: dpanish, mvcode
    real(8), intent(out) :: sdata(:)

    real(8) :: temp_data(dnum)
    integer(8) :: loc(1), idx
    real(8) :: val, pval

    integer(8) :: i

    temp_data = data
    pval = mvcode
    
    do i = 1, dnum
       loc = maxloc(temp_data)
       idx = loc(1)
       
       val = temp_data(idx)

       if (val<=dpanish) then
          val = pval
       end if
       
       sdata(i) = val

       temp_data(idx) = pval
    end do

  end subroutine sort

end module mod_sort




