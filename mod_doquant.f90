module mod_doquant

  use mod_parameter

  implicit none

contains
    subroutine doquant(data, dnum, q, qval)

    real(8), intent(in) :: data(:)
    integer(8), intent(in) :: dnum !dnum = size(data)
    real(8), intent(in) :: q !quantile, 0<= q <= 1 has to hold.

    real(8), intent(out) :: qval
    
    real(8) :: sdata(dnum)
    real(8) :: mask(dnum)

    real(8) :: rlength
    integer(8) :: ilength, temp_dnum, qnum
    
    real(8) :: mvcode !replacement for missing value

    integer(8) :: h

    mvcode = dpanish - 1.0_8 

    call sort(data, dnum, dpanish, mvcode, sdata)

    do h = 1, dnum-1
       if (sdata(h) < sdata(h+1)) then
          write(*,*) 'stop at mod_doquant!!'
          write(*,*) 'h=', h
          write(*,*) 'sdata(h)=', sdata(h)
          write(*,*) 'sdata(h+1)=', sdata(h+1)
          read*
       end if
    end do
    
    do h = 1, dnum
       if (sdata(h) > dpanish) then
          mask(h) = 1
       else
          mask(h) = 0
       end if
    end do

    !!The number of non-missing elements
    rlength = sum(mask)
    !!The "qnum"th element in sorted data is the "q"th quantile of data.
    qnum = int(rlength*q)

    if (rlength==0.0_8) then
       qval = 0.0_8
    else
       qval = sdata(qnum)
    end if
    
  end subroutine doquant

  subroutine sort(data, dnum, dpanish, mvcode, sdata)
    !!!!Note this sort subroutine works for an "one dimension" vector.!!!!!!!

    real(8), intent(in) :: data(:)
    integer(8), intent(in) :: dnum
    real(8), intent(in) :: dpanish, mvcode
    real(8), intent(out) :: sdata(:)

    real(8) :: temp_data(dnum)
    integer(8) :: location(1), idx
    real(8) :: val, pval

    integer(8) :: i

    temp_data = data
    pval = mvcode
    
    do i = 1, dnum
       location = maxloc(temp_data)
       idx = location(1)
       
       val = temp_data(idx)

       if (val<=dpanish) then
          val = pval
       end if
       
       sdata(i) = val

       temp_data(idx) = pval
    end do

  end subroutine sort
end module mod_doquant


