module mod_computelaborincome

    use mod_parameter
  implicit none


contains

  real(8) FUNCTION computelaborincome(wage, h) 
    implicit none
    real(8), intent(in) :: wage, h
    !real(8), intent(out) :: computelaborincome


    if (tiedwage == 0_8) then
        computelaborincome = wage * h
    else if (tiedwage == 1_8) then
        if (h > 0.0_8) then
            computelaborincome = wage * h ** 1.415_8 * 2000.0_8 ** (-0.415_8)
        else
            computelaborincome = 0.0_8
        end if
    else
       computelaborincome = 0.0_8
       write(*,*) 'tiedwage is not defined!!'
    end if

  end function computelaborincome

end module mod_computelaborincome
