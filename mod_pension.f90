module mod_pension

    use mod_parameter

    implicit none

    real(8) :: penavailage, pencfs

contains

  subroutine computepenaccrue(age, ageshift, laborincome, penaccrue)
    ! see Eric (2003) equation 37

    implicit none

    integer(8), intent(in) :: age
    real(8), intent(in) :: ageshift(:), laborincome
    real(8), intent(out) :: penaccrue


    if (laborincome < 0.0_8) then
      penaccrue = 0.0_8
      return
    end if

    if (laborincome < pabk) then
      penaccrue = pax * (pa0 + pa1 * laborincome) * ageshift(age-bornage+1) * laborincome
      return
    end if

    penaccrue = pax * pa2 * ageshift(age-bornage+1) * laborincome

    return
  end subroutine computepenaccrue


  subroutine predictpensionwealth(AIME, pencfs, age, assets, penpred)
    implicit none
    real(8), intent(in) :: AIME, assets
    integer(8), intent(in) :: age
    real(8), intent(in) :: pencfs(6)
    real(8), intent(out) :: penpred
    real(8) :: agekink

    penavailage = 94.0_8

    if (age > (penavailage - 1.0_8)) then
      penpred = 0.0_8
      return
    end if

    agekink = age - penbensstart
    if (agekink < 0.0_8) then
      agekink = 0.0_8
    endif

    penpred = pencfs(1) + pencfs(2)*age + pencfs(3)*agekink + &    ! fortran's index starts from 1
    & pencfs(4)*AIME + pencfs(5)*AIME*age + pencfs(6)*AIME * agekink
    penpred = penpred * (dieage - 1.0_8 - age)

    if (penpred < 0.0_8) then
      penpred = 0.0_8
    end if

    return
  end subroutine predictpensionwealth


    real(8) function predictpensionbenefits(PIA, age)
    implicit none
    real(8), intent(in) :: PIA
    integer(8), intent(in) :: age
    real(8) :: bigPIA

!!    if (age < penbensstart - 0.01) then !!Why do we need -0.01? (Ikeda)
    if (age < penbensstart) then
       predictpensionbenefits = 0.0_8
      return
    end if

    bigPIA = 0.0_8
    if (PIA > pbbk) then
      bigPIA = 1.0_8
    end if

    predictpensionbenefits = pb0 + (pb1*PIA) + (pb2*bigPIA*(PIA-pbbk))

  end function predictpensionbenefits

end module mod_pension
