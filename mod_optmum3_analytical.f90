module mod_optmum3_analytical

  use mod_parameter
  use mod_computePIA
  use mod_pension
  use mod_computeAfterTaxIncome
  
  implicit none

contains

  subroutine optmum3_analytical(A, AIME, M, C_opt, A_opt, val)
    implicit none

    real(8), intent(in) :: A, AIME
    real(8), intent(in) :: M  ! = 1 if Bad

    real(8) :: alpha1
    real(8) :: alpha2
    real(8) :: alpha3
    real(8) :: C_numerator
    real(8) :: C_denominator
    real(8) :: PIA
    real(8) :: ss
    real(8) :: pb
    real(8) :: MTR
    real(8) :: Y
    real(8), intent(out) :: C_opt, A_opt
    real(8), intent(out) :: val

    PIA = computePIA(AIME)
    ss = PIA
    pb = predictpensionbenefits(PIA, 95_8)
    
    Y = computeAfterTaxIncome(0.0_8, A, MTR, 0.0_8, pb, taxtype, 95_8)

    alpha1 = (1.0_8 / (1.0_8-p_gamc))*((p_leispref - p_leisprefbad*M)** &
    & ((1.0_8 - p_gamh)*(1.0_8 - p_gamc)) )
    alpha2 = p_beta * p_bequest / (1.0_8 - p_gamc)
    ! the total income
    alpha3 = A + Y + ss

    if (alpha3 < cfloor) then
        C_opt = cfloor
        A_opt = 0
    else
        C_numerator = alpha3 + p_beqk
        C_denominator = (alpha1 / alpha2)**(1.0_8/(p_gamh*(1.0_8-p_gamc)-1.0_8))+1.0_8
        C_opt = C_numerator / C_denominator
        A_opt = alpha3 - C_opt

        ! Deal with the case of the corner soliution.
        ! C cannot be negative, since if so, FOC would be infinite.
        ! A can be negative following the fomulation above.
        ! However A represents asset (here t = 96), so must not be negative.
        if (A_opt < 0.0_8) then
            A_opt = 0.0_8
            C_opt = alpha3
        end if

        if (C_opt < cfloor) then
            C_opt = cfloor
            A_opt = alpha3 - cfloor
        endif
    endif

    ! Compute the value of value function.

    val = p_conspref * (alpha1 * C_opt**(p_gamh * (1.0_8 - p_gamc)) + alpha2 * (A_opt + p_beqk) ** (p_gamh * (1.0_8 - p_gamc)))

  end subroutine optmum3_analytical
  
end module mod_optmum3_analytical



