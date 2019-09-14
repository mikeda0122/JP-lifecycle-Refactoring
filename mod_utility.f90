module mod_utility
    use mod_parameter
    implicit none

  contains

    real(8) function U(c, h, particip, badheal, nonsep)
    ! Utility function (ret30.cpp: line 189-214)
    ! Arguments:
    !   intent(in)
    !       n: integer. length of input vectors c and h.
    !       c: vector of real(8). data of consumption.
    !       h: vector of real(8). data of working hours.
    !       particip: vector of integer(1). data of dummy variables indicating
    !                 whether individual participating in social security.
    !       badheal: vector of integer(1). data of dummy variables indicating
    !                whether individual is in bad health.
    !   intent(out)
    !       utils: vector of real(8). calculated utilities.

        implicit none
        real(8), intent(in) :: c
        real(8), intent(in) :: h
        integer(1), intent(in) :: particip
        real(8), intent(in) :: badheal
        integer(1) :: nonsep
    !    real(8), intent(out) :: U

        real(8) :: leisure
        real(8) :: within

        U = vpanish
        
        if (nonsep == 1_1) then
           leisure = p_leispref - h - ((p_fixcost*particip) + (p_leisprefbad*badheal))

           if (c<=0 .or. leisure<=0) then
              !!We need to think about how to deal with exception!!(Ikeda)
              U = vpanish
              return
           end if
           
           within = (c**p_gamh) * (leisure**(1-p_gamh))
           if (p_gamc == 1.0_8) then
              U = p_conspref * log(within)
           else
              U = p_conspref * (within**(1-p_gamc)) * p_onemgamc
           end if
        else if (nonsep == 0_1) then
           within = c**(1-p_gamc)*p_onemgamc
           leisure = 5280-h-((p_fixcost*particip) + (p_leisprefbad*badheal))
           leisure = leisure**(1-p_gamh)*p_onemgamh
           U = p_conspref*(within + (p_leispref*leisure))
        end if

      end function U

    real(8) function beq(assets, nonsep)
    ! Bequest function (ret30.cpp: line 220-239)
    ! Arguments:
    !   intent(in)
    !       assets: vector of real(8). assets held by each individual upon death.
    !   intent(out)
    !       val: vector of real(8). calculated bequest utilities.

        implicit none
        real(8), intent(in) :: assets
        integer(1) :: nonsep
    !    real(8), intent(out) :: beq

        beq = vpanish

        if (nonsep == 1_1) then
            if (assets < -(p_beqk - 1.0_8)) then
                beq = p_onemgamc + (assets + p_beqk)
            else
                if (p_gamc == 1.0_8) then
                    beq = log(assets + p_beqk)
                else
                    beq = ((assets + p_beqk) ** ((1 - p_gamc)*p_gamh)) * p_onemgamc
                end if
            end if
        else if (nonsep == 0_1) then
            beq = (assets + p_beqk) ** (1 - p_gamc) * p_onemgamc
        end if

        beq = beq * p_bequest * p_conspref

    end function beq

end module mod_utility
