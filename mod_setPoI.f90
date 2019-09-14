module mod_setPoI

  use mod_parameter

  implicit none

contains
  subroutine setPoI(params)

    real(8), intent(in) :: params(:)
    real(8) :: leisu, consu

    if (job==0_8) then
       p_gamh = params(1)
       p_gamc = params(2)
       p_leispref = params(3)
       p_leisprefbad = params(4)
       p_fixcost = params(5)
       p_beta = params(6)
       p_bequest = params(7)
       p_conspref = 1000000.0_8
       p_beqk = 500000.0_8
       p_onemgamc = 1.0_8
       if (p_gamc/=1.0_8) then
          p_onemgamc = 1.0_8/(1.0_8-p_gamc)
       end if
       p_onemgamh = 1.0_8
       if (p_gamh/=1.0_8) then
          p_onemgamh = 1.0_8/(1.0_8-p_gamh)
       end if
       rhow     = 0.977
       stderr = 0.12_8
    else if (job==1_8 .or. job==2_8 .or. job==3_8) then
       p_gamh = params(1)/10.0_8
       p_gamc = params(2)
       p_leispref = params(3)*1000.0_8
       p_leisprefbad = params(4)*100.0_8
       p_fixcost = params(5)*100.0_8
       p_beta = params(6)/10.0_8
       p_bequest = params(7)/100.0_8
       p_conspref = 1000000.0_8
       p_beqk = 500000.0_8
       p_onemgamc = 1.0_8
       if (p_gamc/=1.0_8) then
          p_onemgamc = 1.0_8/(1.0_8-p_gamc)
       end if
       p_onemgamh = 1.0_8
       if (p_gamh/=1.0_8) then
          p_onemgamh = 1.0_8/(1.0_8-p_gamh)
       end if
       rhow     = 0.977
       stderr = 0.12_8
    end if
    !rhow     = params(8)
    !stderr = params(9)
    !write(*,*) 'p_leispref', p_leispref
    !write(*,*) 'p_fixcost', p_fixcost

    !  p_gamh = 0.593194_8
    !  p_gamc = 3.51206_8
    !  !p_gamc = 1.0_8
    !  p_leispref = 4762.64_8
    !  p_leisprefbad = 258.115_8
    !  p_fixcost = 928.774_8
    !  p_beta = 0.984991_8
    !  p_bequest = 0.0255898_8
    !  p_conspref = 1000000.0_8
    !  p_beqk = 500000.0_8
    !  p_onemgamc = 1.0_8
    !  if (p_gamc/=1.0_8) then
    !     p_onemgamc = 1.0_8/(1.0_8-p_gamc)
    !  end if
    !    p_onemgamh = 1.0_8
    !  if (p_gamh/=1.0_8) then
    !     p_onemgamh = 1.0_8/(1.0_8-p_gamh)
    !  end if

    !**********parameters in the "params.out" file**********
    !Note p_leispref > 4000+p_fixcost+p_leispref_bad
    !p_gamh = 0.54767_8
    !p_gamc = 2.8943_8
    !p_leispref = 5150.0_8
    !p_leispref = 5280.0_8
    !p_leisprefbad = 279.19_8
    !p_fixcost = 963.92_8 !!params(1) !!505.1515 !963.92_8 in params.out
    !p_beta = 0.99298_8
    !p_bequest = 0.8892886 !0.88914_8
    !p_conspref = 1000000.0_8
    !p_beqk = 500000.0_8
    !p_onemgamc = 1.0_8
    !if (p_gamc/=1.0_8) then
    !   p_onemgamc = 1.0_8/(1.0_8-p_gamc)
    !end if
    !p_onemgamh = 1.0_8
    !if (p_gamh/=1.0_8) then
    !   p_onemgamh = 1.0_8/(1.0_8-p_gamh)
    !end if
    rhow     = 0.977
    stderr = 0.12_8

    !**********parameter(1) in the French(2005) TABLE2**********
     ! p_gamh = 0.578_8
     ! p_gamc = 3.34_8
     ! p_leispref = 4466.0_8
     ! p_leisprefbad = 318.0_8
     ! p_fixcost = 1313.0_8
     ! p_beta = 0.992_8
     ! p_bequest = 1.69_8
     ! p_conspref = 1000000.0_8
     ! p_beqk = 500000.0_8
     ! p_onemgamc = 1.0_8
     ! if (p_gamc/=1.0_8) then
     !    p_onemgamc = 1.0_8/(1.0_8-p_gamc)
     ! end if
     ! p_onemgamh = 1.0_8
     ! if (p_gamh/=1.0_8) then
     !    p_onemgamh = 1.0_8/(1.0_8-p_gamh)
     ! end if
     
    !**********parameter(3) in the French(2005) TABLE2**********
    ! p_gamh = 0.533_8
    ! p_gamc = 3.19_8
    ! p_leispref = 3900.0_8
    ! p_leisprefbad = 196.0_8
    ! p_fixcost = 335.0_8
    ! p_beta = 0.981_8
    ! p_bequest = 1.70_8
    ! p_conspref = 1000000.0_8
    ! p_beqk = 500000.0_8
    ! p_onemgamc = 1.0_8
    ! if (p_gamc/=1.0_8) then
    !    p_onemgamc = 1.0_8/(1.0_8-p_gamc)
    ! end if
    !   p_onemgamh = 1.0_8
    ! if (p_gamh/=1.0_8) then
    !    p_onemgamh = 1.0_8/(1.0_8-p_gamh)
    ! end if

    !**********parameter(2) in the French(2005) TABLE2**********
    !  p_gamh = 0.602_8
    !  p_gamc = 3.78_8
    !  p_leispref = 4889.0_8
    !  p_leisprefbad = 191.0_8
    !  p_fixcost = 1292.0_8
    !  p_beta = 0.985_8
    !  p_bequest = 2.58_8
    !  p_conspref = 1000000.0_8
    !  p_beqk = 500000.0_8
    !  p_onemgamc = 1.0_8
    !  if (p_gamc/=1.0_8) then
    !     p_onemgamc = 1.0_8/(1.0_8-p_gamc)
    !  end if
    !    p_onemgamh = 1.0_8
    !  if (p_gamh/=1.0_8) then
    !     p_onemgamh = 1.0_8/(1.0_8-p_gamh)
    !  end if

    !**********parameter(4) in the French(2005) TABLE2**********
     ! p_gamh = 0.615_8
     ! p_gamc = 7.69_8
     ! p_leispref = 3399.0_8
     ! p_leisprefbad = 202.0_8
     ! p_fixcost = 240.0_8
     ! p_beta = 1.04_8
     ! p_bequest = 0.031734 !!0.037_8
     ! p_conspref = 1000000.0_8
     ! p_beqk = 500000.0_8
     ! p_onemgamc = 1.0_8
     ! if (p_gamc/=1.0_8) then
     !    p_onemgamc = 1.0_8/(1.0_8-p_gamc)
     ! end if
     !   p_onemgamh = 1.0_8
     ! if (p_gamh/=1.0_8) then
     !    p_onemgamh = 1.0_8/(1.0_8-p_gamh)
     ! end if


    !    !algs55.src line 18 scaler
    if (nonsep==1_1) then
       p_conspref=(p_avcons**(p_gamh))
       p_conspref=p_conspref*((p_leispref-p_fixcost-500.0d0)**(1.0d0-p_gamh))
       if (p_gamc==1.0d0) then
          p_conspref=log(p_conspref)
       else
          p_conspref=p_onemgamc*p_conspref**(1.0d0-p_gamc)
       endif
       p_conspref = abs(1.0d0 / p_conspref)
    else
       if (p_gamc /= 1.0d0) then
          consu = p_onemgamc*(p_avcons**(1.0d0-p_gamc))
       else
          consu = log(p_avcons)
       endif

       if (p_gamh /= 1.0d0) then
          leisu = p_leispref*p_onemgamh*((L-p_fixcost-1000.0d0)**(1.0d0-p_gamh))
       else
          leisu = p_leispref*log(L-p_fixcost-1000.0d0)
       endif

       p_conspref = 1.0d0/abs(consu+leisu)
    endif

    pi  = 4 * atan (1.0_8)
   

  end subroutine setPoI
end module mod_setPoI
