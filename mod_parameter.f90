module mod_parameter

    implicit none

    ! pension
    real(8), parameter :: pb0 = (-66.50_8)*0.68_8
    real(8), parameter :: pb1 = (0.31460_8)*0.68_8
    real(8), parameter :: pb2 = (2.8240_8)*0.68_8
    real(8), parameter :: pbbk = 5000.0_8
    integer(8), parameter :: penbensstart = 62_8!55_8

    real(8), parameter :: pax = 0.63_8
    real(8), parameter :: pa0 = 0.0334_8*0.40_8
    real(8), parameter :: pa1 = .0000028_8*0.40_8
    real(8), parameter :: pa2 = .0988_8*0.40_8
    real(8), parameter :: pabk = 23000.0_8

    ! utility
    integer(1) :: nonsep = 1_1
    real(8) :: p_conspref
    real(8) :: p_gamc
    real(8) :: p_gamh
    real(8) :: p_leispref
    real(8) :: p_leisprefbad
    real(8) :: p_fixcost
    real(8) :: p_L
    real(8) :: p_bequest
    real(8) :: p_onemgamc
    real(8) :: p_onemgamh
    real(8) :: p_beqk
    real(8) :: p_avcons = 20000 

    real(8), parameter :: ror = 0.04d0
    !Spouse income coefficients
    real(8), parameter :: spicns = 57314.2 !US data 28190.34
    real(8), parameter :: spiage1 = -5981.466 !US data -5967.59
    real(8), parameter :: spiage2 = 190.6772 !US data 255.6247
    real(8), parameter :: spiage3 = -2.528299 !US data -4.82701
    real(8), parameter :: spiage4 = 0.0118362 !US data 0.0422403
    real(8), parameter :: spiage5 = 0.0_8 !US data -.0001402
    real(8), parameter :: spilnw = 6528.143 !US data 9128.822

    ! Tax brackets and marginal tax rates
    real(8), parameter :: tbk1 = 4440.0_8
    real(8), parameter :: tbk2 = 6940.0_8
    real(8), parameter :: tbk3 = 27440.0_8
    real(8), parameter :: tbk4 = 42440.0_8
    real(8), parameter :: tbk5 = 43800.0_8
    real(8), parameter :: tbk6 = 84440.0_8
    !marginal taxes and ati, with SS
    real(8), parameter :: mtr1 = 0.0715d0
    real(8), parameter :: mtr2 = 0.204d0
    real(8), parameter :: mtr3 = 0.251d0
    real(8), parameter :: mtr4 = 0.3979d0
    real(8), parameter :: mtr5 = 0.4738d0
    real(8), parameter :: mtr6 = 0.4023d0
    real(8), parameter :: mtr7 = 0.4395d0
    ! After-tax income at bracket points:
    !ati(j) = ati(j-1) + [1-mtr(j)]*[tbk(j)-tbk(j-1)]
    real(8), parameter :: ati1 = 4123.d0
    real(8), parameter :: ati2 = 6113.d0
    real(8), parameter :: ati3 = 21468.d0
    real(8), parameter :: ati4 = 30500.d0
    real(8), parameter :: ati5 = 31216.d0
    real(8), parameter :: ati6 = 55506.d0

    ! marginal taxes and ati, with SS taxes reduced 20%
!    real(8), parameter :: mtr1a = 0.0611d0
!    real(8), parameter :: mtr2a = 0.1936d0
!    real(8), parameter :: mtr3a = 0.2936d0
!    real(8), parameter :: mtr4a = 0.3875d0
!    real(8), parameter :: mtr5a = 0.4635d0
!    real(8), parameter :: mtr6a = 0.4023d0
!    real(8), parameter :: mtr7a = 0.4395d0
    !  After-tax income at bracket points:
    !  ati(j) = ati(j-1) + [1-mtr(j)]*[tbk(j)-tbk(j-1)]
!    real(8), parameter :: ati1a = 4169.d0
!    real(8), parameter :: ati2a = 6185.d0
!    real(8), parameter :: ati3a = 21753.d0
!    real(8), parameter :: ati4a = 30941.d0
!    real(8), parameter :: ati5a = 31672.d0
!    real(8), parameter :: ati6a = 55962.d0

    ! marginal taxes and ati, without SS
    real(8), parameter :: mtr1a = 0.0055d0
    real(8), parameter :: mtr2a = 0.139d0
    real(8), parameter :: mtr3a = 0.185d0
    real(8), parameter :: mtr4a = 0.3309d0
    real(8), parameter :: mtr5a = 0.4078d0
    real(8), parameter :: mtr6a = 0.4023d0
    real(8), parameter :: mtr7a = 0.4395d0
    !  After-tax income at bracket points:
    !  ati(j) = ati(j-1) + [1-mtr(j)]*[tbk(j)-tbk(j-1)]
    real(8), parameter :: ati1a = 4416.d0
    real(8), parameter :: ati2a = 6569.d0
    real(8), parameter :: ati3a = 23277.d0
    real(8), parameter :: ati4a = 33314.d0
    real(8), parameter :: ati5a = 34119.d0
    real(8), parameter :: ati6a = 58409.d0


    real(8), parameter :: AIMEbk1 = 3720.0_8
    real(8), parameter :: AIMEbk2 = 22392.0_8
    real(8), parameter :: mpr1 = 0.9_8
    real(8), parameter :: mpr2 = 0.32_8
    real(8), parameter :: mpr3 = 0.15_8
    real(8), parameter :: PIA1 = 3348.0_8   !  AIMEbk1*mpr1
    real(8), parameter :: PIA2 = 9323.0_8   !  PIA1 + (AIMEbk2-AIMEbk1)*mpr2
    integer, parameter :: AIMEage = 70 !60
    integer, parameter :: startwork = 25
    integer, parameter :: AIMEyrs = 35  !  AIMEage - startwork
    real(8), parameter :: disabincometest = 3600.0_8 ! note that this is now a negative number

    !grids of W
    integer(8) :: Wnum    = 10
    integer(8) :: Wnint    = 5
    real(8) :: Wmax    = 70 ! 60 in French
    real(8) :: Wmin    = 8  !3 in French
    real(8) :: Wint    = 30 ! 15 in French

    !grids of H
    integer(8) :: Hnum    = 10
    integer(8) :: Hnint    = 5
    integer(8) :: Hnum2    = 10
    integer(8) :: Hnint2    = 5
    real(8) :: Hmax    = 4000 !same as French
    real(8) :: Hmin    = 500 !same as French
    real(8) :: Hint    = 2000 !same as French

    ! grids of A
    integer(8) :: Asnum =   30
    integer(8) :: Asnint    = 15
!    integer(8) :: Anum =   10
!    integer(8) :: Anint    = 5
    real(8) :: Asmax    = 600000 !same as French
    real(8) :: Asmin    = 0 !same as French
    real(8) :: Asint    = 80000 !same as French

    ! grids of AIME
    integer(8) :: AIMEnum  =  10
    integer(8) :: bdplace =    5
    integer(8) :: AIMEnint = 5
    real(8) :: AIMEmax  = 45200 !42480 in French, 70 * 2500 * * 7.38/1000 * 35
    real(8) :: AIMEmin  =  1200 !1960 in French, 8 (100yen)* 2000 (hours) * 7.38/1000 (penrate) * 10 (years)
    real(8) :: AIMEint = 16000 !9000 in French, 25 * 2500 * 7.38/1000* 10

    ! grids of Bi
    integer(8) :: Bnum = 2

    !real(8), parameter :: vpanish = -100000000000000000000.0_8
    real(8), parameter :: vpanish = -1.0d200


    real(8) :: p_beta
    real(8) :: cfloor = 1.0_8
    integer(8) :: Cnum = 181
    integer(8) :: Cnum2 = 180

    ! age parameters
    real(8) :: L = 5280  ! Time endowment: hours/year
    integer(8) :: TR = 66  ! Number of periods in the life
    integer(8) :: bornage = 30
    integer(8) :: momage = 30
    integer(8) :: momage2 = 69
    integer(8) :: trdat
    integer(8) :: mashft
    integer(8) :: trdat2
    integer(8) :: retage = 70  ! Age by which everyone will exit the labor force
    ! integer(8) :: ageseq(_tr)
    integer(8) :: dieage = 95
    integer(8) :: mappage = 70  ! Age by which everyone will apply for SS benefits
    integer(8) :: penage = 65  ! Age of first pension benefits
    integer(8) :: firstpen = 60 !Age of first pension benefits
    integer(8) :: eretage = 60 !early retirement age
    integer(8) :: nret = 65 !normal retirement age
    integer(8) :: etstage = 70 !From this age, SS benefits don't face earnings test
    ! _trdat = momage2-momage+1
    ! mashft = momage-bornage+1
    ! _trdat2 = _trdat+mashft-1
    ! ageseq(_tr) = [(bornage+i, i=1,_tr)]
    ! dieage = bornage+_tr-1
    
    !**************************************************
    !*********Parameters regarding pension

    real(8) :: penrate = 0.007308
    real(8) :: const_pen = 7793.0_8 !!!We just assume everyone (even those without job) participate in National pension
    !early application penalty Factors
    real(8) :: eretadj64 = 0.06_8 !0.93333 in French
    real(8) :: eretadj63 = 0.12_8 !0.9286 in French
    real(8) :: eretadj62 = 0.18_8 !0.92308 in French
    real(8) :: eretadj61 = 0.24_8 
    real(8) :: eretadj60 = 0.30_8

    real(8) :: lretadj65 = 0.0_8 !0.03 in French
    real(8) :: lretadj66 = 0.084_8 !0.02913 in French
    real(8) :: lretadj67 = 0.168_8 !0.0283 in French
    real(8) :: lretadj68 = 0.252_8 !0.02752 in French
    real(8) :: lretadj69 = 0.336_8 !0.026786 in French
    real(8) :: lretadj70 = 0.420_8 !0.025 in French
 
    ! Parameters for crediting back SS benefits lost to earnings test
    !real(8) :: bigcred64 = 0.0714
    !real(8) :: bigcred63 = 0.07692
    !real(8) :: bigcred62 = 0.08333
    !random values again (sungwan)
    !real(8) :: bigcred61 = 0.8333
    !real(8) :: bigcred60 = 0.77

    !*****Earnings Test
    !taxfrac !changes applied in mod_ass.f90
    real(8) :: taxfrac != 0.5 !Used in mod_ass.f90, effective only if earnings below earnings test threshold levels
    real(8) :: earnlev != 6000 !earnlev when taxfrac = 0.5. found in algs55.src line 207.

    !taxtype
    integer(1) :: taxtype = 0
    !liquidity
    integer(1) :: liquid = 0_1

    !integral part
    real(8):: wnodes(1:5) = (/ -2.02018287d0, -0.9585724646d0, 0.0d0, 0.9585724646d0, 2.02018287d0 /)
    !real(8):: wnodes(1:7) = (/ -2.651961356, -1.673551628, - 0.8162878828, 0.0_8, 0.8162878828, 1.673551628, 2.651961356 /)
    real(8):: wwgts(1:5) = (/ 0.01995624205d0, 0.3936193231d0, 0.9453087204d0, 0.3936193231d0, 0.01995624205d0 /)
    !real(8):: wwgts(1:7) = (/  0.000971781245, 0.05451558281, 0.4256072526, 0.8102646175, 0.4256072526, 0.05451558281, 0.000971781245 /)
    integer(8), parameter::  wnodenum = 5
    real(8):: pi
    real(8):: rhow     = 0.977
    real(8):: stderr = 0.12_8

    integer(8) :: momnum = 69-30+1 !!momage2-momage+1
    integer(8) :: simnum = 3500
    integer(8) :: datanum = 3949
    integer(8) :: params_num = 7
    
    real(8) :: dpanish = -1.0_8

    integer(8) :: PrintResults = 1_8
    integer(8) :: two_step = 0_8
    integer(8) :: French_weight = 1_8
    integer(8) :: estimation = 1_8
    integer(8) :: job = 0_8
    integer(8) :: objective_part=1_8
    integer(8) :: init_iter = 1_8
    
    !tiedwage
    integer(8) :: tiedwage = 1_8
    !selection adjustment
    integer(8) :: select_adj = 1_8
    !earnings test
    integer(8) :: etest = 1_8

  end module mod_parameter
  
