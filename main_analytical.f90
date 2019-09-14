program main

  use mod_parameter
  use mod_makegrids
  use mod_optmum0
  use mod_optmum1
  use mod_optmum2
  use mod_optmum3_analytical
  use mod_simulation
  use mod_health_mortality
  use mod_profwage
  
  implicit none

  real(8) :: Astate(Anum), Hstate(Hnum), Wstate(Wnum), AIMEstate(AIMEnum)
  real(8) :: Copt, Aopt, Hopt, valopt
  real(8) :: M

  real(8) :: Vgood(dieage-bornage+1, AIMEnum, Wnum, Anum), Vbad(dieage-bornage+1, AIMEnum, Wnum, Anum)
  real(8) :: optC_good(dieage-bornage+1, AIMEnum, Wnum, Anum), optC_bad(dieage-bornage+1, AIMEnum, Wnum, Anum)
  real(8) :: optA_good(dieage-bornage+1, AIMEnum, Wnum, Anum), optA_bad(dieage-bornage+1, AIMEnum, Wnum, Anum)
  real(8) :: optH_good(dieage-bornage+1, AIMEnum, Wnum, Anum), optH_bad(dieage-bornage+1, AIMEnum, Wnum, Anum)

  real(8) :: prof_A_good(dieage-bornage+1), prof_A_bad(dieage-bornage+1)
  real(8) :: prof_C_good(dieage-bornage+1), prof_C_bad(dieage-bornage+1)
  real(8) :: prof_H_good(dieage-bornage+1), prof_H_bad(dieage-bornage+1)

  integer(8) :: Ai, Wi, AIMEi
  integer(8) :: age

  real(8), allocatable :: A_dist(:), M_dist(:), W_dist(:), AIME_dist(:)

  real(8) :: mortality_good(75), mortality_bad(75), good_to_bad(75), bad_to_bad(75)
  real(8) :: hlogwage(75), ulogwage(75)
  real(8) :: hhgr(60), hugr(60), uhgr(60), uugr(60)

  integer(8) :: sim_length
  integer(8) :: j
  
!  open(unit=10, iostat=ios, file='mortality.csv',&
!       & action='read', form='formatted', status='old')
!
!  if (ios/=0) then
!     write(*,*) 'Failed to open!!'
!     stop
!  end if
!  do j = 1,75,1
!     read(10,*) mortality(j)
!  end do
  
!  close(10)

!  open(unit=11, iostat=ios, file='logwage.csv', action='read', form='formatted', status='old')

!  if (ios/=0) then
!     write(*,*) 'Failed to open "logwage.csv"!!'
!     stop
!  end if
!  do j=1,60
!     read(11,*) logwage(j)
!  end do

!  close(11)

  call health_mortality(mortality_good, mortality_bad, good_to_bad, bad_to_bad)
  call profwage(hlogwage, ulogwage, hhgr, hugr, uhgr, uugr)
 
  call make_A(Astate)
  call make_H(Hstate)
  call make_W(Wstate)
  call make_AIME(AIMEstate)
  
  p_gamh = 0.593194_8
  p_gamc = 3.51206_8
  !p_gamc = 1.0_8
  p_leispref = 4762.64_8
  p_leisprefbad = 258.115_8
  p_fixcost = 928.774_8
  p_beta = 0.984991_8
  p_bequest = 0.0255898_8
  p_conspref = 10000000.0_8
  p_beqk = 500000.0_8
  p_onemgamc = 1.0_8
  if (p_gamc/=1.0_8) then
     p_onemgamc = 1.0_8/(1.0_8-p_gamc)
  end if
    p_onemgamh = 1.0_8
  if (p_gamh/=1.0_8) then
     p_onemgamh = 1.0_8/(1.0_8-p_gamh)
  end if
  nonsep = 1_1
  tiedwage = 0_8

  open(unit=15, file='valuesopt.csv')
  write(15,"(A)") "age, M, A, Aindex, AIME, AIMEindex, Wage, Windex, Aopt, Copt, Hopt, value"
  do Ai = 1, Anum
     do AIMEi = 1, AIMEnum
     call optmum3_analytical(Astate(Ai), AIMEstate(AIMEi), 0.0_8, Copt, Aopt, valopt)
     Vgood(95_8-bornage+1_8, AIMEi, 1_8, Ai)=valopt
     optC_good(95-bornage+1_8, AIMEi, 1_8, Ai)=Copt
     optA_good(95-bornage+1_8, AIMEi, 1_8, Ai)=Aopt
     optH_good(95-bornage+1_8, AIMEi, 1_8, Ai)=0.0_8
     write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
          95, ',', 0.0_8, ',', Astate(Ai), ',', Ai, ',', AIMEstate(AIMEi), ',', AIMEi, ',',  0.0_8, ',', 1_8, ',', Aopt, ',', Copt, ',', 0.0_8, ',', valopt

     call optmum3_analytical(Astate(Ai), AIMEstate(AIMEi), 1.0_8, Copt, Aopt, valopt)
     Vbad(95_8-bornage+1_8, AIMEi, 1_8, Ai)=valopt
     optC_bad(95-bornage+1_8, AIMEi, 1_8, Ai)=Copt
     optA_bad(95-bornage+1_8, AIMEi, 1_8, Ai)=Aopt
     optH_bad(95-bornage+1_8, AIMEi, 1_8, Ai)=0.0_8
     write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a, f18.10)')&
          95, ',', 1.0_8, ',',  Astate(Ai), ',', Ai, ',', AIMEstate(AIMEi), ',', AIMEi, ',',  0.0_8, ',', 1_8, ',', Aopt, ',', Copt, ',', 0.0_8, ',', valopt

     do Wi = 2, Wnum
        Vgood(95_8-bornage+1_8, AIMEi, Wi, Ai) = Vgood(95_8-bornage+1, AIMEi, 1_8, Ai)
        optC_good(95_8-bornage+1_8, AIMEi, Wi, Ai) = optC_good(95_8-bornage+1, AIMEi, 1_8, Ai)
        optA_good(95_8-bornage+1_8, AIMEi, Wi, Ai) = optA_good(95_8-bornage+1, AIMEi, 1_8, Ai)     
        optH_good(95_8-bornage+1_8, AIMEi, Wi, Ai) = optH_good(95_8-bornage+1, AIMEi, 1_8, Ai)
        Vbad(95_8-bornage+1_8, AIMEi, Wi, Ai) = Vbad(95_8-bornage+1, AIMEi, 1_8, Ai)
        optC_bad(95_8-bornage+1_8, AIMEi, Wi, Ai) = optC_bad(95_8-bornage+1, AIMEi, 1_8, Ai)
        optA_bad(95_8-bornage+1_8, AIMEi, Wi, Ai) = optA_bad(95_8-bornage+1, AIMEi, 1_8, Ai)     
        optH_bad(95_8-bornage+1_8, AIMEi, Wi, Ai) = optH_bad(95_8-bornage+1, AIMEi, 1_8, Ai)
     end do
  end do !AIME loop
end do !A loop

  
do age = dieage-1, etstage, -1 !age 94-70
   write(*,*) age
   do Ai = 1, Anum
      do AIMEi = 1, AIMEnum
         call optmum2(age, Astate(Ai), AIMEstate(AIMEi), 0.0_8, Vgood, Vbad, Astate, Wstate, AIMEstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Copt, Aopt, valopt)
         Vgood(age-bornage+1_8, AIMEi, 1_8, Ai) = valopt
         optC_good(age-bornage+1_8, AIMEi, 1_8, Ai)=Copt
         optA_good(age-bornage+1_8, AIMEi, 1_8, Ai)=Aopt
         optH_good(age-bornage+1_8, AIMEi, 1_8, Ai)=0.0_8
         write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a, f18.10)')&
              age, ',', 0.0_8, ',', Astate(Ai), ',', Ai, ',', AIMEstate(AIMEi), ',', AIMEi, ',',  0.0_8, ',', 1_8, ',', Aopt, ',', Copt, ',', 0.0_8, ',', valopt

         call optmum2(age, Astate(Ai), AIMEstate(AIMEi), 1.0_8, Vgood, Vbad, Astate, Wstate, AIMEstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, Copt, Aopt, valopt)
         Vbad(age-bornage+1_8, AIMEi, 1_8, Ai) = valopt
         optC_bad(age-bornage+1_8, AIMEi, 1_8, Ai)=Copt
         optA_bad(age-bornage+1_8, AIMEi, 1_8, Ai)=Aopt
         optH_bad(age-bornage+1_8, AIMEi, 1_8, Ai)=0.0_8
         write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a, f18.10)')&
              age, ',', 1.0_8, ',', Astate(Ai), ',', Ai, ',', AIMEstate(AIMEi), ',', AIMEi, ',',  0.0_8, ',', 1_8, ',', Aopt, ',', Copt, ',', 0.0_8, ',', valopt

         do Wi = 2, Wnum
            Vgood(95_8-bornage+1_8, AIMEi, Wi, Ai) = Vgood(95_8-bornage+1, AIMEi, 1_8, Ai)
            optC_good(95_8-bornage+1_8, AIMEi, Wi, Ai) = optC_good(95_8-bornage+1, AIMEi, 1_8, Ai)
            optA_good(95_8-bornage+1_8, AIMEi, Wi, Ai) = optA_good(95_8-bornage+1, AIMEi, 1_8, Ai)     
            optH_good(95_8-bornage+1_8, AIMEi, Wi, Ai) = optH_good(95_8-bornage+1, AIMEi, 1_8, Ai)
            Vbad(95_8-bornage+1_8, AIMEi, Wi, Ai) = Vbad(95_8-bornage+1, AIMEi, 1_8, Ai)
            optC_bad(95_8-bornage+1_8, AIMEi, Wi, Ai) = optC_bad(95_8-bornage+1, AIMEi, 1_8, Ai)
            optA_bad(95_8-bornage+1_8, AIMEi, Wi, Ai) = optA_bad(95_8-bornage+1, AIMEi, 1_8, Ai)     
            optH_bad(95_8-bornage+1_8, AIMEi, Wi, Ai) = optH_bad(95_8-bornage+1, AIMEi, 1_8, Ai)
         end do
      end do ! AIME loop
   end do ! A loop
end do ! age loop

do age = etstage-1, penage, -1 !age 69-62
   write(*,*) age
   do Wi = 1, Wnum
      do Ai = 1, Anum
         do AIMEi = 1, AIMEnum

            call optmum1(age, Astate(Ai), AIMEstate(AIMEi), Wstate(Wi), 0.0_8, Vgood, Vbad, Astate, AIMEstate, Wstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                 hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, Copt, Hopt, Aopt, valopt)
            Vgood(age-bornage+1_8, AIMEi, Wi, Ai) = valopt
            optC_good(age-bornage+1_8, AIMEi, Wi, Ai)=Copt
            optA_good(age-bornage+1_8, AIMEi, Wi, Ai)=Aopt
            optH_good(age-bornage+1_8, AIMEi, Wi, Ai)=Hopt
            write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a, f18.10)')&
                 age, ',', 0.0_8, ',', Astate(Ai), ',', Ai, ',', AIMEstate(AIMEi), ',', AIMEi, ',', Wstate(Wi), ',', Wi, ',', Aopt, ',', Copt, ',', Hopt, ',', valopt

            call optmum0(age, Astate(Ai), AIMEstate(AIMEi), Wstate(Wi), 1.0_8, Vgood, Vbad, Astate, AIMEstate, Wstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                 hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, Copt, Hopt, Aopt, valopt)
            Vbad(age-bornage+1_8, AIMEi, Wi, Ai) = valopt
            optC_bad(age-bornage+1_8, AIMEi, Wi, Ai)=Copt
            optA_bad(age-bornage+1_8, AIMEi, Wi, Ai)=Aopt
            optH_bad(age-bornage+1_8, AIMEi, Wi, Ai)=Hopt
            write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
                 age, ',', 1.0_8, ',', Astate(Ai), ',', Ai, ',', AIMEstate(AIMEi), ',', AIMEi, ',', Wstate(Wi), ',', Wi, ',', Aopt, ',', Copt, ',', Hopt, ',', valopt
         end do!AIME loop
      end do!A loop
   end do !W loop
end do !age loop
  
do age = penage-1, bornage, -1 !age 61-30
   write(*,*) age
   do Wi = 1, Wnum
      do Ai = 1, Anum
         do AIMEi = 1, AIMEnum      
            call optmum0(age, Astate(Ai), AIMEstate(AIMEi), Wstate(Wi), 0.0_8, Vgood, Vbad, Astate, AIMEstate, Wstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                 hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, Copt, Hopt, Aopt, valopt)
            Vgood(age-bornage+1_8, AIMEi, Wi, Ai) = valopt
            optC_good(age-bornage+1_8, AIMEi, Wi, Ai)=Copt
            optA_good(age-bornage+1_8, AIMEi, Wi, Ai)=Aopt
            optH_good(age-bornage+1_8, AIMEi, Wi, Ai)=Hopt
            write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
                 age, ',', 0.0_8, ',', Astate(Ai), ',', Ai, ',', AIMEstate(AIMEi), ',', AIMEi, ',', Wstate(Wi), ',', Wi, ',', Aopt, ',', Copt, ',', Hopt, ',', valopt

            call optmum0(age, Astate(Ai), AIMEstate(AIMEi), Wstate(Wi), 1.0_8, Vgood, Vbad, Astate, AIMEstate, Wstate, Hstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                 hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, Copt, Hopt, Aopt, valopt)
            Vbad(age-bornage+1_8, AIMEi, Wi, Ai) = valopt
            optC_bad(age-bornage+1_8, AIMEi, Wi, Ai)=Copt
            optA_bad(age-bornage+1_8, AIMEi, Wi, Ai)=Aopt
            optH_bad(age-bornage+1_8, AIMEi, Wi, Ai)=Hopt
            write(15,'(i2, a, f4.2, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, i3, a, f10.2, a, f10.2, a, f10.2, a, f18.10)') &
                 age, ',', 1.0_8, ',', Astate(Ai), ',', Ai, ',', AIMEstate(AIMEi), ',', AIMEi, ',', Wstate(Wi), ',', Wi, ',', Aopt, ',', Copt, ',', Hopt, ',', valopt
         end do !AIME loop
      end do ! A loop
   end do !W loop
end do !age loop
  
  close(15)

  sim_length = 10000_8
  allocate(A_dist(sim_length))
  allocate(M_dist(sim_length))
  allocate(W_dist(sim_length))
  allocate(AIME_dist(sim_length))
  
  open(unit=87, file='sim_A_dist.csv')
  do j=1, sim_length
     read(87,*) A_dist(j)
  end do
  close(87)

  open(unit=88, file='sim_M_dist.csv')
  do j=1, sim_length
     read(88,*) M_dist(j)
  end do
  close(88)

  open(unit=89, file='sim_W_dist.csv')
  do j=1, sim_length
     read(89,*) W_dist(j)
  end do
  close(89)

  open(unit=90, file='sim_AIME_dist.csv')
  do j=1, sim_length
     read(90,*) AIME_dist(j)
  end do
  close(90)
  
  call simulation_mean(A_dist, M_dist, W_dist, AIME_dist, mortality_good, mortality_bad, good_to_bad, bad_to_bad, hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, &
       optC_good, optC_bad, optA_good, optA_bad, optH_good, optH_bad, Astate, Wstate, AIMEstate, prof_C_good, prof_C_bad, prof_A_good, prof_A_bad, prof_H_good, prof_H_bad) 
  
end program main


