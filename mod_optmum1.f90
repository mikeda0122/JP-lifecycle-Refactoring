module mod_optmum1

  use mod_parameter
  !use mod_makegrids
  !use mod_computelaborincome
  !use mod_computeAfterTaxIncome
  !use mod_computeAIME
  !use mod_computePIA
  !use mod_pension
  !use mod_ass
  !use mod_utility
  !use mod_interp
  !use mod_integral
  !use mod_computeval1
  use mod_getadj
  !use mod_sprob
  use mod_workval1

  implicit none

contains

  subroutine optmum1(age, A, AIME, B, W, M, Cinit, Hinit, Vgood, Vbad, Astate, AIMEstate, Wstate, &
    & inCstate, inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
    & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
    & AIMErate, AIMEstd, low_brackets, up_brackets, &
    & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, Copt, Hopt, Aopt, Bopt, &
    & Wopt_good, Wopt_bad, AIMEopt, Iopt, pbopt, ssopt, valopt)

    implicit none

    integer(8), intent(in) :: age
    real(8), intent(in) :: A, AIME, W
    integer(8), intent(in) :: B
    real(8), intent(in) :: M
    real(8), intent(in) :: Cinit, Hinit
    real(8), intent(in) :: Vgood(:,:,:,:,:), Vbad(:,:,:,:,:)
    real(8), intent(in) :: Astate(:) , AIMEstate(:), inHstate(:), Wstate(:), inCstate(:)
    real(8), intent(in) :: HI_payments(:), HI_payments_elderly(:), HI_brackets(:)
    real(8), intent(in) :: pension_brackets(:), pension_payments(:)
    real(8), intent(in) :: AIMErate(:), AIMEstd(:), low_brackets(:), up_brackets(:)
    real(8), intent(in) :: mortality_good(:), mortality_bad(:), good_to_bad(:), bad_to_bad(:)
    real(8), intent(in) :: hlogwage(:), ulogwage(:), hhgr(:), hugr(:), uhgr(:), uugr(:)
    real(8), intent(in) :: gvec(:), ageshift(:)

    real(8), intent(out) :: valopt, Copt, Hopt, Aopt, Wopt_good, Wopt_bad, AIMEopt, Iopt, pbopt, ssopt
    integer(8), intent(out) :: Bopt

    integer(8) :: Ci, initHi, Hlen
    integer(8) :: Hi, i, Wi, Bi, lfpi

    real(8) :: C, Cmin, Cmax
    real(8), allocatable :: Hstate(:), p_Hstate(:), m_Hstate(:)
    real(8) :: H
    integer(8) :: currentB
    real(8) :: cumadj2, bigcred, cumeretadj, eretadj, lretadj
    integer(1) :: particip, apply
    real(8) :: PIA, ss, ss2, pb, laborincome, income, cashonhand, adjAIME
    real(8) :: penacc1, penacc2, nextpenbenpred, penbenpred, nextPIA
    real(8) :: tempnextAIME, tempnextPIA, earlyretirement, makeadjust
    real(8) :: MTR, reduc
    real(8) :: borrowamount, nextperiodassets, nextperiodAIME, utils, bequestutils
    real(8) :: wtpogood, wtpobad
    real(8) :: Evtgood, Evtbad, Evtpo
    real(8) :: valp, valpp, valm, valmm, val, maxvalue

    real(8) :: temp_valopt, temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad,&
    & temp_AIMEopt, temp_Iopt, temp_pbopt, temp_ssopt
    integer(8) :: temp_Bopt
    real(8) :: app_valopt, app_Copt, app_Hopt, app_Aopt, app_Wopt_good, app_Wopt_bad, app_AIMEopt,&
    & app_Iopt, app_pbopt, app_ssopt
    integer(8) :: app_Bopt
    real(8) :: app_lazy_valopt, app_lazy_Copt, app_lazy_Hopt, app_lazy_Aopt, app_lazy_Wopt_good, &
    & app_lazy_Wopt_bad, app_lazy_AIMEopt, app_lazy_Iopt, app_lazy_pbopt, app_lazy_ssopt
    integer(8) :: app_lazy_Bopt
    real(8) :: lazy_valopt, lazy_Copt, lazy_Hopt, lazy_Aopt, lazy_Wopt_good, lazy_Wopt_bad, lazy_AIMEopt, &
    & lazy_Iopt, lazy_pbopt, lazy_ssopt
    integer(8) :: lazy_Bopt

    integer(8) :: flag = 0_8
    integer(8) :: intval = 1_8

    real(8) :: plot_val

    valopt = vpanish

    app_Copt = -1.0_8
    app_Hopt = 0.0_8
    app_Aopt = 0.0_8
    app_Bopt = 0.0_8
    app_Wopt_good = 0.0_8
    app_Wopt_bad = 0.0_8
    app_AIMEopt = 0.0_8
    app_Iopt = 0.0_8
    app_pbopt = 0.0_8
    app_ssopt = 0.0_8
    app_valopt = vpanish

    app_lazy_Copt = -1.0_8
    app_lazy_Hopt = 0.0_8
    app_lazy_Aopt = 0.0_8
    app_lazy_Bopt = 0.0_8
    app_lazy_Wopt_good = 0.0_8
    app_lazy_Wopt_bad = 0.0_8
    app_lazy_AIMEopt = 0.0_8
    app_lazy_Iopt = 0.0_8
    app_lazy_pbopt = 0.0_8
    app_lazy_ssopt = 0.0_8
    app_lazy_valopt = vpanish

    lazy_Copt = -1.0_8
    lazy_Hopt = 0.0_8
    lazy_Aopt = 0.0_8
    lazy_Bopt = 0.0_8
    lazy_Wopt_good = 0.0_8    
    lazy_Wopt_bad = 0.0_8    
    lazy_AIMEopt = 0.0_8
    lazy_Iopt =  0.0_8
    lazy_pbopt = 0.0_8
    lazy_ssopt = 0.0_8
    lazy_valopt = vpanish

    do Bi = 0, 1

       if(B == 1_8) then
          currentB = B
          apply = 0_8
       else
          currentB = Bi
          if (currentB==1_8) then
             apply = 1_8
          else
             apply = 0_8
          end if
       end if

       call getadj(age, eretadj, lretadj)
       
       adjAIME = AIME
       if (apply==1_8) then
          if (age < penage) then
             adjAIME = adjAIME*(1.0_8-eretadj)
          else if (age==penage) then
             adjAIME = adjAIME
          else if (penage < age) then
             adjAIME = adjAIME*(1.0_8+lretadj)
          end if
       end if

       ss = currentB*adjAIME

       pb = 0.0_8 !predictpensionbenefits(PIA, age)

       do lfpi = 0, 1
          if (lfpi==0_8) then
             particip = 0_1
             H = 0.0_8

             if (nonsep==1_1) then
                if (p_leispref - H - ((p_fixcost*particip) + (p_leisprefbad*M))<=0) then
                   cycle
                end if
             else
                if (5280.0_8 - H - ((p_fixcost*particip) + (p_leisprefbad*M))<=0) then
                   cycle
                end if
             end if

             call workval1(age, A, adjAIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate, &
             & inCstate, inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
             & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
             & AIMErate, AIMEstd, low_brackets, up_brackets, &
             & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, &
             & H, currentB, PIA, pb, ss, particip, cumadj2, eretadj, bigcred, cumeretadj, lretadj, &
             & temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, &
             & temp_AIMEopt, temp_Iopt, temp_pbopt, temp_ssopt, temp_valopt)

             if (currentB==0) then
                lazy_Copt = temp_Copt
                lazy_Hopt = temp_Hopt
                lazy_Aopt = temp_Aopt
                lazy_Bopt = currentB
                lazy_Wopt_good = temp_Wopt_good
                lazy_Wopt_bad = temp_Wopt_bad
                lazy_AIMEopt = temp_AIMEopt
                lazy_Iopt = temp_Iopt
                lazy_pbopt = temp_pbopt
                lazy_ssopt = temp_ssopt
                lazy_valopt = temp_valopt
             else if (currentB==1) then
                app_lazy_Copt = temp_Copt
                app_lazy_Hopt = temp_Hopt
                app_lazy_Aopt = temp_Aopt
                app_lazy_Bopt = currentB
                app_lazy_Wopt_good = temp_Wopt_good
                app_lazy_Wopt_bad = temp_Wopt_bad
                app_lazy_AIMEopt = temp_AIMEopt
                app_lazy_Iopt = temp_Iopt
                app_lazy_pbopt = temp_pbopt
                app_lazy_ssopt = temp_ssopt
                app_lazy_valopt = temp_valopt
             end if
          else if (lfpi==1_8) then

             particip = 1_1

             if (intval==1_8) then

                if (Hinit<inHstate(2)) then
                   initHi = 1
                   Hlen = size(inHstate)
                   allocate(Hstate(Hlen))
                   Hstate = inHstate
                else if (Hinit>=inHstate(Hnum-1)) then
                   initHi = Hnum
                   Hlen = size(inHstate)
                   allocate(Hstate(Hlen))
                   do i = Hnum, 1, -1
                      Hstate(Hnum-i+1) = inHstate(i)
                   end do
                else if (inHstate(2)<=Hinit .and. Hinit<inHstate(Hnum-1)) then
                   initHi = Hnum+1_8

                   do i = 2, Hnum-1
                      if (inHstate(i)<=Hinit .and. Hinit<inHstate(i+1)) then
                         initHi = i
                         exit
                      end if
                   end do

                   if (initHi==Hnum+1_8) then
                      write(*,*) 'something is wrong with initHi in optmum1'
                   end if
                   
                   call workval1(age, A, adjAIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate, &
                   & inCstate, inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                   & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
                   & AIMErate, AIMEstd, low_brackets, up_brackets, &
                   & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, inHstate(initHi+1), currentB,&
                   &  PIA, pb, ss, particip, cumadj2, eretadj, bigcred, cumeretadj, lretadj, &
                   &temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, &
                   & temp_AIMEopt, temp_Iopt, temp_pbopt, temp_ssopt, valp)
                   if (initHi<=Hnum-2) then
                      call workval1(age, A, adjAIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate, &
                      & inCstate, inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                      & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
                      & AIMErate, AIMEstd, low_brackets, up_brackets, &
                      & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, inHstate(initHi+2),&
                      & currentB, PIA, pb, ss, particip, cumadj2, eretadj, bigcred, cumeretadj, lretadj, &
                      & temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, temp_AIMEopt,&
                      & temp_Iopt, temp_pbopt, temp_ssopt, valpp)
                   else if (initHi==Hnum-1) then
                      valpp = vpanish
                   else
                      write(*,*) 'something is wrong with initHi!!'
                   end if

                   call workval1(age, A, adjAIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate,&
                   & inCstate, inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                   & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
                   & AIMErate, AIMEstd, low_brackets, up_brackets, &
                   & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, inHstate(initHi), &
                   & currentB, PIA, pb, ss, particip, cumadj2, eretadj, bigcred, cumeretadj, lretadj, &
                   & temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, temp_AIMEopt, &
                   & temp_Iopt, temp_pbopt, temp_ssopt, valm)
                   call workval1(age, A, adjAIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate,&
                   & inCstate, inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                   & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
                   & AIMErate, AIMEstd, low_brackets, up_brackets, &
                   & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, inHstate(initHi-1), &
                   & currentB, PIA, pb, ss, particip, cumadj2, eretadj, bigcred, cumeretadj, lretadj, &
                   & temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, temp_AIMEopt, &
                   & temp_Iopt, temp_pbopt, temp_ssopt, valmm)

                   allocate(p_Hstate(Hnum-initHi))
                   allocate(m_Hstate(initHi))

                   do i = initHi+1, Hnum
                      p_Hstate(i-initHi) = inHstate(i)
                   end do

                   do i = 1, initHi
                      m_Hstate(i) = inHstate(initHi-i+1)
                   end do

                   if (valmm>=valm)then
                      Hlen = size(m_Hstate)
                      allocate(Hstate(Hlen))
                      Hstate = m_Hstate
                   else if (valp>=valm) then
                      Hlen = size(p_Hstate)
                      allocate(Hstate(Hlen))
                      Hstate = p_Hstate
                   else if (valm>valp .and. valm>valmm) then
                      Hlen = size(m_Hstate)
                      allocate(Hstate(Hlen))
                      Hstate = m_Hstate
                      !                else if (valm>valp) then
                      !                   val = valm
                      !                   Hlen = 1_8
                      !                   allocate(Hstate(Hlen))
                      !                   Hstate(1) = inHstate(initHi-1)
                   else
                      write(*,*) 'Something is wrong with optmum1!!!'
                      write(*,*) 'valpp=', valpp
                      write(*,*) 'valp=', valp
                      write(*,*) 'valm=', valm
                      write(*,*) 'valmm=', valmm
                      allocate(Hstate(1))
                      Hstate(1) = 0.0_8
                      !read*
                   end if

                   !write(*,*) 'valpp=', valpp
                   !write(*,*) 'valp=', valp
                   !write(*,*) 'valm=', valm
                   !write(*,*) 'valmm=', valmm

                   !if (Hlen==size(m_Hstate)) then
                   !   write(*,*) 'Going down!!'
                   !else if (Hlen==size(p_Hstate)) then
                   !   write(*,*) 'Going up!!'
                   !else
                   !   write(*,*) 'Something is wrong!!'
                   !end if

                   !open(unit=37, file='Hval.csv')
                   !write(37, "(A)") "Hi, H, val"
                   !do Hi = 1, Hnum
                   !   call workval1(age, A, AIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate, inCstate, inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                   !        hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, &
                   !        inHstate(Hi), currentB, PIA, pb, ss, particip, cumadj2, eretadj, bigcred, cumeretadj, lretadj, &
                   !       temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, temp_AIMEopt, temp_Iopt, temp_pbopt, temp_ssopt, plot_val)
                   !   write(37, '(i3, a, f18.5, a, f20.10)') Hi, ',', inHstate(Hi), ',', plot_val
                   !end do
                   !close(37)
                   !flag = 1_8
                   !read*
                else
                   write(*,*) 'error in mod_optmum1.f90'
                   allocate(Hstate(1))
                   Hstate(1) = 0.0_8
                end if

             else
                initHi = Hnum
                Hlen = size(inHstate)
                allocate(Hstate(Hlen))
                Hstate = inHstate
             end if

             do Hi = 1, Hlen

                H = Hstate(Hi)

                if (nonsep==1_1) then
                   if (p_leispref - H - ((p_fixcost*particip) + (p_leisprefbad*M))<=0) then
                      cycle
                   end if
                else
                   if (5280.0_8 - H - ((p_fixcost*particip) + (p_leisprefbad*M))<=0) then
                      cycle
                   end if
                end if

                call workval1(age, A, adjAIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate, &
                & inCstate, inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
                & HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
                & AIMErate, AIMEstd, low_brackets, up_brackets, &
                & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, &
                & H, currentB, PIA, pb, ss, particip, cumadj2, eretadj, bigcred, cumeretadj, lretadj, &
                & temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, temp_AIMEopt, &
                & temp_Iopt, temp_pbopt, temp_ssopt, temp_valopt)

                if (currentB==0 .and. temp_valopt>valopt) then
                   Copt = temp_Copt
                   Hopt = temp_Hopt
                   Aopt = temp_Aopt
                   Bopt = currentB
                   Wopt_good = temp_Wopt_good
                   Wopt_bad = temp_Wopt_bad
                   AIMEopt = temp_AIMEopt
                   Iopt = temp_Iopt
                   pbopt = temp_pbopt
                   ssopt = temp_ssopt
                   valopt = temp_valopt
                   if (valopt>temp_valopt) then
                      exit
                   end if
                else if (currentB==1 .and. temp_valopt>app_valopt) then
                   app_Copt = temp_Copt
                   app_Hopt = temp_Hopt
                   app_Aopt = temp_Aopt
                   app_Bopt = currentB
                   app_Wopt_good = temp_Wopt_good
                   app_Wopt_bad = temp_Wopt_bad
                   app_AIMEopt = temp_AIMEopt
                   app_Iopt = temp_Iopt
                   app_pbopt = temp_pbopt
                   app_ssopt = temp_ssopt
                   app_valopt = temp_valopt
                   if (app_valopt>temp_valopt) then
                      exit
                   end if
                end if

             end do !End Hi loop
             !if (flag==1_8) then
             !   write(*,*) 'Hopt=', Hopt
             !   write(*,*) 'app_Hopt=', app_Hopt
             !   write(*,*) 'valopt=', valopt
             !   write(*,*) 'app_valopt=', app_valopt
             !   call workval1(age, A, AIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate, inCstate, inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
             !        hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, &
             !        Hopt, currentB, PIA, pb, ss, particip, cumadj2, eretadj, bigcred, cumeretadj, lretadj, &
             !        temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, temp_AIMEopt, temp_Iopt, temp_pbopt, temp_ssopt, plot_val)
             !   write(*,*) 'FYI', plot_val
             !   write(*,*) 'Hinint', Hinit

             !   open(unit=38, file='Hval2.csv')
             !   write(38, "(A)") "Hi, H, val"
             !   do Hi = 1, Hnum
             !      call workval1(age, A, AIME, W, Cinit, M, Vgood, Vbad, Astate, AIMEstate, Wstate, inCstate, inHstate, mortality_good, mortality_bad, good_to_bad, bad_to_bad, &
             !           hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, &
             !           inHstate(Hi), currentB, PIA, pb, ss, particip, cumadj2, eretadj, bigcred, cumeretadj, lretadj, &
             !           temp_Copt, temp_Hopt, temp_Aopt, temp_Wopt_good, temp_Wopt_bad, temp_AIMEopt, temp_Iopt, temp_pbopt, temp_ssopt, plot_val)
             !      write(38, '(i3, a, f18.5, a, f20.10)') Hi, ',', inHstate(Hi), ',', plot_val
             !   end do
             !   close(38)

             !   read*
             !end if
             !flag = 0_8
          end if
       end do !End lfpi loop

       if (age>=62 .and. B==1_8) then
          exit
       end if

       if (inHstate(2)<=Hinit .and. Hinit<inHstate(Hnum-1) .and. intval==1_8) then
          deallocate(p_Hstate)
          deallocate(m_Hstate)
          deallocate(Hstate)
       else if (Hinit>=inHstate(Hnum-1) .or. Hinit<inHstate(2) .or. intval==0_8) then
          deallocate(Hstate)
       end if
    end do !End Bi loop

    maxvalue = max(lazy_valopt, app_lazy_valopt, valopt, app_valopt)
    if (maxvalue==lazy_valopt) then
       Copt = lazy_Copt
       Hopt = lazy_Hopt
       Aopt = lazy_Aopt
       Bopt = lazy_Bopt
       Wopt_good = lazy_Wopt_good
       Wopt_bad = lazy_Wopt_bad
       AIMEopt = lazy_AIMEopt
       Iopt = lazy_Iopt
       pbopt = lazy_pbopt
       ssopt = lazy_ssopt
       valopt = lazy_valopt
    else if (maxvalue == app_lazy_valopt) then
       Copt = app_lazy_Copt
       Hopt = app_lazy_Hopt
       Aopt = app_lazy_Aopt
       Bopt = app_lazy_Bopt
       Wopt_good = app_lazy_Wopt_good
       Wopt_bad = app_lazy_Wopt_bad
       AIMEopt = app_lazy_AIMEopt
       Iopt = app_lazy_Iopt
       pbopt = app_lazy_pbopt
       ssopt = app_lazy_ssopt
       valopt = app_lazy_valopt
    else if (maxvalue == app_valopt) then
       Copt = app_Copt
       Hopt = app_Hopt
       Aopt = app_Aopt
       Bopt = app_Bopt
       Wopt_good = app_Wopt_good
       Wopt_bad = app_Wopt_bad
       AIMEopt = app_AIMEopt
       Iopt = app_Iopt
       pbopt = app_pbopt
       ssopt = app_ssopt
       valopt = app_valopt
    end if

    if (Copt==-1.0_8) then
       write(*,*) 'Optimization did not happen in optmum1'
       write(*,*) 'valopt is just "vpanish"'
       write(*,*) 'lazy_valopt, app_lazy_valopt, valopt, app_valopt'
       write(*,*) lazy_valopt, app_lazy_valopt, valopt, app_valopt
    end if
    
  end subroutine optmum1
end module mod_optmum1
