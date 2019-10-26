  call setPoI(params)
  !write(*,*) params
  !write(*,*) 'here', p_conspref, p_leispref-p_fixcost-500.0_8
  !write(*,*) 'p_leispref', p_leispref
  !write(*,*) 'p_fixcost', p_fixcost

  write(*,*) 'params unscaled', p_gamh, p_gamc, p_leispref, p_leisprefbad, p_fixcost, p_beta, p_bequest

  if (p_leispref-p_fixcost-500.0_8<=0.0_8) then
     write(*,*) 'leisure parameter is too low!!'
     !       read*
     return
  end if


  !*************Read and Construct Relevalnt Data Profiles    
  call readAIMEparams(AIMErate, AIMEbrackets, AIMEstd)

  m = size(AIMEbrackets)
  allocate(low_brackets(m+1))
  allocate(up_brackets(m+1))
  call createAIMEbrackets(AIMEbrackets, low_brackets, up_brackets)

  call readTaxparams(HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments)
  call health_mortality_2(mortality_good, mortality_bad, good_to_bad, bad_to_bad)
  call profwage(hlogwage, ulogwage, hhgr, hugr, uhgr, uugr)
  call computeg_2(gvec)
  call ageshifter(ageshift)

  call make_A(Astate)
  call make_W(Wstate)
  call make_AIME(AIMEstate)

  !**************Calculate Decision Rules
  write(*,*) 'Calculating Decision rules...'
  call decrule(Astate, Wstate, AIMEstate, mortality_good, mortality_bad, good_to_bad, &
       & bad_to_bad, HI_payments, HI_payments_elderly, HI_brackets, pension_brackets, pension_payments, &
       & AIMErate, AIMEstd, low_brackets, up_brackets, &
       & hlogwage, ulogwage, hhgr, hugr, uhgr, uugr, gvec, ageshift, &
       & optC_good, optC_bad, optA_good, optA_bad, optH_good, optH_bad, optP_good, optP_bad, &
       & optB_good, optB_bad, optW_good_good, optW_good_bad, optW_bad_good, optW_bad_bad, &
       & optAIME_good, optAIME_bad, optI_good, optI_bad, optpb_good, optpb_bad, optss_good, optss_bad)

end program run_decrule
