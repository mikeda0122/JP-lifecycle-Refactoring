local workdir "C:\Users\Masato\Dropbox\RA_workplace\French_rep\fortran_codes\Simple_models\French_finishing_touch_v7"
*local workdir "C:\Users\masatike\Dropbox\RA_workplace\french_rep\fortran_codes\Simple_models\French_finishing_touch_v6"
*local workdir "C:\Users\Masato\Dropbox\RA_workplace\French_rep\fortran_codes\result_comparison\result_data\result1204"

clear
/*
set obs 7000

gen random = runiform(0,1)
gen M_dist = 1 if random<0.01
replace M_dist = 0 if random>=0.01
drop random

*gen A_dist = rnormal(1000, 250)
gen A_health = rnormal(40000, 5000)
gen A_unhealth = rnormal(40000, 5000)

gen A_dist = A_health if M_dist==0
replace A_dist = A_unhealth if M_dist==1

gen AIME_health = rnormal(3000, 500)
gen AIME_unhealth = rnormal(3000, 500)

gen AIME_dist = AIME_health if M_dist==0
replace AIME_dist = AIME_unhealth if M_dist==1

preserve

keep A_dist
*export delimited using "C:\Users\Masato\Dropbox\RA_workplace\French_rep\fortran_codes\optimization_with_simulation\sim_dist.csv", novar replace
export delimited using "`workdir'\sim_A_dist.csv", novar replace

restore

keep M_dist
export delimited using "`workdir'\sim_M_dist.csv", novar replace

clear

set obs 10000

gen W_dist = rnormal(10,7)

gen AIME_dist = W_dist*214
preserve

keep AIME_dist
*export delimited using "C:\Users\Masato\Dropbox\RA_workplace\French_rep\fortran_codes\optimization_with_simulation\sim_dist.csv", novar replace
export delimited using "`workdir'\sim_AIME_dist.csv", novar replace

restore

export delimited using "`workdir'\sim_W_dist.csv", novar replace

clear

set obs 66*7000

gen W_shock = rnormal(0,1)

export delimited using "`workdir'\wage_shock.csv", novar replace

clear
*/
*import delimited using "`workdir'\simulated_prof_ind.csv"
*import delimited using "`workdir'\simulated_prof.csv"
import delimited using "`workdir'\valuesopt.csv"

collapse (mean) aopt copt hopt bopt nextaimeopt, by(age m)
twoway (line bopt age if m==1) (line bopt age if m==0, xline(65))
xx
*twoway (line hopt age if m==1) (line hopt age if m==0)
*twoway (line aopt age if m==1) (line aopt age if m==0)
*twoway (line copt age if m==1) (line copt age if m==0)
*twoway (line nextaimeopt age if m==1) (line nextaimeopt age if m==0, xline(60 61 62))

/*
twoway (line h_good age) (line h_bad age) (line h age)
twoway (line h_work_good age) (line h_work_bad age) (line h_work age)
twoway (line p_good age) (line p_bad age)
twoway (line c_good age) (line c_bad age) (line c age)
twoway (line b_good age) (line b_bad age) if age>=61
twoway (line b age) if age >=61
twoway (line a_good age) (line a_bad age)
twoway (line w_good age) (line w_bad age)
*/
twoway (line h_good age) (line h_bad age) (line h age)
xx
twoway (line h_work_good age) (line h_work_bad age) (line h_work age)
twoway (line w_good age) (line w_bad age)

twoway line aime age
xx
twoway (line i age) (line pb age) (line ss age)
twoway (line pb_good age) (line pb_bad age)
twoway (line ss_good age) (line ss_bad age)
xx
gen Wnum = 10
gen Hnum = 7
gen Anum = 30
gen AIMEnum = 10
gen Cnum = 180
*gen pen_start = 65
gen last = "gsearch"
*gen integral = "French"
gen integral="mean+random"
*gen order = "Bi loop -> ss cal -> Hi loop"

save `workdir'\simulated_prof_fixed_ver2.dta, replace
