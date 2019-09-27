（_v8からの変形、さらにそれを
元々はconsumption gridなどの微調整をしたもの）

（最終的な目標はconsumption gridとwage meanの調整、可能であれば調整後のconsumption gridでlocal searchのinitial valueを追加）

（v11からEstimationの土台へcopy
simulation部分はいじってないのでsimulation profileは一致するはず
Data profileはまだ）

2019/4/11
mod_simulation_prof.f90の75行目あたりでwage_shock.csvがcloseされていないのを修正

2019/4/26
wage_shockを標準正規分布に差し替えた。今までは分散=stdの正規分布を使っていたために分散が２回かかかっていた。

2019/5/17
simulationをFrench_finishing_touch_v19に合わせた。

2019/5/19
Optimal weightの計算を追加した
Parameter(std), data(wage, initial value)を調整した

2019/5/24
mod_gmm.f90 L190あたりの配列番号
mod_utility.f90 例外処理追加 主にmod_computeval1.f90(グリッド決める時)のため

mod_ageshifter.f90, mod_makegrids.f90, mod_sprob.f90  iosはinteger(4)でないといけないらしい-->integer (番号指定なし)で定義した

mod_pension.f90 L75 comment out

mod_parameter.f90 pa* pb*のパラメータを倍精度にした

mod_makegrids.f90 配列を作る時の[***] => (/***/)

mod_sprob.f90 i_iをinteger(8)に、ageがunused

2019/5/25
mod_optmum1.f90:
PGF90-I-0035-Predefined intrinsic maxval loses intrinsic property (mod_optmum1.f90: 366)
------> maxvalue

mod_simulation_prof.f90:
PGF90-I-0035-Predefined intrinsic index loses intrinsic property (mod_simulation_prof.f90: 536)
PGF90-I-0035-Predefined intrinsic index loses intrinsic property (mod_simulation_prof.f90: 565)
------> indx

mod_readmom.f90:
PGF90-I-0035-Predefined intrinsic len loses intrinsic property (mod_readmom.f90: 68)
------>length

mod_doquant.f90:
PGF90-I-0035-Predefined intrinsic loc loses intrinsic property (mod_doquant.f90: 70)
----->location

mod_parameter.f90:
PGF90-I-0035-Predefined intrinsic anint loses intrinsic property (mod_parameter.f90: 132)
PGF90-I-0035-Predefined intrinsic aint loses intrinsic property (mod_parameter.f90: 137)
------> A to As (e.g.Aint to Asint)

mod_makegrids.f90:
PGF90-I-0035-Predefined intrinsic anint loses intrinsic property (mod_makegrids.f90: 16)
PGF90-W-0103-Type conversion of subscript expression for astate (mod_makegrids.f90: 16)
PGF90-I-0035-Predefined intrinsic aint loses intrinsic property (mod_makegrids.f90: 16)
PGF90-W-0103-Type conversion of subscript expression for astate (mod_makegrids.f90: 17)
PGF90-W-0103-Type conversion of subscript expression for astate (mod_makegrids.f90: 17)
PGF90-S-0038-Symbol, anint, has not been explicitly declared (mod_makegrids.f90)
PGF90-S-0038-Symbol, amin, has not been explicitly declared (mod_makegrids.f90)
PGF90-S-0038-Symbol, aint, has not been explicitly declared (mod_makegrids.f90)
PGF90-S-0038-Symbol, anum, has not been explicitly declared (mod_makegrids.f90)
PGF90-S-0038-Symbol, amax, has not been explicitly declared (mod_makegrids.f90)
  2 inform,   3 warnings,   5 severes, 0 fatal for make_a

mod_ass.f90:
PGF90-S-0038-Symbol, amax, has not been explicitly declared (mod_ass.f90)
  0 inform,   0 warnings,   1 severes, 0 fatal for ass

mod_integral.f90:
PGF90-S-0188-Argument number 9 to interp: type mismatch (mod_integral.f90: 49)
PGF90-S-0038-Symbol, anum, has not been explicitly declared (mod_integral.f90)
  0 inform,   0 warnings,   2 severes, 0 fatal for integral

mod_optmum2.f90:
PGF90-S-0188-Argument number 9 to interp: type mismatch (mod_optmum2.f90: 91)
PGF90-S-0188-Argument number 9 to interp: type mismatch (mod_optmum2.f90: 92)
PGF90-S-0038-Symbol, anum, has not been explicitly declared (mod_optmum2.f90)
  0 inform,   0 warnings,   3 severes, 0 fatal for optmum2

mod_simulation_prof.f90:
PGF90-S-0038-Symbol, anum, has not been explicitly declared (mod_simulation_prof.f90)
  0 inform,   0 warnings,   1 severes, 0 fatal for simulation_prof
PGF90-S-0038-Symbol, anum, has not been explicitly declared (mod_simulation_prof.f90)
  0 inform,   0 warnings,   1 severes, 0 fatal for trac_lifecycle

mod_decrule.f90:
PGF90-S-0038-Symbol, anum, has not been explicitly declared (mod_decrule.f90)
  0 inform,   0 warnings,   1 severes, 0 fatal for decrule

mod_gmm.f90:
PGF90-S-0038-Symbol, anum, has not been explicitly declared (mod_gmm.f90)
  0 inform,   0 warnings,   1 severes, 0 fatal for gmm

main_run.f90:
PGF90-S-0038-Symbol, anum, has not been explicitly declared (main_run.f90)
  0 inform,   0 warnings,   1 severes, 0 fatal for main_run


************************************
change length of arrays (such as mean_prof_H_work_bad) in mod_gmm.f90 around L178

add leisure<=0 case to mod_utility.f90 as an exception.

mod_readmom.f90
change typo weight(i,225) to weight(i,255)


mod_utility.f90 U=vpanish beq=vpanish

mod_computeaftertaxincome.f90 computeAfterTaxIncome = 0.0_8 in "else"

mod_interp.f90 initialize *idx and add check for them

mod_optmum2.f90 Evtpo=0.0_8 in "else"

mod_optmum1.f90
allocate(Hstate(1))=0.0_8 Hstate(1)=0.0_8 in "else"
initialize all the temporary variables such as lazy_Copt
initialize initHi and add check for it

mod_optmum0.f90
allocate(Hstate(1))=0.0_8 Hstate(1)=0.0_8 in "else"
initialize all the temporary variables such as lazy_Copt
initialize initHi and add check for it

mod_readmom.f90
add totobs=1_8 in "else"
use weight_data(:,1:momnum) even when the size of VCV2 and that of weight_date coincide
change typo weight(i,225) to weight(i,255)
add allocate in "else" for two_step dummy
define i, j, length, totobs as integer(8)

mod_simulation_prof.f90 death_age=death_age+1_8

mod_makegrids.f90 AIMEgridsの後ろ半分の配列を作る時i=1, AIMEnintでimplicit doを回していたのをi=1, AIMEnum-AIMEnintにした

2019/5/26
Hongさんが修正済みのもの(gfortran_v1)をサーバー上のpgi等のデバッグを統合し、GNUの導入を行った(gfortran_v2)
test_GNU_min.f90: 確認ずみ

2019/5/30
_v8から
profwageを修正し、v6と同様のグラフが書けることまで確認ずみ
Fgsl関連のモジュールや表現を取り除いてpgiで回るようにしたもの

2019/6/6
succesfully implement Estimation with NAG (Although some problem with weight, technically work hour graph)
enable us to use French's optimal weight(vinv)
we can calcurate objective functions for each moments.

2019/6/6
Estimation using only working hour weight.

2019/6/14
Computation of inverse matrix (mod_NAG_pd_sym_inverse.f90)

mod_readmom.f90:
add reading healsim data
add reading deathsim data

mod_simulation_prof.f90
add writing healsim data
add writing deathsim data

mod_gmm.f90
add writing mean sim data

After interpolating prof_P in mod_simulation_prof.f90, we have to make it 0 or 1. 

Computation of optimal matrix.

2019/06/28
Fix wage shock by eliminating sqrt(2.0_8)

2019/07/03
Renew gvec.csv

2019/7/4
Add random generation of initial value with loop.

2019/07/31

cutting off "pension" ("ss" corresponds to pension system in JAPAN)
mod_simulation_prof.f90
L328 pb = 0.0_8
L192-L386 commented out
L449 pb = 0.0_8
(same for unhealthy guys
L470 pb = 0.0_8
L489-L527 commented out

mod_optmum3.f90
L43 pb = 0.0_8

mod_optmum2.f90
L50 pb = 0.0_8

mod_optmum1.f90
L135 pb = 0.0_8

mod_computeval1.f90
L96-L102 commented out

mod_computeval0.f90
L68-L104 commented out

taking ss benefit as constant

****Wage Profile
2019/08/01
mod_profwage.f90
add directly name to the file name

mod_parameter.f90
change spousal income coefficients

****Japanese Tax System
Add
mod_JP_computeaftertaxincome.f90
mod_AIME_params.f90

****Japanese Pension System (Except Early Application)
Add HI_payments, HI_payments_elderly, HI_brackets, pension_brackets and pension_payments to the arguments.
mod_gmm.f90, mod_decrule.f90, mod_optmum3_gsearch.f90, mod_optmum2.f90, mod_optmum1.f90, mod_workval1.f90, mod_optmum0.f90, mod_workval0.f90 and mod_simulation_prof.f90

mod_parameter.f90
Add penrate and const_pen.


Change pension system
mod_gmm.f90, mod_decrule.f90, 


mod_optmum1.f90
eliminate PIA

mod_workval1.f90


mod_computeval1.f90
L91 - L95: How to compute next period AIME.

mod_optmum0.f90

mod_simulation_prof.f90

mod_parameter.f90
*max, *min, *int


****Early Application
mod_optmum1.f90 L139-
mod_getadj.f90
mod_parameter.f90 L189-

mod_decrule.f90
L238 age 70-60
L327 age 59-30
L159 age 94-71

mod_ass.f90 Eliminating earnings test from mod_ass.f90
mod_optmum3_gsearch.f90 L71
mod_optmum2.f90 L85
mod_optmum1.90 L64
mod_optmum0.f90 L68

mod_simulation_prof.f90
Add apply.
Change AIME adjustment.


2019/08/26
mod_condmean.f90
add pop_work_* = 1.0_8

test
