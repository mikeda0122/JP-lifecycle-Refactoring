module mod_condmean

  use mod_parameter
  
  implicit none

contains

  subroutine condmean(simdeath, simhealth, simC, simA, simH, simP, simB, simAIME, mean_prof_C, mean_prof_A, mean_prof_H_good, mean_prof_H_bad, mean_prof_H, mean_prof_H_work_good, mean_prof_H_work_bad, mean_prof_H_work, mean_prof_P_good, mean_prof_P_bad, mean_prof_P, mean_prof_B, mean_prof_AIME)

    implicit none
    
    integer(8), intent(in) :: simdeath(:,:)
    real(8), intent(in) :: simhealth(:,:), simC(:,:), simA(:,:), simH(:,:), simP(:,:), simB(:,:), simAIME(:,:)
    real(8), intent(out) :: mean_prof_C(:), mean_prof_A(:)
    real(8), intent(out) :: mean_prof_H_good(:), mean_prof_H_bad(:), mean_prof_H(:), mean_prof_H_work_good(:), mean_prof_H_work_bad(:), mean_prof_H_work(:)
    real(8), intent(out) :: mean_prof_P_good(:), mean_prof_P_bad(:), mean_prof_P(:)
    real(8), intent(out) :: mean_prof_B(:), mean_prof_AIME(:)

    real(8) :: pop_good(dieage-bornage+1), pop_bad(dieage-bornage+1)
    real(8) :: pop(dieage-bornage+1)
    real(8) :: pop_work_good(dieage-bornage+1), pop_work_bad(dieage-bornage+1)
    real(8) :: pop_work(dieage-bornage+1)

    integer(8) :: age
    
    pop = simnum - sum(simdeath, DIM=1)
    pop_good = pop - sum(simhealth, DIM=1, mask=(simdeath==0_8))
    pop_bad = pop - pop_good
    !!Note dead people cannot work!!
    pop_work_good = sum(simP, DIM=1, mask=(simhealth==0.0_8))
    pop_work_bad = sum(simP, DIM=1, mask=(simhealth==1.0_8))
    pop_work = sum(simP, DIM=1)

    mean_prof_C = sum(simC, DIM=1, MASK=(simdeath==0_8))/pop

    mean_prof_A = sum(simA, DIM=1, MASK=(simdeath==0_8))/pop
    !mean_prof_A_good = sum(simA, DIM=1, MASK=(simdeath==0_8 .and. simhealth==0.0_8))/pop_good

    !mean_prof_A_bad = sum(simA, DIM=1, MASK=(simdeath==0_8 .and. simhealth==1.0_8))/pop_bad

    mean_prof_H_good = sum(simH, DIM=1, MASK=(simdeath==0_8 .and. simhealth==0.0_8))/pop_good
    mean_prof_H_bad = sum(simH, DIM=1, MASK=(simdeath==0_8 .and. simhealth==1.0_8))/pop_bad
    mean_prof_H = sum(simH, DIM=1, MASK=(simdeath==0_8))/pop

    mean_prof_H_work_good = sum(simH, DIM=1, MASK=(simdeath==0_8 .and. simhealth==0.0_8 .and. simP==1.0_8))/pop_work_good
    mean_prof_H_work_bad = sum(simH, DIM=1, MASK=(simdeath==0_8 .and. simhealth==1.0_8 .and. simP==1.0_8))/pop_work_bad
    mean_prof_H_work = sum(simH, DIM=1, MASK=(simdeath==0_8 .and. simP==1.0_8))/pop_work

    mean_prof_P_good =  sum(simP, DIM=1, MASK=(simdeath==0_8 .and. simhealth==0.0_8))/pop_good
    mean_prof_P_bad = sum(simP, DIM=1, MASK=(simdeath==0_8 .and. simhealth==1.0_8))/pop_bad

    mean_prof_P = sum(simP, DIM=1, MASK=(simdeath==0_8))/pop

    mean_prof_B = sum(simB, DIM=1, MASK=(simdeath==0_8))/pop

    mean_prof_AIME = sum(simAIME, DIM=1, MASK=(simdeath==0_8))/pop       
       
    open(unit=69, file='simulated_prof.csv')
    write(69, "(A)") &
         "age, C, A_good, A_bad, A, H_good, H_bad, H, H_work_good, H_work_bad, H_work, P_good, P_bad, P, AIME, B"

    do age = 1, dieage-bornage+1
       write(69, &
            '(i2, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5)') &
            age+bornage-1, &
            ',', mean_prof_C(age), ',',mean_prof_A(age), &
             ',', mean_prof_H_good(age), ',', mean_prof_H_bad(age), ',', mean_prof_H(age), &
             ',', mean_prof_H_work_good(age), ',', mean_prof_H_work_bad(age), ',', mean_prof_H_work(age), &
             ',', mean_prof_P_good(age), ',', mean_prof_P_bad(age), ',', mean_prof_P(age), &
             ',', mean_prof_AIME(age), ',',  mean_prof_B(age)
    end do

    close(69)

  end subroutine condmean
end module mod_condmean

