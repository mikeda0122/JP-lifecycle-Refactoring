module mod_condmean

  use mod_parameter

  implicit none

contains

  subroutine condmean(simdeath, simhealth, simC, simA, simH, simP, simB, simAIME, mean_prof_C, mean_prof_A, &
    & mean_prof_H_good, mean_prof_H_bad, mean_prof_H, mean_prof_H_work_good, mean_prof_H_work_bad, mean_prof_H_work,&
    & mean_prof_P_good, mean_prof_P_bad, mean_prof_P, mean_prof_B, mean_prof_AIME)

    implicit none

    integer(8), intent(in) :: simdeath(:,:)
    real(8), intent(in) :: simhealth(:,:), simC(:,:), simA(:,:), simH(:,:), simP(:,:), simB(:,:), simAIME(:,:)
    real(8), intent(out) :: mean_prof_C(:), mean_prof_A(:)
    real(8), intent(out) :: mean_prof_H_good(:), mean_prof_H_bad(:), mean_prof_H(:), mean_prof_H_work_good(:),&
    & mean_prof_H_work_bad(:), mean_prof_H_work(:)
    real(8), intent(out) :: mean_prof_P_good(:), mean_prof_P_bad(:), mean_prof_P(:)
    real(8), intent(out) :: mean_prof_B(:), mean_prof_AIME(:)

    real(8) :: pop_good(dieage-bornage+1), pop_bad(dieage-bornage+1)
    real(8) :: pop(dieage-bornage+1)
    real(8) :: pop_work_good(dieage-bornage+1), pop_work_bad(dieage-bornage+1)
    real(8) :: pop_work(dieage-bornage+1)

    integer(8) :: age, i



    do age = 1, dieage-bornage+1
       !mean_prof_C_good(age) = 0.0_8
       !mean_prof_C_bad(age) = 0.0_8
       mean_prof_C(age) = 0.0_8
       mean_prof_A(age) = 0.0_8
       !mean_prof_A_good(age) = 0.0_8
       !mean_prof_A_bad(age) = 0.0_8
       mean_prof_H_good(age) = 0.0_8
       mean_prof_H_bad(age) = 0.0_8
       mean_prof_H(age) = 0.0_8
       mean_prof_H_work_good(age) = 0.0_8
       mean_prof_H_work_bad(age) = 0.0_8
       mean_prof_H_work(age) = 0.0_8
       !mean_prof_B_good(age) = 0.0_8
       !mean_prof_B_bad(age) = 0.0_8
       mean_prof_B(age) = 0.0_8
       mean_prof_P(age) = 0.0_8
       mean_prof_P_good(age) = 0.0_8
       mean_prof_P_bad(age) = 0.0_8
       !mean_prof_W_good(age) = 0.0_8
       !mean_prof_W_bad(age) = 0.0_8
       !mean_prof_I(age) = 0.0_8
       !mean_prof_pb(age) = 0.0_8
       !mean_prof_ss(age) = 0.0_8
       mean_prof_AIME(age) = 0.0_8
       pop_good(age) = 0.0_8
       pop_bad(age) = 0.0_8
       pop(age) = 0.0_8
       pop_work_good(age) = 0.0_8
       pop_work_bad(age) = 0.0_8
       pop_work(age) = 0.0_8
       if (age+bornage-1>=70) then
          pop_work_good(age) = 1.0_8
          pop_work_bad(age) = 1.0_8
          pop_work(age) = 1.0_8
       end if
    end do


    do i = 1, simnum

       do age = 1, dieage-bornage+1
          if (simdeath(i, age)==0_8) then
             if (simhealth(i, age)==0.0_8) then
                !mean_prof_C_good(age) = mean_prof_C_good(age) + simC(i, age)
                !mean_prof_A_good(age) = mean_prof_A_good(age) + simA(i, age)
                mean_prof_H_good(age) = mean_prof_H_good(age) + simH(i, age)
                !mean_prof_B_good(age) = mean_prof_B_good(age) + simB(i, age)
                !mean_prof_W_good(age) = mean_prof_W_good(age) + simW(i, age)
                if (simH(i, age)>0.0_8) then
                   mean_prof_H_work_good(age) = mean_prof_H_work_good(age) + simH(i, age)
                   mean_prof_P_good(age) = mean_prof_P_good(age) + 1.0_8
                   pop_work_good(age) = pop_work_good(age) + 1
                end if

                pop_good(age) = pop_good(age) + 1
             else if (simhealth(i, age)==1.0_8) then
                !mean_prof_C_bad(age) = mean_prof_C_bad(age) + simC(i, age)
                !mean_prof_A_bad(age) = mean_prof_A_bad(age) + simA(i, age)
                mean_prof_H_bad(age) = mean_prof_H_bad(age) + simH(i, age)
                !mean_prof_B_bad(age) = mean_prof_B_bad(age) + simB(i, age)
                !mean_prof_W_bad(age) = mean_prof_W_bad(age) + simW(i, age)
                if (simH(i, age)>0.0_8) then
                   mean_prof_H_work_bad(age) = mean_prof_H_work_bad(age) + simH(i, age)
                   mean_prof_P_bad(age) = mean_prof_P_bad(age) + 1.0_8
                   pop_work_bad(age) = pop_work_bad(age) + 1
                end if

                pop_bad(age) = pop_bad(age) + 1
             end if
             mean_prof_C(age) = mean_prof_C(age) + simC(i, age)
             mean_prof_B(age) = mean_prof_B(age) + simB(i, age)
             mean_prof_H(age) = mean_prof_H(age) + simH(i, age)
             if (simH(i, age)>0.0_8) then
                mean_prof_H_work(age) = mean_prof_H_work(age) + simH(i, age)
                mean_prof_P(age) = mean_prof_P(age) + 1.0_8
                pop_work(age) = pop_work(age) + 1
             end if
             mean_prof_A(age) = mean_prof_A(age) + simA(i, age)
             !mean_prof_I(age) = mean_prof_I(age) + prof_I(age)
             !mean_prof_pb(age) = mean_prof_pb(age) + prof_pb(age)
             !mean_prof_ss(age) = mean_prof_ss(age) + prof_ss(age)
             mean_prof_AIME(age) = mean_prof_AIME(age) + simAIME(i, age)
             pop(age) = pop(age) + 1
          end if

       end do

    end do

    do age = 1, dieage-bornage+1
       if (pop(age)==0.0_8) then
          pop(age)=1.0_8
       end if
       if (pop_good(age)==0.0_8) then
          pop_good(age)=1.0_8
       end if
       if (pop_bad(age)==0.0_8) then
          pop_bad(age)=1.0_8
       end if
       if (pop_work_good(age)==0.0_8) then
          pop_work_good(age)=1.0_8
       end if
       if (pop_work_bad(age)==0.0_8) then
          pop_work_bad(age)=1.0_8
       end if
    end do

    mean_prof_C = (mean_prof_C)/(pop)
    !mean_prof_C_good = mean_prof_C_good/pop_good
    !mean_prof_C_bad = mean_prof_C_bad/pop_bad
    mean_prof_A = mean_prof_A/pop
    !mean_prof_A_good = mean_prof_A_good/pop_good
    !mean_prof_A_bad = mean_prof_A_bad/pop_bad
    mean_prof_H_good = mean_prof_H_good/pop_good
    mean_prof_H_bad = mean_prof_H_bad/pop_bad
    mean_prof_H = mean_prof_H/pop
    !mean_prof_B_good = mean_prof_B_good/pop_good
    !mean_prof_B_bad = mean_prof_B_bad/pop_bad
    mean_prof_B = (mean_prof_B)/(pop)
    mean_prof_P_good = mean_prof_P_good/pop_good
    mean_prof_P_bad = mean_prof_P_bad/pop_bad
    mean_prof_P = mean_prof_P/pop
    !mean_prof_W_good = mean_prof_W_good/pop_good
    !mean_prof_W_bad = mean_prof_W_bad/pop_bad
    !mean_prof_I = mean_prof_I/pop
    !mean_prof_pb = mean_prof_pb/pop
    !mean_prof_ss = mean_prof_ss/pop
    mean_prof_AIME = mean_prof_AIME/pop
    mean_prof_H_work_good = mean_prof_H_work_good/pop_work_good
    mean_prof_H_work_bad = mean_prof_H_work_bad/pop_work_bad
    mean_prof_H_work = (mean_prof_H_work)/(pop_work)

    open(unit=69, file='simulated_prof.csv')
    write(69, "(A)") &
         "age, pop, pop_good, pop_work_good, C, A, H_good, H_bad, H, H_work_good, H_work_bad, H_work, P_good, P_bad, P, AIME, B"

    do age = 1, dieage-bornage+1
       write(69, &
            '(i2, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5,&
            & a, f18.5, a, f18.5, a, f18.5, a, f18.5, a, f18.5)') &
            age+bornage-1,',', pop(age), ',', pop_good(age), ',', pop_work_good(age), &
            ',', mean_prof_C(age), ',',mean_prof_A(age), &
             ',', mean_prof_H_good(age), ',', mean_prof_H_bad(age), ',', mean_prof_H(age), &
             ',', mean_prof_H_work_good(age), ',', mean_prof_H_work_bad(age), ',', mean_prof_H_work(age), &
             ',', mean_prof_P_good(age), ',', mean_prof_P_bad(age), ',', mean_prof_P(age), &
             ',', mean_prof_AIME(age), ',',  mean_prof_B(age)
    end do

    close(69)

  end subroutine condmean
end module mod_condmean
