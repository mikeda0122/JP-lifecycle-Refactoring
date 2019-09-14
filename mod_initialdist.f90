module mod_initialdist

  use mod_parameter

  implicit none

contains
  subroutine initialdist(A_dist, M_dist, W_dist, AIME_dist)

    real(8), intent(out) :: A_dist(:), M_dist(:), W_dist(:), AIME_dist(:)

    integer(8) :: j

    open(unit=89, file='JP_init_dist/init_wage.csv') !!Data from wsim, which is not consistent with logwage data.
    do j=1, simnum
       read(89,*) W_dist(j)
    end do
    close(89)

    open(unit=87, file='JP_init_dist/init_assets.csv') !!Data from asim
    do j=1, simnum
       read(87,*) A_dist(j)
    end do
    close(87)

    open(unit=88, file='JP_init_dist/sim_M_dist_2.csv') !!Data from healsim
    do j=1, simnum
       read(88,*) M_dist(j)
    end do
    close(88)

!    open(unit=90, file='init_dist/sim_AIME_dist.csv')
    do j=1, simnum
!       read(90,*) AIME_dist(j)
       AIME_dist(j) = W_dist(j) * 2500 * penrate
    end do
!    close(90)

  end subroutine initialdist
end module mod_initialdist
