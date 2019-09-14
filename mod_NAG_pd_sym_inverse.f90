MODULE mod_NAG_pd_sym_inverse

!      F07AJF Example Program Text

!      Mark 23 Release. NAG Copyright 2011.

!      .. Use Statements ..
  USE nag_library, ONLY : f01adf, nag_wp, x04caf
!      .. Implicit None Statement ..
       IMPLICIT NONE
contains
  subroutine NAG_pd_sym_inverse(mat, inv)
    
    IMPLICIT NONE

    real (KIND=nag_wp), intent(in) :: mat(:,:)
    real (KIND=nag_wp), intent(out) :: inv(:,:)
!      .. Parameters ..
    INTEGER, PARAMETER              :: nin = 5, nout = 6
!      .. Local Scalars ..
    INTEGER                         :: i, ifail, lda, n
!      .. Local Arrays ..
    REAL (KIND=nag_wp), ALLOCATABLE :: a(:,:)
    real (KIND=nag_wp) :: E(4,4)
!      .. Executable Statements ..
    WRITE (nout,*) 'F07AJF Example Program Results'
!      Skip heading in data file
!       READ (nin,*)
!       READ (nin,*) n

    n = ubound(mat, 1) - lbound(mat, 1) + 1

    lda = n + 1
    ALLOCATE (a(lda,n))

!      Read A from data file

!       READ (nin,*) (a(i,1:n),i=1,n)

    a(1:n,1:n) = mat

!    write(*,*) n, a, size(a), lda

!      Factorize A

!         Compute inverse of A
!         ifail: behaviour on error exit
!                =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
    ifail = 0
    CALL f01adf(n, a, lda, ifail)

    if (ifail/=0) then
       write(*,*) 'Failed to compute an inverse matrix'
    end if

    inv = a(2:n+1,1:n)

    do i = 1, n
       inv(i,1:n) = inv(1:n,i) 
    end do

!    E = matmul(mat, inv)

!    CALL x04caf('L', 'B', lda, n, a, lda, 'Lower triangle of inverse', ifail)    

!    CALL x04caf('General',' ',n,n,inv,n,'Inverse',ifail)

!    CALL x04caf('General',' ',n,n,E,n,'Identity',ifail)
  end subroutine NAG_pd_sym_inverse
end MODULE mod_NAG_pd_sym_inverse
