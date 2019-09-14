MODULE mod_NAG_inverse

!      F07AJF Example Program Text

!      Mark 23 Release. NAG Copyright 2011.

!      .. Use Statements ..
       USE nag_library, ONLY : dgetrf, dgetri, nag_wp, x04caf
!      .. Implicit None Statement ..
       IMPLICIT NONE
contains
  subroutine NAG_inverse(mat, inv)
    
    IMPLICIT NONE

!    real (KIND=nag_wp), intent(in) :: mat(1:4,1:4)
    real, intent(in) :: mat(:,:)
!    real (KIND=nag_wp), intent(out) :: inv(1:4,1:4)
    real, intent(out) :: inv(:,:)
!      .. Parameters ..
    INTEGER, PARAMETER              :: nin = 5, nout = 6
!      .. Local Scalars ..
    INTEGER                         :: i, ifail, info, lda, lwork, n
!      .. Local Arrays ..
!    REAL (KIND=nag_wp), ALLOCATABLE :: a(1:4,1:4), work(1:4)
!    REAL (KIND=nag_wp) :: a(4_8,4_8), work(4_8)
    REAL :: a(4_8,4_8), work(4_8)
!    REAL (KIND=nag_wp) :: d = (2,2)
    REAL :: d = (2,2)
!    INTEGER, ALLOCATABLE            :: ipiv(1:4)
    INTEGER            :: ipiv(4_8)
!      .. Executable Statements ..
    WRITE (nout,*) 'F07AJF Example Program Results'
!      Skip heading in data file
!       READ (nin,*)
!       READ (nin,*) n

    n = 4_8

    lda = n
    lwork = 64*n
!    ALLOCATE (a(lda,n),work(lwork),ipiv(n))

!      Read A from data file

!       READ (nin,*) (a(i,1:n),i=1,n)

    a = mat

    write(*,*) n, a, size(a), lda, ipiv, info, lwork

!      Factorize A

!      The NAG name equivalent of dgetrf is f07adf
    CALL dgetrf(4,4,a,4,ipiv,info)

    WRITE (nout,*)
    FLUSH (nout)
    IF (info==0) THEN

!         Compute inverse of A

!         The NAG name equivalent of dgetri is f07ajf
!       CALL dgetri(n,a,lda,ipiv,work,lwork,info)

       inv = a

!         Print inverse

!         ifail: behaviour on error exit
!                =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
       ifail = 0
!       CALL x04caf('General',' ',n,n,a,lda,'Inverse',ifail)
       
    ELSE
       WRITE (nout,*) 'The factor U is singular'
    END IF
  end subroutine NAG_inverse
end MODULE mod_NAG_inverse
