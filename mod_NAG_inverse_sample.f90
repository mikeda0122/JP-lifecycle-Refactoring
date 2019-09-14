    PROGRAM f07ajfe

!      F07AJF Example Program Text

!      Mark 23 Release. NAG Copyright 2011.

!      .. Use Statements ..
       USE nag_library, ONLY : f07adf, f07ajf, nag_wp, x04caf
!      .. Implicit None Statement ..
       IMPLICIT NONE
!      .. Parameters ..
       INTEGER, PARAMETER              :: nin = 5, nout = 6
!      .. Local Scalars ..
       INTEGER                         :: i, ifail, info, lda, lwork, n
!      .. Local Arrays ..
       REAL (KIND=nag_wp), ALLOCATABLE :: a(:,:), work(:)
       INTEGER, ALLOCATABLE            :: ipiv(:)
!      .. Executable Statements ..
       WRITE (nout,*) 'F07AJF Example Program Results'
!      Skip heading in data file
       READ (nin,*)
       READ (nin,*) n
       lda = n
       lwork = 64*n
       ALLOCATE (a(lda,n),work(lwork),ipiv(n))

!      Read A from data file

       READ (nin,*) (a(i,1:n),i=1,n)

!      Factorize A

!      The NAG name equivalent of dgetrf is f07adf
       CALL f07adf(n,n,a,lda,ipiv,info)

       WRITE (nout,*)
       FLUSH (nout)
       IF (info==0) THEN

!         Compute inverse of A

!         The NAG name equivalent of dgetri is f07ajf
          CALL f07ajf(n,a,lda,ipiv,work,lwork,info)

!         Print inverse

!         ifail: behaviour on error exit
!                =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
          ifail = 0
          CALL x04caf('General',' ',n,n,a,lda,'Inverse',ifail)

       ELSE
          WRITE (nout,*) 'The factor U is singular'
       END IF

    END PROGRAM f07ajfe
