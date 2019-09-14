!   E04CBF Example Program Text
!   Mark 23 Release. NAG Copyright 2011.
MODULE mod_NAG_min

  !      E04CBF Example Program Module:
  !             Parameters and User-defined Routines

  !      .. Use Statements ..
  USE nag_library, ONLY : e04cbf, e04cbk, nag_wp, x02ajf
  USE mod_gmm
  USE mod_parameter
  !      .. Implicit None Statement ..
  IMPLICIT NONE
  !      .. Parameters ..
  INTEGER, PARAMETER                  :: nout = 6
CONTAINS
  SUBROUTINE monit(fmin,fmax,sim,n,ncall,serror,vratio,iuser,ruser)

    !         .. Implicit None Statement ..
    IMPLICIT NONE
    !         .. Scalar Arguments ..
    REAL (KIND=nag_wp), INTENT (IN)     :: fmax, fmin, serror, vratio
    INTEGER, INTENT (IN)                :: n, ncall
    !         .. Array Arguments ..
    REAL (KIND=nag_wp), INTENT (INOUT)  :: ruser(*)
    REAL (KIND=nag_wp), INTENT (IN)     :: sim(n+1,n)
    INTEGER, INTENT (INOUT)             :: iuser(*)
    !         .. Executable Statements ..
    WRITE (nout,*)
    WRITE (nout,99999) ncall
    WRITE (nout,99998) fmin
    WRITE (nout,99997)
    WRITE (nout,99996) sim(1:(n+1),1:n)
    WRITE (nout,99995) serror
    WRITE (nout,99994) vratio

    RETURN

99999 FORMAT (1X,'There have been',I5,' function calls')
99998 FORMAT (1X,'The smallest function value is',F10.4)
99997 FORMAT (1X,'The simplex is')
99996 FORMAT (1X,2F10.4)
99995 FORMAT (1X,'The standard deviation in function values at the ', &
         'vertices of the simplex is',F10.4)
99994 FORMAT (1X,'The linearized volume ratio of the current simplex', &
         ' to the starting one is',F10.4)
  END SUBROUTINE monit

  SUBROUTINE NAG_min(x)
  !      E04CBF Example Main Program

  !      .. Implicit None Statement ..
  IMPLICIT NONE
  !      .. Parameters ..
  INTEGER, PARAMETER                  :: n = 7
  !      **Initial Values**
  real(KIND=nag_wp), intent(inout) :: x(n)
  !      .. Local Scalars ..
  REAL (KIND=nag_wp)                  :: f, tolf, tolx
  INTEGER                             :: ifail, maxcal
  !      .. Local Arrays ..
  REAL (KIND=nag_wp)                  :: ruser(1)
  INTEGER                             :: iuser(1)
  !      .. Output ..
  character :: filename*128
  !      .. Intrinsic Functions ..
  INTRINSIC                              sqrt
  !      .. Executable Statements ..
  !WRITE (nout,*) 'E04CBF Example Program Results'

  tolf = sqrt(x02ajf())
  tolx = sqrt(tolf)
  maxcal = 10000
  ruser(1) = 1.0_nag_wp
  iuser(1) = 1
  ifail = 0


  CALL e04cbf(n,x,f,tolf,tolx,gmm,monit,maxcal,iuser,ruser,ifail)

  WRITE (nout,*)
  WRITE (nout,99999) f
  WRITE (nout,99998) x(1:n)

  if (job==2_8 .and. two_step==0_8) then
     open(unit=2, file='params/first_step_params.csv')
     !  write(1, '(A)') 'p_gamh , p_gamc , p_leispref , p_leisureprefbad, p_fixcost , p_beta , p_bequest , obj'
     write(2,*) x
     close(2)
  else if (job==3_8) then 
     
     write(filename, '("params/estimated_params_iter", i3.3, ".csv")') init_iter
     open(unit=3, file=filename, status='replace') 
     write(3, '(A)') 'p_gamh , p_gamc , p_leispref , p_leisureprefbad, p_fixcost , p_beta , p_bequest , obj'
        write(3,'(f10.5, a, f10.5, a, f10.5, a, f10.5, a, f10.5, a, f10.5, a, f10.5, &
             & a, f10.5)') x(1)/10.0_nag_wp, ',', x(2), ',', x(3)*1000_nag_wp, ',', x(4)*100.0_nag_wp, ',', x(5)*100.0_nag_wp, ',', x(6)/10.0_nag_wp, ',', x(7)/10.0_nag_wp, ',', f

  end if

99999 FORMAT (1X,'The final function value is',F12.4)
99998 FORMAT (1X,'at the point',2F12.4)

END SUBROUTINE NAG_min
END MODULE mod_NAG_min

