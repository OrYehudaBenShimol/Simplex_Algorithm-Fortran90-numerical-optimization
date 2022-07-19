MODULE ComputeCbCdModule
  IMPLICIT none
  CONTAINS

SUBROUTINE compute_cb_cd(cb, cd, c, d, n, m)
  DOUBLE PRECISION, DIMENSION(128) :: cb, cd, c
  INTEGER, DIMENSION(128) :: d 
  INTEGER :: i, n, m

  DO i = 1, m
    cb(i) = c(1 + d(i))
  END DO
   
  DO i = m + 1, n
    cd(i - m) = c(1 + d(i))
  END DO

  WRITE(*, '("d:")')  
  DO i = 1, n
    WRITE(*, '(" "I2" ")', ADVANCE='no') d(i)
  END DO
  WRITE(*,*) ''

  WRITE(*, '("cb:")')  
  DO i = 1, m
    WRITE(*, '(" "F12.6" ")', ADVANCE='no') cb(i)
  END DO
  WRITE(*,*) ''

  WRITE(*, '("cd:")')  
  DO i = 1, n
    WRITE(*, '(" "F12.6" ")', ADVANCE='no') cd(i)
  END DO
  WRITE(*,*) ''

END SUBROUTINE compute_cb_cd

END MODULE ComputeCbCdModule