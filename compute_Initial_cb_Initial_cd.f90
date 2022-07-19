MODULE ComputeInitialCbInitialCdModule
  IMPLICIT none
  CONTAINS

SUBROUTINE compute_Initial_cb_Initial_cd(Initial_cb, Initial_cd, Initial_c, Initial_d, Initial_n, m)
  DOUBLE PRECISION, DIMENSION(128) :: Initial_cb, Initial_cd, Initial_c
  INTEGER, DIMENSION(128) :: Initial_d 
  INTEGER :: i, Initial_n, m

  DO i = 1, m
    Initial_cb(i) = Initial_c(1 + Initial_d(i))
  END DO
   
  DO i = m + 1, Initial_n
    Initial_cd(i - m) = Initial_c(1 + Initial_d(i))
  END DO

  WRITE(*, '("Initial_d:")')  
  DO i = 1, Initial_n
    WRITE(*, '(" "I2" ")', ADVANCE='no') Initial_d(i)
  END DO
  WRITE(*,*) ''

  WRITE(*, '("Initial_cb:")')  
  DO i = 1, m
    WRITE(*, '(" "F12.6" ")', ADVANCE='no') Initial_cb(i)
  END DO
  WRITE(*,*) ''

  WRITE(*, '("Initial_cd:")')  
  DO i = 1, Initial_n - m
    WRITE(*, '(" "F12.6" ")', ADVANCE='no') Initial_cd(i)
  END DO
  WRITE(*,*) ''

END SUBROUTINE compute_Initial_cb_Initial_cd

END MODULE ComputeInitialCbInitialCdModule