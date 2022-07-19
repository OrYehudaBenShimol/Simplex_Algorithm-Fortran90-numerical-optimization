MODULE FindMinValueModule
  IMPLICIT none
  CONTAINS

! Assumptions: d[i] is original index, rd orderred by i=0,.., n-1 
!                                                  d[0] ... d[n-1]

SUBROUTINE find_min_value(rd, n, min_value)
  DOUBLE PRECISION, DIMENSION(128) :: rd
  DOUBLE PRECISION :: min_value
  INTEGER :: n, i

  min_value = rd(1)
  DO i = 1, n
    IF (rd(i) <  min_value) THEN
      min_value = rd(i)
    END IF
  END DO

END SUBROUTINE find_min_value 

END MODULE FindMinValueModule