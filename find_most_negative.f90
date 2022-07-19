MODULE FindMostNegativeModule
  IMPLICIT none
  CONTAINS

! Assumptions: d[i] is original index, rd orderred by i=0,.., n-1 
!                                                  d[0] ... d[n-1]
SUBROUTINE find_most_negative(d, rd, n, m, most_index)
  INTEGER, DIMENSION(128) :: d 
  DOUBLE PRECISION, DIMENSION(128) :: rd
  INTEGER :: n, m, most_index, i
  DOUBLE PRECISION :: temp_value

  most_index = d(m + 1)
  temp_value = rd(1)
  DO i = 1, n - m
    IF (rd(i) < temp_value) THEN
      most_index = d(m + i)
      temp_value = rd(i)
    END IF
  END DO

END SUBROUTINE find_most_negative 

END MODULE FindMostNegativeModule