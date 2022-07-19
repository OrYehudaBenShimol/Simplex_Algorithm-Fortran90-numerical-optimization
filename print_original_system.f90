MODULE PrintOriginalSystem
  IMPLICIT none
  CONTAINS
        
SUBROUTINE print_original_system(A, n, m)
  INTEGER :: n, m
  DOUBLE PRECISION, DIMENSION(128, 128) :: A
  INTEGER :: i, j

  DO i = 1, m
    DO j = 1, n
      WRITE(*, '(F10.3)', ADVANCE='no') A(i, j) 
    END DO
    WRITE(*,*) ''
  END DO
  
END SUBROUTINE print_original_system

END MODULE PrintOriginalSystem