MODULE CopyMatrix
  IMPLICIT none
  CONTAINS
        
SUBROUTINE copy_matrix(Dest, Source, n, m)
  INTEGER :: n, m
  DOUBLE PRECISION, DIMENSION(128, 128) :: Dest, Source
  INTEGER :: i, j

  DO i = 1, m
    DO j = 1, n
      Dest(i, j) = Source(i, j) 
    END DO
  END DO
  
END SUBROUTINE copy_matrix

END MODULE CopyMatrix