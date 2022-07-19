MODULE CopySubMatrixModule
  IMPLICIT none
  CONTAINS
        
SUBROUTINE copy_submatrix(Dest, Source, istart, depth, jstart, length)
  INTEGER :: istart, depth, jstart, length
  DOUBLE PRECISION, DIMENSION(128, 128) :: Dest, Source
  INTEGER :: i, j

  DO i = istart + 1, depth
    DO j = jstart + 1, jstart + length
      Dest(i - istart, j - jstart) = Source(i, j) 
    END DO
  END DO
  
END SUBROUTINE copy_submatrix

END MODULE CopySubMatrixModule