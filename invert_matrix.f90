MODULE InvertMatrixModule
  USE SwapRowsModule
  IMPLICIT none
  CONTAINS

SUBROUTINE inv_gaussian(B, A, n, epsilon)
  INTEGER :: n
  DOUBLE PRECISION, DIMENSION(128, 128) :: A, B, W
  DOUBLE PRECISION :: epsilon
  INTEGER :: i, j, k, p, itemp
  DOUBLE PRECISION :: MaxValue, RelativeValue, temp

  DO i = 1, n
    DO j = 1, n
      W(i, j) = A(i, j)
    END DO
  END DO

  DO i = 1, n
    DO j = n + 1, 2 * n
      W(i, j) = 0.0
    END DO
  END DO

  DO i = 1, n
    W(i, n + i) = 1.0
  END DO

  WRITE(*,*) ''
  WRITE(*, '("Before loop W: ")')  
  DO i = 1, n
    DO j = 1, 2 * n
      WRITE(*, '(" "F8.2" ")', ADVANCE='no') W(i, j) 
    END DO
    WRITE(*,*) ''
  END DO
  WRITE(*,*) ''
  
  DO k = 1, n
    WRITE(*, '("k = "I2)') k - 1
    p = k
    MaxValue = ABS(W(k,k))

    DO i = k + 1,n
      IF (ABS(W(i, k)) >  MaxValue) THEN
        p = i
        MaxValue = ABS(W(i, k))
      END IF
    END DO
  
    WRITE(*,*) ''
    WRITE(*, '("p = "I2", k = "I2)') p - 1, k - 1
 
    IF (p /= k) THEN
      CALL swap_rows(W, n, k, p);
    END IF
    RelativeValue = W(k, k)
    WRITE(*, '("RelativeValue = "F8.2)') RelativeValue
    W(k, k) = 1.0

    DO j = k + 1, 2 * n
      temp = W(k, j) / RelativeValue
      IF (ABS(temp) < epsilon) THEN
        W(k, j) = 0.0
      ELSE
        W(k, j) = temp
      END IF
    END DO

    DO i = 1, n
      IF (i /= k) THEN
        RelativeValue = W(i, k)
        W(i, k) = 0.0
        DO j = k + 1, 2 * n + 1
          temp = W(i, j) - RelativeValue * W(k, j) 
          IF (ABS(temp) < epsilon) THEN
            W(i, j) = 0.0
          ELSE
            W(i, j) = temp
          END IF
        END DO
      END IF
    END DO

    WRITE(*,*) ''
    WRITE(*, '(" W: ")')  
    DO i = 1, n
      DO j = 1, 2 * n
        WRITE(*, '(" "F8.2" ")', ADVANCE='no') W(i, j) 
      END DO
      WRITE(*,*) ''
    END DO
    WRITE(*,*) ''
  END DO

  DO i = 1, n
    DO j = 1, n
      B(j, i) = W(j, i + n)
    END DO
  END DO

  WRITE(*,*) ''
  WRITE(*, '(" BI: ")')  
  DO i = 1, n
    DO j = 1, n
      WRITE(*, '(" "F12.6" ")', ADVANCE='no') B(i, j) 
    END DO
    WRITE(*,*) ''
  END DO
  WRITE(*,*) ''

  WRITE(*,*) ''
  WRITE(*, '(" W: ")')  
  DO i = 1, n
    DO j = 1, 2 * n
      WRITE(*, '(" "F8.2" ")', ADVANCE='no') W(i, j) 
    END DO
    WRITE(*,*) ''
  END DO
  WRITE(*,*) ''

END SUBROUTINE inv_gaussian

END MODULE InvertMatrixModule