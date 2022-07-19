MODULE ReadFileModule
  USE CopyMatrix
  IMPLICIT none
  CONTAINS
        
SUBROUTINE get_number_from_string(number, str)
  DOUBLE PRECISION number
  CHARACTER(LEN = 80) :: str
  INTEGER :: ii

  ii = INDEX(str,' ')
  READ(str(1:ii-1), *) number
  str = ADJUSTL(str(ii+1:len(str)))
  
END SUBROUTINE get_number_from_string 

SUBROUTINE read_file(m, n, A, B, C, epsilon, A_aux)
  INTEGER :: m, n
  DOUBLE PRECISION, DIMENSION(128, 128) :: A, A_aux
  DOUBLE PRECISION, DIMENSION(128) :: B, C 
  DOUBLE PRECISION :: epsilon, bb

  CHARACTER(LEN = 80) :: str
  INTEGER :: i, j, IERR 

  READ(1, '(A)', iostat=IERR) str
  WRITE(*, '("str =")', ADVANCE='no')
  WRITE(*, *) str
  WRITE(*, *) ''
  
  READ(1, '(A)', iostat=IERR) str
  str = ADJUSTL(str)
  DO i = 1, n
    CALL get_number_from_string(C(i), str)
  END DO
  DO i = 1, n
    WRITE(*, '("c["I2"] ="F9.6)') i-1, C(i) 
  END DO
  WRITE(*, *) ''

  READ(1, '(A)', iostat=IERR) str
  WRITE(*, '("A: str =")', ADVANCE='no')
  WRITE(*, *) str
  WRITE(*, *) ''
 
  DO i = 1, m
    READ(1, '(A)', iostat=IERR) str
    str = ADJUSTL(str)
    DO j = 1, n
      CALL get_number_from_string(A(i, j), str) 
      WRITE(*, '(F12.6)', ADVANCE='no') A(i,j)
    END DO
    WRITE(*, *) ''
  END DO

  WRITE(*, *) ''
  READ(1, '(A)', iostat=IERR) str
  WRITE(*, '("b: str =")', ADVANCE='no')
  WRITE(*, *) str
  WRITE(*, *) ''
 
  READ(1, '(A)', iostat=IERR) str
  str = ADJUSTL(str)
  DO i = 1, m
    CALL get_number_from_string(B(i), str)
  END DO

  WRITE(*, *) ''
  READ(1, '(A)', iostat=IERR) str
  WRITE(*, '("str =")', ADVANCE='no')
  WRITE(*, *) str
  WRITE(*, *) ''
  READ(1, *, iostat=IERR) epsilon

  WRITE(*, '("b:")')
  DO i = 1, m 
    WRITE(*, '(F12.6)', ADVANCE='no') B(i)
  END DO
  WRITE(*, *) ''
  WRITE(*, *) ''

  CALL copy_matrix(A_aux, A, n, m)

END SUBROUTINE read_file

END MODULE ReadFileModule