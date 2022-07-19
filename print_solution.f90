MODULE PrintSolutionModule
  IMPLICIT NONE
  CONTAINS

SUBROUTINE print_solution(basis, BIb, c,  m)
  INTEGER, DIMENSION(128) :: basis
  DOUBLE PRECISION, DIMENSION(128) :: BIb, c
  DOUBLE PRECISION :: temp, sum 
  INTEGER :: m, i
  
  WRITE (*, *) ''
  WRITE (*, '("basis:")')
  DO i = 1, m
    WRITE (*, '(" "I2" ")', ADVANCE='no') basis(i)
  END DO
  WRITE (*, *) ''

  WRITE (*, *) ''
  WRITE (*, '("Basic Solution:")')
  DO i = 1, m
    WRITE (*, '("    X("I2") = "F12.6)', ADVANCE='no') basis(i) + 1, BIb(i)
  END DO
  WRITE (*, *) ''

  WRITE (*, *) ''
  WRITE (*, '("Solution value:")')

  temp = c(basis(1) + 1) * BIb(1)
  sum = temp;
  WRITE (*, '(" "F12.6" * "F12.6" ")', ADVANCE='no')  c(basis(1) + 1), BIb(1) 

  DO i = 2, m
    temp = c(basis(i) + 1) * BIb(i)
    sum = sum + temp
    WRITE (*, '("+"F12.6" * "F12.6" ")', ADVANCE='no')  c(basis(i) + 1), BIb(i)  
  END DO

  WRITE (*, '(" = "F12.6)') sum

END SUBROUTINE print_solution

END MODULE PrintSolutionModule