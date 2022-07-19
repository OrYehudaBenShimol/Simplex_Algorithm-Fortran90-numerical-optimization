MODULE PrintInitialSolutionModule
  IMPLICIT NONE
  CONTAINS

SUBROUTINE print_initial_solution(Initial_basis, Initial_BIb, m)
  INTEGER, DIMENSION(128) :: Initial_basis
  DOUBLE PRECISION, DIMENSION(128) :: Initial_BIb 
  INTEGER :: m, i
  
  WRITE (*, *) ''
  WRITE (*, '("Initial basis:")')
  DO i = 1, m
    WRITE (*, '(" "I2" ")', ADVANCE='no') Initial_basis(i)
  END DO
  WRITE (*, *) ''

  WRITE (*, *) ''
  WRITE (*, '("Basic Solution:")')
  DO i = 1, m
    WRITE (*, '("    X("I2") = "F12.6)', ADVANCE='no') Initial_basis(i), Initial_BIb(i)
  END DO
  WRITE (*, *) ''

END SUBROUTINE print_initial_solution

END MODULE PrintInitialSolutionModule