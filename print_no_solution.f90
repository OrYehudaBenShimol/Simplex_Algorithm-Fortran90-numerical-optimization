MODULE PrintNoSolutionModule
  IMPLICIT NONE
  CONTAINS

SUBROUTINE print_no_solution
  WRITE (*, *) 'System A has NO solution'
END SUBROUTINE print_no_solution

END MODULE PrintNoSolutionModule