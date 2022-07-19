PROGRAM simplex
  USE BubbleSortDModule
  USE ReadFileModule
  USE PrintOriginalSystem
  USE CopyToInitialMatrix
  USE InitialSimplexAlgorithm
  USE PrintNoSolutionModule
  USE PrintInitialSolutionModule
  USE PrintSolutionModule
  USE SimplexAlgorithmModule
  IMPLICIT NONE
  
  CHARACTER(LEN = 80) :: inputFileName 
  CHARACTER(LEN = 80) :: str 
  INTEGER :: IERR
  INTEGER :: i, j, m, n, n_p_m, Initial_n, itemp
  INTEGER, DIMENSION(128) :: basis, dd
  INTEGER, DIMENSION(128) :: Initial_basis, Initial_dd
  DOUBLE PRECISION, DIMENSION(128, 128) :: A, A_aux, B, BI, BIA_aux, D
  DOUBLE PRECISION, DIMENSION(128, 128) :: Initial_A, Initial_A_aux 
  DOUBLE PRECISION, DIMENSION(128, 128) :: Initial_B, Initial_BI, Initial_BIA_aux
  DOUBLE PRECISION, DIMENSION(128, 128) :: Initial_D  
  DOUBLE PRECISION, DIMENSION(128) :: bb, C, BIb, cb, cbBI, cbBID, cd, rd 
  DOUBLE PRECISION, DIMENSION(128) :: Initial_bb, Initial_b_aux, Initial_BIb 
  DOUBLE PRECISION, DIMENSION(128) :: Initial_cb, Initial_cbBI, Initial_cbBID, Initial_cc, Initial_cd, Initial_rd
  DOUBLE PRECISION :: epsilon

  inputFileName = 'bur3.txt'

  OPEN(1,FILE=inputFileName)
    READ(1,'(A)',iostat=IERR) str
    WRITE(*, '("str =")', ADVANCE='no')
    WRITE(*, *) str
    WRITE(*, *) ''
    READ(1, *) m, n
    WRITE(*, '("n ="I2", m ="I2)') n, m
    Initial_n = n + m
    WRITE(*, '("str =")')
    WRITE(*, *) ''
    n_p_m = n + m
    CALL read_file(m, n, A, bb, C, epsilon, A_aux)
    WRITE(*, '("epsilon = "F6.2)') epsilon
  CLOSE(1)

  WRITE(*, '(" A: ")')  
  CALL print_original_system(A, n, m)

  CALL copy_to_initial_matrix(A, Initial_A, Initial_A_aux, n, m)

  WRITE(*, *) ''
  WRITE(*, '("Initial_A:")')  
  DO i = 1, m
    DO j = 1, n_p_m
      WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_A(i, j) 
    END DO
    WRITE(*,*) ''
  END DO
  WRITE(*,*) ''

  DO i = 1, m
    Initial_basis(i) = i + n - 1
  END DO
  DO i = 1, n
     Initial_cc(i) = 0.0
  END DO
  DO i = n + 1, Initial_n
     Initial_cc(i) = 1.0
  END DO
  DO i = 1, m
     Initial_bb(i) = bb(i)
  END DO
  DO i = 1, m
     Initial_b_aux(i) = bb(i)
  END DO

  WRITE(*,*) ''
  WRITE(*, '("Initial_basis:")')
  DO i = 1, m
    WRITE(*, '(" "I2" ")', ADVANCE='no') Initial_basis(i)
  END DO
  WRITE(*,*) ''

  WRITE(*,*) ''
  WRITE(*, '("Initial_c:")')
  DO i = 1, Initial_n
     WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_cc(i)
  END DO
  WRITE(*,*) ''

  WRITE(*,*) ''
  WRITE(*, '("Initial_b:")')
  DO i = 1, m
     WRITE(*, '(" "F6.2" ")', ADVANCE='no') Initial_bb(i)
  END DO
  WRITE(*,*) ''
 
  CALL Initial_simplex_algorithm(Initial_A, Initial_A_aux, Initial_B, Initial_bb, Initial_basis, & 
         Initial_BI, Initial_BIA_aux, Initial_BIb, Initial_cb, Initial_cbBI, Initial_cbBID, Initial_cd, &
         Initial_cc, Initial_D, Initial_dd, Initial_rd, Initial_n, n, m, epsilon)

  DO i = 1, m
    itemp = Initial_basis(i)
    basis(i) = itemp
    IF (itemp >= n) THEN
      CALL print_no_solution()
      STOP
    END IF
  END DO

  CALL print_initial_solution(Initial_basis, Initial_BIb, m)

  CALL simplex_algorithm(A, A_aux, B, bb, basis, BI, BIA_aux, BIb, cb, cbBI, cbBID, cd, C, D, dd, rd, n, m, epsilon)

  CALL bublesort_d(basis, BIb, m)

  CALL print_solution(basis, BIb, C, m)

END PROGRAM simplex