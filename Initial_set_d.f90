MODULE InitialSetDModule
  IMPLICIT none
  CONTAINS

SUBROUTINE Initial_set_d(Initial_basis, Initial_d, Initial_n, m)
  INTEGER :: Initial_n, m
  INTEGER, DIMENSION(128) :: Initial_basis, Initial_d
  INTEGER :: i, j, pos, flag

  DO i = 1, m
    Initial_d(i) = Initial_basis(i)
  END DO

  pos = m + 1
  DO i = 1, Initial_n
    flag = 1
    j = 1
    DO WHILE ((j <= m) .AND. (flag == 1))
      IF (i == Initial_basis(j) + 1) THEN  
        flag = 0
      END IF
      j = j + 1
    END DO

    IF (flag == 1) THEN
      Initial_d(pos) = i - 1
      pos = pos + 1
    END IF
  END DO

END SUBROUTINE Initial_set_d

END MODULE InitialSetDModule
