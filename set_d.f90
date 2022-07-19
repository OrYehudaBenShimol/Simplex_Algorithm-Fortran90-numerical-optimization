MODULE SetDModule
  IMPLICIT none
  CONTAINS

SUBROUTINE set_d(basis, d, n, m)
  INTEGER :: n, m
  INTEGER, DIMENSION(128) :: basis, d
  INTEGER :: i, j, pos, flag

  DO i = 1, m
    d(i) = basis(i)
  END DO

  pos = m + 1
  DO i = 1, n
    flag = 1
    j = 1
    DO WHILE ((j <= m) .AND. (flag == 1))
      IF (i == basis(j) + 1) THEN  
        flag = 0
      END IF
      j = j + 1
    END DO

    IF (flag == 1) THEN
      d(pos) = i - 1
      pos = pos + 1
    END IF
  END DO

END SUBROUTINE set_d

END MODULE SetDModule
