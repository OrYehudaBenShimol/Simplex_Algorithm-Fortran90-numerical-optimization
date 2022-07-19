MODULE BubbleSortModule
  IMPLICIT none
  CONTAINS
        
SUBROUTINE bublesort(array, n)
  INTEGER :: n
  INTEGER, DIMENSION(128) :: array  
  INTEGER :: i, j, limit, flag, temp

  i = 1
  flag = 1
  DO WHILE ((i <= n) .AND. (flag == 1))
    flag = 0
    limit = n - i
    DO j = 1, limit
      IF (array(j) > array(j + 1)) THEN
        flag = 1
        temp = array(j)
        array(j) = array(j + 1)
        array(j + 1) = temp
      END IF
    END DO
    i = i + 1
  END DO
END SUBROUTINE bublesort

END MODULE BubbleSortModule