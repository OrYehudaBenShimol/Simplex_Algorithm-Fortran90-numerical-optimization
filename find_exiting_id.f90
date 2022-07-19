MODULE FindExitingIdModule 
  IMPLICIT none
  CONTAINS

! Assuming y = B-1D, x = B-1b

SUBROUTINE find_exiting_id(d, y, x, enter_id, n, m, min_index)
  INTEGER, DIMENSION(128) :: d
  DOUBLE PRECISION, DIMENSION(128) :: x
  DOUBLE PRECISION, DIMENSION(128, 128) :: y
  INTEGER :: enter_id, n, m, min_index

  INTEGER :: i, j, init_flag, q
  DOUBLE PRECISION :: temp_min, temp

  DO i = 1, n
    IF (d(i) == enter_id) THEN
      q = i
    END IF
  END DO

  init_flag = 0
  DO i = 1, m
    WRITE (*,'("y["I2"]["I2"] = "F12.6", x["I2"] = "F12.6)') i - 1, q - 1, y(i, q), i - 1, x(i)
    WRITE (*,'("init_flag ="I2)') init_flag
    IF (y(i, q) > 0.0) THEN
      temp = x(i) / y(i, q)
      WRITE (*,'("i ="I2", temp = "F12.6)') i - 1, temp
      IF (init_flag == 0) THEN
        temp_min = temp
        min_index = i
        init_flag = 1
      ELSE 
        IF (temp < temp_min) THEN
          temp_min = temp
          min_index = i
        END IF 
      END IF
    END IF
    WRITE (*,'("temp_min_index  ="I2", temp_min  = "F12.6)') min_index - 1, temp_min
  END DO

END SUBROUTINE find_exiting_id

END MODULE FindExitingIdModule