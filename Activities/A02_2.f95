program A02_2

    ! 
    ! D is the distace of the track
    ! T1 is temperature 1
    ! T2 is temperature 2
    ! L is delta distance 
    !

    REAL :: D, T1, T2, L
    REAL, PARAMETER :: ALPHA = 11E-6

    L = 0.0

    WRITE(*,*) "Please input distance, then temp 1, then temp 2"
    READ(*,*) D, T1, T2

    L = ALPHA * D * ABS(T1 - T2)

    ! WRITE(*, *) L
    WRITE(*, 100) L
    100 FORMAT("The change is length is: ", F0.4)
    


end