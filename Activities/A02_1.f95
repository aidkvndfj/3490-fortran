program A02_1

    REAL :: HALF_TIME, TIME, MATERIAL_AT_TIME, DECAY, INITIAL_MATERIAL
    REAL, PARAMETER :: E = 2.718281828459045

    DECAY = 0.0
    INITIAL_MATERIAL = 0.0

    WRITE(*,*) 'Please Input Half Time'
    READ(*,*) HALF_TIME
    WRITE(*,*) 'Please Input Time'
    READ(*,*) Time
    WRITE(*,*) 'Please Input Material at time'
    READ(*,*) MATERIAL_AT_TIME

    DECAY = log(2.0_4) / HALF_TIME
    INITIAL_MATERIAL = MATERIAL_AT_TIME * E ** (DECAY * TIME)

    WRITE(*,100) INITIAL_MATERIAL
    100 FORMAT('Initial Weight: ', F8.3)

end