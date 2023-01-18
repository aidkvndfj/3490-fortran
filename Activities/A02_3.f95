program A02_3
implicit none


    REAL :: T, H, M, B
    REAL, PARAMETER :: G = 981

    B = 0.0

    WRITE(*,*) 'Please input M then H then T'
    READ(*,*) M, H, T
    
    B = H / G * M * (T ** 2)

    WRITE(*,100) B
    100 FORMAT(F0.8)
    
    if (B < 0.003) then
        WRITE(*,*) 'Surging'
    else if (B > 0.068) then
        WRITE(*,*) 'Spilling'
    else 
        WRITE(*,*) 'Plunging'
    end if


end
