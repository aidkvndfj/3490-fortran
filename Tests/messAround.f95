program messArouond
implicit none

    real :: a, b, c, theta
    REAL, PARAMETER :: PI = 3.1415926


    write (*,*) 'Enter the length of the hypotenuse C: '
    read (*,*) c
    write (*,*) 'Enter the angle theta in degrees: '
    read (*,*) theta
    a = c * cos(theta * PI/180.0)
    b = c * sin(theta * PI/180.0)
    write (*,*) 'The length of the adjacent side is ', a
    write (*,*) 'The length of the opposite side is ', b


end