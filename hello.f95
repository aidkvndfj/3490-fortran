program hello
implicit none


    REAL :: num1, num2, num3, total, divideRes

    INTEGER :: numberOfNumbers

    numberOfNumbers = 3
    total = 0.0

    write (*,*) 'Please input number 1'
    read (*,*) num1
    write (*,*) 'Please input number 2'
    read (*,*) num2
    write (*,*) 'Please input number 3'
    read (*,*) num3

    total = num1 + num2 + num3

    divideRes = total/numberOfNumbers

    write (*,100)  divideRes
    100 format('All 3 divided by 3 equals ', F10.3)
    
end
