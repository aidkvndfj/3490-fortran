program bubbleSort
    implicit none
    integer :: N, I, J, INC, TEMP
    integer, dimension(50001) :: A
    character(50) :: FILENAME_IN, FILENAME_OUT
    integer :: IN_FILE, OUT_FILE, STATUS

    real :: start, finish

    ! Prompt user for input file name
    write(*,*) "Enter input file name: "
    read(*,*) FILENAME_IN

    ! Open input file and read integers into array A
    open(unit=IN_FILE, file=FILENAME_IN, status='old', action='read')
    if (STATUS /= 0) then
        write(*,*) "Error opening input file."
        stop
    end if
    N = 0
    do
        read(IN_FILE, *, iostat=STATUS) A(N+1)
        if (STATUS /= 0) exit
        N = N + 1
        if (N > 50000) then
            write(*,*) "Error: too many numbers in input file."
            stop
        end if
    end do
    close(IN_FILE)

    call cpu_time(start)
    ! Perform modified bubblesort algorithm
    do I = 1, N-1
        do J = 1, N-I
            if (A(J) > A(J+1)) then
                TEMP = A(J)
                A(J) = A(J+1)
                A(J+1) = TEMP
            end if
        end do
    end do
    call cpu_time(finish)

    print '("Time = ",f10.6," seconds.")',finish-start

    ! Open output file and write sorted integers
    open(unit=OUT_FILE, file="sortedF.txt", status='new', action='write', iostat=STATUS)
    do I = 1, N
        write(OUT_FILE, *) A(I)
    end do
    close(OUT_FILE)

    ! Print success message
    write(*,*) "Sorting complete. Results written to sortedF.txt."

end program bubbleSort
