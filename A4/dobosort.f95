program Dobosort
    implicit none
    integer :: N, I, J, INC, TEMP, K, Q
    integer, dimension(50001) :: A
    character(50) :: FILENAME_IN, FILENAME_OUT
    integer :: IN_FILE, OUT_FILE, STATUS
    character(1) :: END_OF_FILE

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
    ! Perform Dobosort algorithm
    ! First loop
    Q = 1
    do while (Q <= n-1)
    inc = Q
    do i = 1, n-inc
    if (A(i) > A(i + inc)) then
        A(i) = A(i) + A(i + inc)
        A(i + inc) = A(i) - A(i + inc)
        A(i) = A(i) - A(i + inc)
    endif
    end do
    Q = Q + 1
    end do

    ! Second loop
    Q = n-1
    do while (Q > 0)
    k = 0
    do i = 1, Q
    if (A(i) > A(i+1)) then
        A(i) = A(i) + A(i+1)
        A(i+1) = A(i) - A(i+1)
        A(i) = A(i) - A(i+1)
        k = i
    endif
    end do
    Q = k - 1
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
end program Dobosort
