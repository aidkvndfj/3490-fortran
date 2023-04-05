program Quicksort
    implicit none
    integer :: N, I
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
    
    !Sort the array
    call quicksort_rec(A, 1, N)

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

contains

    recursive subroutine quicksort_rec(a, l, r)
        implicit none

        integer, dimension(:) :: a
        integer :: l, r, i, j, pivot, temp

        if (r > l) then
            pivot = a(l)
            i = l + 1
            j = r

            do while (i <= j)
            do while (i <= r .and. a(i) <= pivot)
                i = i + 1
            end do

            do while (j >= l+1 .and. a(j) > pivot)
                j = j - 1
            end do

            if (i < j) then
                ! Swap a(i) and a(j)
                temp = a(i)
                a(i) = a(j)
                a(j) = temp
            end if
            end do

            ! Swap a(l) and a(j)
            temp = a(l)
            a(l) = a(j)
            a(j) = temp

            ! Recursively sort the two sub-arrays
            call quicksort_rec(a, l, j-1)
            call quicksort_rec(a, j+1, r)
    end if

    end subroutine quicksort_rec
end program Quicksort
