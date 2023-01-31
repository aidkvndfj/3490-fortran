program A03_2
   implicit none

   INTEGER :: seed

   WRITE(*,*) "input a 6 digit number"
   READ(*,*) seed

   call midsqr(seed)

   WRITE(*,*) seed

end program A03_2

recursive subroutine midsqr(seed)
   INTEGER, intent(inout) :: seed

   seed = seed ** 2

   seed = seed / 1000

   seed = mod(seed, 100000)

end subroutine midsqr
