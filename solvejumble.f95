program solvejumble
   USE lexicon
   IMPLICIT NONE

   TYPE jumbleWord
      CHARACTER(len=128) :: word
      INTEGER, DIMENSION(128) :: circledLetters
   END TYPE jumbleWord


   TYPE(jumbleWord), DIMENSION(:), ALLOCATABLE :: jumble
   INTEGER :: N, I
   LOGICAL :: wordThere

   WRITE(*,*) "Building Lexicon..."
   CALL buildLexicon()

   WRITE(*,*) "INPUT TIME!!"
   CALL inputJumble(jumble, N)

   WRITE(*,*) "FIND TIME!!"
   CALL findLexicon("zyzomys", wordThere)

   WRITE(*,*) wordThere


   do I = 1, N
      WRITE(*,*) jumble(I)
   end do

   CALL freeLexicon()
end program solvejumble

subroutine inputJumble(jumble, N)
   TYPE(jumbleWord), DIMENSION(:), ALLOCATABLE, INTENT(out) :: jumble
   INTEGER, intent(out) :: N
   INTEGER :: I

   WRITE(*,*) 'Enter the number of jumbled words: '
   READ(*,*) N
   WRITE(*,*) 'Enter the ', N, ' jumbled words:'

   allocate(jumble(N))

   ! Get each word one at a time and asign it to the correct spot in the array
   do I = 1, N
      READ(*,*) jumble(I)
   end do

end subroutine inputJumble

subroutine findAnagram()

end subroutine findAnagram
