program solvejumble
   USE lexicon
   IMPLICIT NONE

   CHARACTER (len=128), dimension(200) :: jumble
   INTEGER :: N, I
   LOGICAL :: wordThere

   WRITE(*,*) "GET TIME!!"
   CALL buildLexicon()
   CALL findLexicon(wordThere)

   WRITE(*,*) "INPUT TIME!!"
   CALL inputJumble(jumble, N)

   do I = 1, N
      WRITE(*,*) jumble(I)
   end do

   CALL freeLexicon()
end program solvejumble

subroutine inputJumble(jumble, N)
   CHARACTER(len=128), dimension(200), intent(out) :: jumble
   INTEGER, intent(out) :: N
   INTEGER :: I

   WRITE(*,*) 'Enter the number of jumbled words: '
   READ(*,*) N
   WRITE(*,*) 'Enter the ', N, ' jumbled words:'

   ! Get each word one at a time and asign it to the correct spot in the array
   do I = 1, N
      READ(*,*) jumble(I)
   end do

end subroutine inputJumble

subroutine findAnagram()

end subroutine findAnagram
