! solveJumble.f95

MODULE jumbleMod

   TYPE jumbleWord
      CHARACTER(len=:), ALLOCATABLE :: word
      CHARACTER(len=:), ALLOCATABLE :: solvedWord
   END TYPE jumbleWord

CONTAINS

END MODULE jumbleMod

program solvejumble
   USE lexicon
   USE jumbleMod
   IMPLICIT NONE

   TYPE(jumbleWord), DIMENSION(:), ALLOCATABLE :: jumble
   ! TYPE(jumbleWord) :: jumbledWord
   INTEGER :: numWords, I

   LOGICAL :: wordThere = .false.

   WRITE(*,*) "Building Lexicon..."
   CALL buildlexicon()

   WRITE(*,*) "How many words are there?"
   READ(*,*) numWords

   allocate(jumble(numWords))

   WRITE(*,*) "Enter the ", numWords, " jumbled words"
   DO I = 1, numWords
      call inputJumble(jumble(I))
      jumble(I)%solvedWord = jumble(I)%word
   END DO

   WRITE(*,*) "Anagrams:"
   DO I = 1, numWords
      call generateAnagram(jumble(I)%solvedWord, 1, wordThere)
      IF (wordThere) then
         WRITE(*,*) "JUMBLE: ", jumble(I)%word, " SOLVE: ", jumble(I)%solvedWord
      ELSE
         WRITE(*,*) "JUMBLE: ", jumble(I)%word, " SOLVE: NO SOLUTION"
      END IF
   END DO
   WRITE(*,*) "end anagram"

   ! CALL generateAnagram(jumble(1)%word, jumble(1)%solvedWord, 1, len(jumble(1)%word))

   ! WRITE(*,*) jumble(1)%word
   ! WRITE(*,*) jumble(1)%solvedWord

   CALL freeLexicon()
end program solvejumble

subroutine inputJumble(jumbledWord)
   USE jumbleMod
   TYPE(jumbleWord), INTENT(out) :: jumbledWord
   CHARACTER(len=128) :: word
   INTEGER :: wordSize

   READ(*,*) word
   wordSize = len(trim(word))

   allocate(CHARACTER(wordSize) :: jumbledWord%word)
   allocate(CHARACTER(wordSize) :: jumbledWord%solvedWord)

   jumbledWord%word = trim(word)
end subroutine inputJumble

subroutine findAnagram()

end subroutine findAnagram

recursive subroutine generateAnagram(str,i,wordThere)
   USE lexicon
   character(len=*), intent(inout) :: str
   logical, intent(inout) :: wordThere
   integer, intent(in) :: i
   integer :: n
   integer :: j

   n = len(str)

   if (i == n) then
      call findlexicon(str, wordThere)
   else
      do j = i,n
         call swap(str,i,j)
         call generateAnagram(str,i+1,wordThere)
         if (wordThere .eqv. .true.) then
            exit
         end if
         call swap(str,i,j)
      end do
   end if
end subroutine generateAnagram

subroutine swap(string, i, j)
   character(len=*), intent(inout) :: string
   integer, intent(in) :: i, j

   character(len=1) :: temp

   temp = string(i:i)
   string(i:i) = string(j:j)
   string(j:j) = temp

end subroutine swap

