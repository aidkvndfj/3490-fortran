! solveJumble.f95

MODULE jumbleMod

   TYPE jumbleWord
      CHARACTER(len=:), ALLOCATABLE :: word
      CHARACTER(len=:), ALLOCATABLE :: solvedWord
      INTEGER, DIMENSION(128) :: circledLetters
   END TYPE jumbleWord

CONTAINS

END MODULE jumbleMod

program solvejumble
   USE lexicon
   USE jumbleMod
   IMPLICIT NONE

   TYPE(jumbleWord), DIMENSION(:), ALLOCATABLE :: jumble
   TYPE(jumbleWord) :: jumbledWord
   INTEGER :: numWords, I
   LOGICAL :: wordThere

   WRITE(*,*) "Building Lexicon..."
   CALL buildlexicon()

   WRITE(*,*) "INPUT TIME!!"
   WRITE(*,*) "How many words are there?"
   READ(*,*) numWords

   allocate(jumble(numWords))

   WRITE(*,*) "Enter the ", numWords, " jumbled words"
   DO I = 1, numWords
      call inputJumble(jumble(I))
      ! jumble(i) = jumbledWord
   END DO


   WRITE(*,*) "FIND TIME!!"
   CALL findlexicon("zyzomys", wordThere)

   WRITE(*,*) wordThere

   do I = 1, numWords
      WRITE(*,*) jumble(I)%word
   end do

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
