! solveJumble.f95

MODULE jumbleMod

   TYPE jumbleWord
      CHARACTER(len=:), ALLOCATABLE :: word
      CHARACTER(len=:), ALLOCATABLE :: solvedWord
      CHARACTER(len=8), DIMENSION(:), ALLOCATABLE :: anagrams
      INTEGER :: numAna
   END TYPE jumbleWord

CONTAINS

END MODULE jumbleMod

program solvejumble
   USE lexicon
   USE jumbleMod
   IMPLICIT NONE

   TYPE(jumbleWord), DIMENSION(:), ALLOCATABLE :: jumble
   INTEGER :: numWords, I, J, counter, factResult
   CHARACTER :: solveJumlbeQ
   TYPE(jumbleWord) :: bigJumbledWord
   CHARACTER(len=16), DIMENSION(:), ALLOCATABLE :: tmpString

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

   WRITE(*,*) achar(10), "Generating Anagrams..."
   DO I = 1, numWords
      call factorial(len(jumble(I)%word), factResult)
      allocate(jumble(I)%anagrams(factResult))
      jumble(I)%numAna = 1

      call generateAnagram(jumble(I), 1)
   END DO

   WRITE(*,*) achar(10), "Finding Anagram..."
   DO I = 1, numWords
      call findAnagram(jumble(I))
      WRITE(*,*) "   Found anagram for '", jumble(I)%word, "'"
   END DO

   WRITE(*,*) achar(10), "Solved jumbles:"
   DO I = 1, numWords
      print*, "Jumbeled: ", jumble(I)%word, " Solved: ", jumble(I)%solvedWord
   END DO

   WRITE(*,*) achar(10), "Solve word jumble puzzle? (y/N)"
   READ(*,*) solveJumlbeQ

   IF (solveJumlbeQ == 'Y' .or. solveJumlbeQ == 'y') THEN
      allocate(tmpString(numWords))

      WRITE(*,*) "Select the circled letters from the word puzzle"
      DO I = 1, numWords
         WRITE(*,*) jumble(I)%solvedWord
         READ(*,'(A)') tmpString(I)
      END DO

      counter = 0
      DO I = 1, numWords
         DO J = 1, len(tmpString(I))
            IF (tmpString(I)(J:J) /= " ") THEN
               counter = counter + 1
            END IF
         END DO
      END DO

      allocate(CHARACTER(counter) :: bigJumbledWord%word)
      bigJumbledWord%numAna = 1

      DO I = 1, numWords
         DO J = 1, len(tmpString(I))
            IF (tmpString(I)(J:J) /= " ") THEN
               bigJumbledWord%word(bigJumbledWord%numAna:bigJumbledWord%numAna) = tmpString(I)(J:J)
               bigJumbledWord%numAna = bigJumbledWord%numAna + 1
            END IF
         END DO
      END DO

      WRITE(*,*) "Jumbled Word: ", bigJumbledWord%word
      WRITE(*,*) "Solving..."
      allocate(CHARACTER(len(trim(bigJumbledWord%word))) :: bigJumbledWord%solvedWord)
      bigJumbledWord%solvedWord = trim(bigJumbledWord%word)

      call factorial(len(bigJumbledWord%solvedWord), factResult)
      allocate(bigJumbledWord%anagrams(factResult))
      bigJumbledWord%numAna = 1

      call generateAnagram(bigJumbledWord, 1)
      call findAnagram(bigJumbledWord)

      WRITE(*,*) "Solved Word: ", bigJumbledWord%solvedWord

   END IF

   DO I = 1, numWords
      deallocate(jumble(I)%word)
      deallocate(jumble(I)%solvedWord)
   END DO

   deallocate(jumble)

   CALL freeLexicon()
end program solvejumble

subroutine inputJumble(jumbledWord)
   USE lexicon
   USE jumbleMod
   IMPLICIT NONE
   TYPE(jumbleWord), INTENT(out) :: jumbledWord
   CHARACTER(len=128) :: word
   INTEGER :: wordSize

   READ(*,*) word
   wordSize = len(trim(word))

   allocate(CHARACTER(wordSize) :: jumbledWord%word)
   allocate(CHARACTER(wordSize) :: jumbledWord%solvedWord)

   jumbledWord%word = trim(word)
   call toLower(jumbledWord%word)
end subroutine inputJumble

recursive subroutine generateAnagram(currWord, i)
   USE jumbleMod

   TYPE(jumbleWord), INTENT(inout) :: currWord
   integer, intent(in) :: i

   integer :: n
   integer :: j

   n = len(currWord%word)

   if (i == n) then
      currWord%anagrams(currWord%numAna) = currWord%word
      currWord%numAna = currWord%numAna + 1
   else
      do j = i,n
         call swap(currWord%word,i,j)
         call generateAnagram(currWord, i+1)
         call swap(currWord%word,i,j)
      end do
   end if
end subroutine generateAnagram

recursive subroutine findAnagram(currWord)
   USE lexicon
   USE jumbleMod

   TYPE(jumbleWord), INTENT(inout) :: currWord
   INTEGER :: I
   LOGICAL :: wordThere
   CHARACTER(len=:), ALLOCATABLE :: currAnagram

   allocate(CHARACTER(len(currWord%word)) :: currAnagram)


   DO I = 1, size(currWord%anagrams)
      currAnagram = trim(currWord%anagrams(I))
      call findlexicon(currAnagram, wordThere)
      IF (wordThere) then
         currWord%solvedWord = currAnagram
         exit
      END IF
   END DO

   deallocate(currAnagram)

end subroutine findAnagram

subroutine swap(string, i, j)
   IMPLICIT NONE
   character(len=*), intent(inout) :: string
   integer, intent(in) :: i, j

   character(len=1) :: temp

   temp = string(i:i)
   string(i:i) = string(j:j)
   string(j:j) = temp

end subroutine swap

subroutine factorial(n, result)
   integer, intent(in) :: n
   integer, intent(out) :: result

   result = 1
   do i = 2, n
      result = result * i
   end do
end subroutine factorial


