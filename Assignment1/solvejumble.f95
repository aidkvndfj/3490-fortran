! solveJumble.f95

MODULE jumbleMod
   ! word: the inputted jumlbed work
   ! solvedWord: the found solution
   ! anagrams: a list of all the permutations for the word
   ! numAna: a tracker for numbers of anagrams

   TYPE jumbleWord
      CHARACTER(len=:), ALLOCATABLE :: word
      CHARACTER(len=:), ALLOCATABLE :: solvedWord
      CHARACTER(len=8), DIMENSION(:), ALLOCATABLE :: anagrams
      INTEGER :: numAna
   END TYPE jumbleWord

CONTAINS

END MODULE jumbleMod

PROGRAM solvejumble
   USE lexicon
   USE jumbleMod
   IMPLICIT NONE

   ! jubmle: a list of the words
   ! numwords: the numebrs of inputted jumbled words
   ! I/J/counter: all different counters, I J for loops, and counter for general counting
   ! factResults: var to store the result from my facotrial subroutine
   ! bigJumbleWord: a jumblwWord to store the circled chars from the jumbled words
   ! tmpstring: a temp string for getting circled chars
   TYPE(jumbleWord), DIMENSION(:), ALLOCATABLE :: jumble
   INTEGER :: numWords, I, J, counter, factResult
   CHARACTER :: solveJumlbeQ
   TYPE(jumbleWord) :: bigJumbledWord
   CHARACTER(len=16), DIMENSION(:), ALLOCATABLE :: tmpString

   WRITE(*,*) "Building Lexicon..."
   CALL buildlexicon()

   WRITE(*,*) "How many words are there?"
   READ(*,*) numWords

   ALLOCATE(jumble(numWords)) ! allocate enough memory for jumbled words

   WRITE(*,*) "Enter the ", numWords, " jumbled words"
   DO I = 1, numWords ! repeat numwords time, getting words from user
      CALL inputJumble(jumble(I))
   END DO

   WRITE(*,*) achar(10), "Generating Anagrams..."
   DO I = 1, numWords ! repeat for every word, generate all the different permuations
      CALL factorial(len(jumble(I)%word), factResult)
      ALLOCATE(jumble(I)%anagrams(factResult))
      jumble(I)%numAna = 1

      CALL generateAnagram(jumble(I), 1)
   END DO

   WRITE(*,*) achar(10), "Finding Anagram..."
   DO I = 1, numWords ! loop through each word and search through all the anagrams untill we find one in the dict
      CALL findAnagram(jumble(I))
      WRITE(*,*) "   Found anagram for '", jumble(I)%word, "'"
   END DO

   WRITE(*,*) achar(10), "Solved jumbles:" ! print the solved word
   DO I = 1, numWords
      print*, "Jumbeled: ", jumble(I)%word, " Solved: ", jumble(I)%solvedWord
   END DO

   WRITE(*,*) achar(10), "Solve word jumble puzzle? (y/N)"
   READ(*,*) solveJumlbeQ

   IF (solveJumlbeQ == 'Y' .or. solveJumlbeQ == 'y') THEN ! only find the big jumble if the user wants
      ALLOCATE(tmpString(numWords))

      WRITE(*,*) "Select the circled letters from the word puzzle" ! get the circled letters for each word
      DO I = 1, numWords
         WRITE(*,*) jumble(I)%solvedWord
         READ(*,'(A)') tmpString(I)
      END DO

      counter = 0
      DO I = 1, numWords ! count number of letters
         DO J = 1, len(tmpString(I))
            IF (tmpString(I)(J:J) /= " ") THEN
               counter = counter + 1
            END IF
         END DO
      END DO

      ! allocate the space for the bigword
      ALLOCATE(CHARACTER(counter) :: bigJumbledWord%word)
      bigJumbledWord%numAna = 1

      ! Put the chars into big word
      DO I = 1, numWords
         DO J = 1, len(tmpString(I))
            IF (tmpString(I)(J:J) /= " ") THEN
               bigJumbledWord%word(bigJumbledWord%numAna:bigJumbledWord%numAna) = tmpString(I)(J:J)
               bigJumbledWord%numAna = bigJumbledWord%numAna + 1
            END IF
         END DO
      END DO

      ! Allocate space for solvedword
      WRITE(*,*) "Jumbled Word: ", bigJumbledWord%word
      WRITE(*,*) "Solving..."
      ALLOCATE(CHARACTER(len(trim(bigJumbledWord%word))) :: bigJumbledWord%solvedWord)
      bigJumbledWord%solvedWord = trim(bigJumbledWord%word)

      ! get the number of permutations
      CALL factorial(len(bigJumbledWord%solvedWord), factResult)
      ALLOCATE(bigJumbledWord%anagrams(factResult))
      bigJumbledWord%numAna = 1

      ! generate permutations and find the correct anagram
      CALL generateAnagram(bigJumbledWord, 1)
      CALL findAnagram(bigJumbledWord)

      WRITE(*,*) "Solved Word: ", bigJumbledWord%solvedWord

   END IF

   ! free the stuff

   DO I = 1, numWords
      DEALLOCATE(jumble(I)%word)
      DEALLOCATE(jumble(I)%solvedWord)
   END DO

   DEALLOCATE(jumble)

   CALL freeLexicon()
END PROGRAM solvejumble

SUBROUTINE inputJumble(jumbledWord)
   USE lexicon
   USE jumbleMod
   IMPLICIT NONE

   TYPE(jumbleWord), INTENT(out) :: jumbledWord
   CHARACTER(len=128) :: word ! inputted wordd
   INTEGER :: wordSize

   READ(*,*) word
   wordSize = len(trim(word))

   ! allocate space for the word in the type
   ALLOCATE(CHARACTER(wordSize) :: jumbledWord%word)
   ALLOCATE(CHARACTER(wordSize) :: jumbledWord%solvedWord)

   !set the words
   jumbledWord%word = trim(word)
   CALL toLower(jumbledWord%word)
   jumbledWord%solvedWord = jumbledWord%word
END SUBROUTINE inputJumble

recursive SUBROUTINE generateAnagram(currWord, i)
   USE jumbleMod

   TYPE(jumbleWord), INTENT(inout) :: currWord
   INTEGER, intent(in) :: i

   INTEGER :: n
   INTEGER :: j

   n = len(currWord%word)

   IF (i == n) THEN ! if we went through the whole word, add it to the anagrams
      currWord%anagrams(currWord%numAna) = currWord%word
      currWord%numAna = currWord%numAna + 1
   ELSE ! go through and swap letters
      DO j = i,n
         CALL swap(currWord%word,i,j)
         CALL generateAnagram(currWord, i+1)
         CALL swap(currWord%word,i,j)
      END DO
   END IF
END SUBROUTINE generateAnagram

recursive SUBROUTINE findAnagram(currWord)
   USE lexicon
   USE jumbleMod

   TYPE(jumbleWord), INTENT(inout) :: currWord
   INTEGER :: I
   LOGICAL :: wordThere
   CHARACTER(len=:), ALLOCATABLE :: currAnagram

   ALLOCATE(CHARACTER(len(currWord%word)) :: currAnagram)

   ! loop through each anagram and see if it is in the dictionary
   DO I = 1, size(currWord%anagrams)
      currAnagram = trim(currWord%anagrams(I))
      CALL findlexicon(currAnagram, wordThere)
      IF (wordThere) THEN ! if the word is there, set the solved word to curr anagram and exit the subroutine
         currWord%solvedWord = currAnagram
         exit
      END IF
   END DO

   DEALLOCATE(currAnagram)

END SUBROUTINE findAnagram

SUBROUTINE swap(string, i, j)
   IMPLICIT NONE
   CHARACTER(len=*), intent(inout) :: string
   INTEGER, intent(in) :: i, j

   CHARACTER(len=1) :: temp

   temp = string(i:i)
   string(i:i) = string(j:j)
   string(j:j) = temp

END SUBROUTINE swap

SUBROUTINE factorial(n, result)
   INTEGER, intent(in) :: n
   INTEGER, intent(out) :: result

   result = 1
   DO i = 2, n
      result = result * i
   END DO
END SUBROUTINE factorial


