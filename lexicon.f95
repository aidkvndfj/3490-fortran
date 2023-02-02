! lexicon.f95

MODULE lexicon
   ! The type for the linked list, holds a word and a pointer to the next item in the list
   TYPE dictWord
      CHARACTER(len=128) :: word
      TYPE(dictWord), POINTER :: next => null()
   END TYPE dictWord



   ! global head
   TYPE(dictWord), POINTER :: tmp, head

CONTAINS

   SUBROUTINE buildlexicon()
      CHARACTER(len=12) :: dictLocation = "./dict2.txt"
      CHARACTER(len=10) :: currWord
      INTEGER :: Reason

      !allocate the head
      ALLOCATE(head)
      head%word = ""
      head%next => null()

      ! open the dict in the working directory
      open(unit=100, file=dictLocation, status='old', action='read')

      ! Loop untill at EOF
      DO
         ! read the file file, and store the status to reason
         READ(100,*,IOSTAT=Reason) currWord
         call toLower(currWord) ! make the current word lowercase

         IF (Reason > 0)  THEN ! If > 0 then something went wrong
            WRITE(*,*) "broken"
         ELSE IF (Reason < 0) THEN ! if < 0 then we're at the end of the file
            exit
         ELSE ! we read a line succesfully
            IF (head%word == "") THEN ! if the head is empty (first word), but the word in the head
               head%word = currWord
            ELSE ! otherwise create a new dictWord and add it to the list
               ALLOCATE(tmp)
               tmp%word = currWord
               call addNode(tmp)
            END IF
         END IF
      END DO

   END SUBROUTINE buildlexicon

   SUBROUTINE addNode(newNode)
      TYPE(dictWord), POINTER, INTENT(in) :: newNode
      TYPE(dictWord), POINTER :: tempHead
      LOGICAL :: isNotNull
      tempHead => head

      DO
         isNotNull = associated(tempHead%next)
         IF (isNotNull) THEN
            tempHead => tempHead%next
         ELSE
            exit
         END IF
      END DO

      ! WRITE(*,*) "ADD ", newNode%word, " ", head%word
      tempHead%next => newNode
   END SUBROUTINE

   SUBROUTINE findlexicon(toFind, isThere)
      ! Have 24 linked lists, and search a given list based on the first letter of the input word
      CHARACTER(len=*), INTENT(in) :: toFind
      LOGICAL, INTENT(out) :: isThere
      LOGICAL :: isNotNull
      isThere = .false.

      tmp => head

      DO
         ! WRITE(*,*) tmp%word

         isNotNull = associated(tmp%next)
         IF (trim(tmp%word) == trim(toFind)) THEN
            isThere = .true.
            exit
         END IF
         IF (isNotNull) THEN
            tmp => tmp%next
         ELSE
            exit
         END IF
      END DO
   END SUBROUTINE findlexicon

   SUBROUTINE freeLexicon()
      LOGICAL :: isNotNull

      tmp => head%next
      DEALLOCATE(head)

      DO
         isNotNull = associated(tmp%next)
         IF (isNotNuLL .eqv. .false.) THEN
            exit
         END IF
         head => tmp
         tmp => tmp%next
         DEALLOCATE(head)
      END DO
   END SUBROUTINE freeLexicon

   SUBROUTINE toLower(input)
      CHARACTER(len=*), INTENT(inout) :: input
      INTEGER :: i

      DO i = 1, len(input)
         IF (input(i:i) .GE. 'A' .AND. input(i:i) .LE. 'Z') THEN
            input(i:i) = achar(iachar(input(i:i)) + 32)
         ELSE
            input(i:i) = input(i:i)
         END IF
      END DO
   END SUBROUTINE toLower

END MODULE lexicon
