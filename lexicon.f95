MODULE lexicon

   TYPE dictWord
      CHARACTER(len=128) :: word
      TYPE(dictWord), POINTER :: next => null()
   END TYPE dictWord

   TYPE(dictWord), POINTER :: head, tmp

CONTAINS

   SUBROUTINE buildlexicon()
      ALLOCATE(head)
      head%word = "Hello"
      ! WRITE(*,*) head%word

      ALLOCATE(tmp)
      tmp%word = "bye"
      ! WRITE(*,*) tmp%word

      head%next => tmp

   END SUBROUTINE buildLexicon

   SUBROUTINE findLexicon(isThere)
      LOGICAL, INTENT(out) :: isThere
      isThere = .false.

      tmp => head


      DO
         WRITE(*,*) tmp%word

         isThere = associated(tmp%next)
         IF (isThere .eqv. .false.) THEN
            exit
         ELSE
            tmp => tmp%next
         END IF
      END DO

   END SUBROUTINE findLexicon

   SUBROUTINE freeLexicon()
      LOGICAL :: isNotNull

      tmp = head%next
      DEALLOCATE(head)

      DO
         isNotNull = associated(tmp%next)
         IF (isNotNuLL .eqv. .false.) THEN
            exit
         END IF
         tmp = head
         head = head%next
         DEALLOCATE(head)
      END DO
   END SUBROUTINE freeLexicon

END MODULE lexicon
