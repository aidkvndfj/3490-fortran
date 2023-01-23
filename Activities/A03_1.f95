program A03_1
   implicit none

   ! D = deflection (in, m)
   ! P = central load (lbs, N)
   ! L = support span (in, m)
   ! E = modulus of elasticity (psi, N/m2)
   ! I = moment of inertia (in4, m4)
   !
   ! A = Allowed Deflection
   !

   REAL :: D, P, L, E, I
   REAL :: A

   P = 100.0

   WRITE(*,*) "Please input elasticity, inertia, then span."
   READ(*,*) E, I, L

   A = L / 360.0

   CALL calcDeflection(P, L, E, I, D)

   IF (D > A) THEN
      WRITE(*,100) D, A
   ELSE
      WRITE(*,200) D, A
   END IF

100 FORMAT('Deflection 0' F0.3 'in is greater then allowed limit 0' F0.3 'in')
200 FORMAT('Deflection 0' F0.3 'in is less then then allowed limit 0' F0.3 'in')
end

subroutine calcDeflection (P, L, E, I, D)
   REAL, intent(in) :: P, L, E, I
   REAL, intent(out) :: D

   D = (P * (L ** 3)) / (48.0 * E * I)
end subroutine calcDeflection
