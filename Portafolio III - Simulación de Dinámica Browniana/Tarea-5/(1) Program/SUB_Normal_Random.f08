SUBROUTINE AZARG(ISEED, X)
      IMPLICIT NONE
        REAL(KIND=8), INTENT(OUT)   :: X
        REAL(KIND=8)                :: PI, R, S, a, b
        INTEGER                     :: I
        INTEGER, INTENT(IN)         :: ISEED

!   Cálculos preeliminares
    PI = 4.0*ATAN(1.0)
!   Dos números aleatorios con distribución uniforme
    a = REAL(ISEED)
    call random_number(a)
    R = SQRT(-2.0*LOG(a))
    b = REAL(ISEED)
    call random_number(b)
    S = COS(2.0*PI*b)
!   Número aleatorio con distribución gaussiana (o normal)    
    X = R*S

END SUBROUTINE




