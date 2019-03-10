!=====================================================================================================|
! Subrutina para el cálculo de la energía de la configuración inicial. 
! Incluye: 
!           1. Condiciones periódicas
!           2. Condición de imagen mínima.
!=====================================================================================================|
SUBROUTINE SUMUP (N, RCUT, BOXL, V, X, Y)
    IMPLICIT NONE
    REAL(KIND=8), INTENT(IN)                :: RCUT, BOXL
    REAL(KIND=8), DIMENSION(N), INTENT(IN)  :: X, Y
    REAL(KIND=8), INTENT(OUT)               :: V
    REAL(KIND=8), PARAMETER                 :: PI = 3.1415927
    REAL(KIND=8)                            :: RXI, RXIJ, RIJSQ, VIJ, RYI, RYIJ
    INTEGER, INTENT(IN)                     :: N
    INTEGER                                 :: I, J 
 

! INICIAR ENERGÍA EN CERO (0.0) --------------------------------------------------------|
    V = 0.0


! CÁLCULO DE ENERGÍA -------------------------------------------------------------------|
        DO 100 I = 1, N-1
            RXI = X(I)
            RYI = Y(I)
                DO 99 J =I+1, N
                    RXIJ = RXI - X(J)
                    RYIJ = RYI - Y(J)
!                       Condición de imágen mínima: 
                        RXIJ = RXIJ - BOXL*ANINT( RXIJ/BOXL )
                        RYIJ = RYIJ - BOXL*ANINT( RYIJ/BOXL )
                        RIJSQ = SQRT(RXIJ * RXIJ + RYIJ * RYIJ)
!                       Nos interesa lo que esté dentro del círculo RIJSQ<RCUT
                        IF ( RIJSQ < RCUT ) THEN
!                           Modelo de potencial de interacción (HD) 
                            IF (RIJSQ<=1.0)THEN
                                VIJ=1.0E+10
                            ELSE
                                VIJ=0.0
                            END IF
!                           Cálculo del potencial
                            V = V + VIJ
                        END IF
                99 CONTINUE
        100 CONTINUE

        
! FIN DE LA SUBRUTINA ------------------------------------------------------------------|
END SUBROUTINE SUMUP