!==================================================================================================================|
! Subrutina para el cálculo de la energía de la configuración inicial. 
! MODELO: DGM
! Incluye: 
!           1. Condiciones periódicas
!           2. Condición de imagen mínima.
!==================================================================================================================|
SUBROUTINE SUMUP (N, RCUT, BOXL, DENS, TS, KAPPA, V, P, X, Y, Z)
    IMPLICIT NONE
        REAL(KIND=8), INTENT(IN)                :: RCUT, BOXL, KAPPA, TS, DENS
        REAL(KIND=8), DIMENSION(N), INTENT(IN)  :: X, Y, Z
        REAL(KIND=8), INTENT(OUT)               :: V, P
        REAL(KIND=8), PARAMETER                 :: PI = 3.1415927
        REAL(KIND=8)                            :: RXI, RYI, RZI, RXIJ, RYIJ, RZIJ, R, VIJ
        REAL(KIND=8)                            :: PIJ, VSTAR
        INTEGER, INTENT(IN)                     :: N
        INTEGER                                 :: I, J 


! CÁLCULOS PREELIMINARES -------------------------------------------------------------------------------------------|
    VSTAR=BOXL**3

    
! INICIAR ENERGÍA EN CERO (0.0) ------------------------------------------------------------------------------------|
    V = 0.0
    P = 0.0 


! CÁLCULO DE ENERGÍA -----------------------------------------------------------------------------------------------|
        DO 100 I = 1, N-1
            RXI = X(I)
            RYI = Y(I)
            RZI = Z(I)
                DO 99 J = I+1, N
                    RXIJ = RXI - X(J)
                    RYIJ = RYI - Y(J)
                    RZIJ = RZI - Z(J) 
!   Condición de imágen mínima: 
                        RXIJ = RXIJ - BOXL*ANINT( RXIJ/BOXL )
                        RYIJ = RYIJ - BOXL*ANINT( RYIJ/BOXL )
                        RZIJ = RZIJ - BOXL*ANINT( RZIJ/BOXL )
                        R = SQRT(RXIJ**2 + RYIJ**2 + RZIJ**2)
!   Nos interesa lo que esté dentro del círculo R<RCUT
                        IF ( R < RCUT ) THEN
                        ! Cálculo de la energía potencial 
                        VIJ = (1.0/TS)*EXP(-R*R)-KAPPA*EXP(-(R-3)*(R-3))
                        ! Cálculo de la presión
                        PIJ = (-1.0/(3.0*VSTAR))*((-2.0*R*R/TS)*EXP(-R*R)+(2.0*R*(R-3)*KAPPA)*EXP(-(R-3)*(R-3)))
!   Cálculo del potencial y presión (acumulado)               
                        V = V + VIJ
                        P = P + PIJ
                        END IF
                99 CONTINUE
        100 CONTINUE
P = DENS + P
        
! FIN DE LA SUBRUTINA ----------------------------------------------------------------------------------------------|
END SUBROUTINE SUMUP