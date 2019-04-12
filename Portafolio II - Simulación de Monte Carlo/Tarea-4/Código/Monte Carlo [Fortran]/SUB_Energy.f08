!=====================================================================================================|
! Subrutina para el cálculo de la energía de cada configuración.
! MODELO: DGM
! Incluye: 
!           1. Condiciones periódicas
!           2. Condición de imagen mínima.
!=====================================================================================================|
SUBROUTINE ENERGY (N, BOXL, DENS, KAPPA, TS, XI, YI, ZI, I, RCUT, V, P, X, Y, Z)
    IMPLICIT NONE
        REAL(KIND=8), INTENT(IN)                :: XI, YI, ZI, RCUT, BOXL, KAPPA, TS, DENS
        REAL(KIND=8), DIMENSION(N), INTENT(IN)  :: X, Y, Z
        REAL(KIND=8), INTENT(OUT)               :: V, P
        REAL(KIND=8)                            :: XIJ, YIJ, ZIJ, R, VIJ, PIJ, VSTAR
        INTEGER, INTENT(IN)                     :: N, I
        INTEGER                                 :: J


! CÁLCULOS PREELIMINARES -----------------------------------------------------------------------------|
    VSTAR=BOXL**3

! INICIAR ENERGÍA Y PRESIÓN EN CERO ------------------------------------------------------------------|
    V = 0.0
    P = 0.0


! INICIAR EL CÁLCULO DE ENERGÍA ENTRE PARES DE PARTÍCULAS --------------------------------------------|    
    DO J=1, N
        IF(I /= J) THEN 
            XIJ = XI - X(J)
            YIJ = YI - Y(J)
            ZIJ = ZI - Z(J)
!   Condición de imágen mínima
            XIJ = XIJ - BOXL*ANINT(XIJ/BOXL)
            YIJ = YIJ - BOXL*ANINT(YIJ/BOXL)
            ZIJ = ZIJ - BOXL*ANINT(ZIJ/BOXL)
            R = SQRT(XIJ**2+YIJ**2+ZIJ**2)
!   Modelo de potencial de interacción (DGM)
            IF(R < RCUT) THEN
            ! Cálculo de la energía potencial 
                VIJ = (1.0/TS)*EXP(-R*R)-KAPPA*EXP(-(R-3)*(R-3))
            ! Cálculo de la presión
                PIJ = (-1.0/(3.0*VSTAR))*((-2.0*R*R/TS)*EXP(-R*R)+(2.0*R*(R-3)*KAPPA)*EXP(-(R-3)*(R-3)))
!   Cálculo del potencial y presión (acumulado)               
                V = V + VIJ
                P = P + PIJ
            END IF
        END IF
    END DO
P = DENS + P

! FIN DE LA SUBRUTINA --------------------------------------------------------------------------------|    
END SUBROUTINE ENERGY