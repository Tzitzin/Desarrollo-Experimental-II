!=====================================================================================================|
! Subrutina para el cálculo de la energía de cada configuración. 
! Incluye: 
!           1. Condiciones periódicas
!           2. Condición de imagen mínima.
!=====================================================================================================|
SUBROUTINE ENERGY (N, BOXL, XI, YI, ZI, I, RCUT, V, X, Y, Z, lambda, TS)
    IMPLICIT NONE
        REAL(KIND=8), INTENT(IN)                :: XI, YI, ZI, RCUT, BOXL, lambda, TS
        REAL(KIND=8), DIMENSION(N), INTENT(IN)  :: X, Y, Z
        REAL(KIND=8), INTENT(OUT)               :: V
        REAL(KIND=8)                            :: XIJ, YIJ, ZIJ, RIJSQ, VIJ
        INTEGER, INTENT(IN)                     :: N, I
        INTEGER                                 :: J


! INICIAR ENERGÍA EN CERO (0.0) --------------------------------------------------------|
    V=0.0

! INICIAR EL CÁLCULO DE ENERGÍA ENTRE PARES DE PARTÍCULAS ------------------------------|    
    DO J=1, N
        IF(I /= J) THEN 
            XIJ = XI - X(J)
            YIJ = YI - Y(J)
            ZIJ = ZI - Z(J)
!           Condición de imágen mínima
            XIJ = XIJ - BOXL*ANINT(XIJ/BOXL)
            YIJ = YIJ - BOXL*ANINT(YIJ/BOXL)
            ZIJ = ZIJ - BOXL*ANINT(ZIJ/BOXL)
            RIJSQ = SQRT(XIJ**2+YIJ**2+ZIJ**2)
!           Modelo de potencial de interacción (HD)
            IF(RIJSQ < RCUT) THEN
                IF (RIJSQ<=1.0)THEN
                    VIJ=1.0E+10
                ELSE IF (RIJSQ > 1.0 .AND. RIJSQ < lambda) THEN
                    VIJ = -1.0/TS
                ELSE
                    VIJ=0.0
                END IF
                V = V + VIJ
            END IF
        END IF
    END DO


! FIN DE LA SUBRUTINA ------------------------------------------------------------------|    
END SUBROUTINE ENERGY