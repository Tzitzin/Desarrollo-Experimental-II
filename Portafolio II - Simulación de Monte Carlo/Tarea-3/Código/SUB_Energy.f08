!=====================================================================================================|
! Subrutina para el cálculo de la energía de cada configuración. 
! Incluye: 
!           1. Condiciones periódicas
!           2. Condición de imagen mínima.
!=====================================================================================================|
SUBROUTINE ENERGY (N, BOXL, XI, YI, I, RCUT, V, X, Y)
    IMPLICIT NONE
        REAL(KIND=8), INTENT(IN)                :: XI, YI, RCUT, BOXL
        REAL(KIND=8), DIMENSION(N), INTENT(IN)  :: X, Y
        REAL(KIND=8), INTENT(OUT)               :: V
        REAL(KIND=8)                            :: XIJ, YIJ, RIJSQ, VIJ
        INTEGER, INTENT(IN)                     :: N, I
        INTEGER                                 :: J


! INICIAR ENERGÍA EN CERO (0.0) --------------------------------------------------------|
    V=0.0

! INICIAR EL CÁLCULO DE ENERGÍA ENTRE PARES DE PARTÍCULAS ------------------------------|    
    DO J=1, N
        IF(I /= J) THEN 
            XIJ = XI - X(J)
            YIJ = YI - Y(J)
!           Condición de imágen mínima
            XIJ = XIJ - BOXL*ANINT(XIJ/BOXL)
            YIJ = YIJ - BOXL*ANINT(YIJ/BOXL)
            RIJSQ = SQRT(XIJ*XIJ+YIJ*YIJ)
!           Modelo de potencial de interacción (HD)
            IF(RIJSQ < RCUT) THEN
                IF(RIJSQ <= 1.0) THEN 
                    VIJ=1.0E+10
                ELSE
                    VIJ=0.0
                END IF 
                V = V + VIJ
            END IF
        END IF
    END DO


! FIN DE LA SUBRUTINA ------------------------------------------------------------------|    
END SUBROUTINE ENERGY