!=====================================================================================================|
!   Subrutina para crear configuración inicial regular en una celda dos-dimensional
!   sin traslapes entre partículas. Condición: los centros de las partículas están 
!   dentro de la celda.
!                                                                           UNIDADES REDUCIDAS: SIGMA
!   VARIABLES DE ENTRADA    : N, BOXL, X, Y
!   VARIABLES DE SALIDA     : X, Y
!=====================================================================================================|
SUBROUTINE CONFINI_REGULAR_3D(N, BOXL, X, Y, Z)
    IMPLICIT NONE
        REAL(KIND=8), INTENT(IN)                :: BOXL
        REAL(KIND=8), DIMENSION(N), INTENT(OUT) :: X, Y, Z
        REAL(KIND=8)                            :: SIGMA, XX, YY, ZZ, DA
        INTEGER, INTENT(IN)                     :: N
        INTEGER                                 :: I, J, L, NPS, KK


! APERTURA DE ARCHIVOS -----------------------------------------------------------------|
    OPEN (UNIT = 10, FILE = "01confi_in.dat", STATUS = 'unknown')
        
        
! VALORES Y CÁLCULOS PREELIMINARES -----------------------------------------------------|
    SIGMA=1.0
    NPS=NINT(N**(1.0/3.0))
    KK=0
    DA=BOXL/(NPS)


! GENERAR CONFIGURACIÓN INICIAL REGULAR ------------------------------------------------|
    DO I=1, NPS
        YY=+(BOXL/2.0)-(2.0*I-1.0)*DA/2.0
            DO J=1, NPS
                XX=-(BOXL/2.0)+(2.0*J-1.0)*DA/2.0
                    DO L=1, NPS
                        ZZ=+(BOXL/2.0)-(2.0*L-1.0)*DA/2.0
                            KK=KK+1
                            X(KK)=XX
                            Y(KK)=YY
                            Z(KK)=ZZ
!   Escribir las coordenadas en un archivo
                            WRITE(10,*) X(KK), Y(KK), Z(KK)
                    END DO
            END DO
    END DO


! CERRAR ARCHIVOS ----------------------------------------------------------------------|
    CLOSE(10)


! FIN DE LA SUBRUTINA ------------------------------------------------------------------|
END SUBROUTINE CONFINI_REGULAR_3D