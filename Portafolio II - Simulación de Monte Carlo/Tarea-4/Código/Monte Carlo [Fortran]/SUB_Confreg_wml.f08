!=====================================================================================================|
!   Subrutina para crear configuración inicial regular en un cubo (tres dimensiones). Incluye Maria 
!   Luisa: las partículas quedan dentro de la celda totalmente.
!                                                                 UNIDADES REDUCIDAS: SIGMA-BETA (σ, β)
!   VARIABLES DE ENTRADA    : N, BOXL, X, Y, Z
!   VARIABLES DE SALIDA     : X, Y, Z
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
    NPS=NINT(1.0*N**(1.0/3.0))
    KK=0
    DA=(BOXL-1.0)/((1.0*NPS)-1)


! GENERAR CONFIGURACIÓN INICIAL REGULAR ------------------------------------------------|
    DO 10 I=1, NPS
        YY=((BOXL-1)/2.0)-(1.0*I-1.0)*DA
            DO 20 J=1, NPS
                XX=((BOXL-1)/2.0)-(1.0*J-1.0)*DA
                    DO 30 L=1, NPS
                        ZZ=((BOXL-1)/2.0)-(1.0*L-1.0)*DA
                            KK=KK+1
                            X(KK)=XX
                            Y(KK)=YY
                            Z(KK)=ZZ
                        WRITE(10,*) X(KK), Y(KK), Z(KK)
                    30 CONTINUE
            20 CONTINUE
    10 CONTINUE


! CERRAR ARCHIVOS ----------------------------------------------------------------------|
    CLOSE(10)


! FIN DE LA SUBRUTINA ------------------------------------------------------------------|
END SUBROUTINE CONFINI_REGULAR_3D