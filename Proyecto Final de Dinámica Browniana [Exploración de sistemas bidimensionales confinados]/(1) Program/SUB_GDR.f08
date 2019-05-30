! ============================================================================================
! Esta subroutina se emplea para calcular puntos sobre la función de distribución radial g(r).
! Requiere como entrada los arreglos generados en el programa principal (Main program). La in-
! formación aquí incluida se refiere al modelo de potencial: DISCOS DUROS (HD) y las variables
! están reducidas con SIGMA (diámetro de las partículas = 1) y BETA.
!   DESCRIPCIÓN DE VARIABLES:
!       BOXL            = Longitud de la celda (L*).
!       DENS            = Densidad reducida (n*).
!       CX, CY          = Arreglos que contienen posiciones de las partículas.
!       NHIST           = Histograma.
!       GDRTA           = Valores de la g(r).
!       NBIN, MAXBIN    = Lo referente a las entradas del histograma NHIST.
!       KI2             = Tamaño del ensemble.
!       PSS             = Valor de la presión para g(1+), PSS = 1+(1/2)(PI)(n*)g(1+).
!       I, J, L, M      = Contadores.
! ============================================================================================

SUBROUTINE GDR(N, BOXL, DENS, H, NN2, CY, KI2)
    IMPLICIT NONE
        REAL(KIND=8), INTENT(IN)                        :: BOXL, DENS, H
        REAL(KIND=8), DIMENSION(N, NN2), INTENT(IN)     :: CY
        REAL(KIND=8)                                    :: DELTAR, PI, XL0, XLT, XL0T, YL0
        REAL(KIND=8)                                    :: YLT, YL0T, R0T, C1, RL, RU, RT, C2
        REAL(KIND=8)                                    :: GDRTA
        INTEGER, INTENT(IN)                             :: N, NN2, KI2
        INTEGER, ALLOCATABLE                            :: NHIST(:)
        INTEGER                                         :: I, L, M, J, NBIN, MAXBIN, DONDE


! APERTURA DE ARCHIVOS ----------------------------------------------------------------------|
    OPEN (16, FILE = '06g(r).dat',      STATUS = 'unknown')            


! VALORES Y CÁLCULOS PREELIMINARES ----------------------------------------------------------| 
!   Ancho de la 'cinta':
    DELTAR=0.1E0
!   Número máximo de entradas del histograma:
    MAXBIN=INT(BOXL/2.E0/DELTAR)
!   Dimensión del histograma:
    allocate(NHIST(MAXBIN))
!   Iniciar conteo en cero cada entrada del histograma de MAXBIN componentes:
    DO I=1, MAXBIN
        NHIST(I)=0
    END DO
!   Cálculo de PI:
    PI=4.E0*ATAN(1.E0)
!   Constante para el modelo de potencial HD:
    C1=2.0E0*H*DENS     
    DONDE = INT(N/5.0)


! CONTEO DE 'VECINOS' -----------------------------------------------------------------------|
    DO L=1, N
        DO 25 M=1, N
            IF(M == L) GO TO 25
                DO J=1, KI2
!                    XL0 = CX(L,J)
!                    XLT = CX(M,J)
!                        XL0T = XL0-XLT
                    YL0 = CY(L,J)
                    YLT = CY(M,J)
                        YL0T = YL0-YLT
!   Condiciones periódicas:
!                        XL0T = XL0T-BOXL*ANINT(XL0T/BOXL)
                        YL0T = YL0T-BOXL*ANINT(YL0T/BOXL)
!   Cálculo de distancia:                        
                    R0T=SQRT(YL0T**2)
                    NBIN=INT(R0T/DELTAR)+1
!   Protección:                    
                        IF(NBIN<=MAXBIN) THEN
                            NHIST(NBIN)=NHIST(NBIN)+1
                        END IF
                END DO
        25 CONTINUE
        IF(MOD(L, DONDE) == 0) THEN
            !WRITE(*,*) 'Vamos en', L, 'de', N
            WRITE(*,*) L, 'de', N
        END IF
    END DO


! RESULTADO DE CONTEO DE 'VECINOS'-----------------------------------------------------------|
    DO NBIN=1, MAXBIN
        RL=REAL(NBIN-1)*DELTAR
        RU=RL+DELTAR
        RT=RL+DELTAR/2.E0
        C2=C1*(RU-RL)
        GDRTA=REAL(NHIST(NBIN))/REAL(KI2)/REAL(N)/C2
            WRITE(16,*) RT, GDRTA
    END DO


! CERRAR ARCHIVOS ---------------------------------------------------------------------------|
    CLOSE(16)


! FIN DE LA SUBRUTINA -----------------------------------------------------------------------| 
END SUBROUTINE GDR