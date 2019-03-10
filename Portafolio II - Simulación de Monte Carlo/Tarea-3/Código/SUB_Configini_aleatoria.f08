!=====================================================================================================|
! Thursday, January 31, 2019  
!   Subrutina para crear configuración inicial aleatoria en una celda dos-dimensional
!   sin traslapes entre partículas. Condición: los centros de las partículas están 
!   dentro de la celda.
!                                                                           UNIDADES REDUCIDAS: SIGMA
!   VARIABLES DE ENTRADA    : N, BOXL, X, Y
!   VARIABLES DE SALIDA     : X, Y
!   DESCRIPCIÓN DE VARIABLES:
!       Físicas     : 
!                       Xij, Yij = distancias en X, Y entre la partícula I y la J
!                             RO = distancia**2 entre la partícula I y la J
!       Contador    : I, J
!       Aleatorias  : R, S números aleatorios entre -1/2 y 1/2
!=====================================================================================================|

SUBROUTINE CONFIGINI(N, BOXL, X, Y)

IMPLICIT NONE
        REAL(KIND=8), INTENT(IN)                :: BOXL
        REAL(KIND=8)                            :: SIGMA, Xij, Yij, RO
        REAL(KIND=8), DIMENSION(N), INTENT(OUT) :: X, Y
        INTEGER, INTENT(IN)                     :: N
        INTEGER                                 :: I, J


! APERTURA DE ARCHIVOS -----------------------------------------------------------------|
    OPEN (UNIT = 10, FILE = "01confi_in.dat", STATUS = 'unknown')


! VALORES Y CÁLCULOS PREELIMINARES -----------------------------------------------------|   
    SIGMA=1.0


! GENERADOR DE NÚMEROS ALEATORIOS ------------------------------------------------------|
    Call init_random_seed()


! GENERAR CONFIGURACIÓN INICIAL (ALEATORIA Y SIN TRASLAPES) ----------------------------|
    DO 10 I=1, N
        2   Call random_number(X(I))
            Call random_number(Y(I))
                X(I)=(X(I) - 0.5)*BOXL
                Y(I)=(Y(I) - 0.5)*BOXL
!               Anotación: X(I), Y(I) están entre -L*/2 y L*/2
                    DO 9 J=1, I-1
                        Xij=X(I)-X(J)
                        Yij=Y(I)-Y(J)
                        RO=Xij**2 + Yij**2 
                        IF (RO<=SIGMA) THEN
                            !WRITE(*,*) 'Hay traslape para:', I, J
                            GO TO 2
                        END IF
                   9 CONTINUE
                    WRITE(10,*) X(I), Y(I)
    10 CONTINUE

! REGRESAR AL PROGRAMA DE DONDE SE LLAMÓ A CONFIGINI -----------------------------------|
RETURN


! FIN DE LA SUBRUTINA ------------------------------------------------------------------|
END SUBROUTINE CONFIGINI
