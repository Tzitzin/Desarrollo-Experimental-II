!====================================================================================|
! Thursday, January 31, 2019  
!   Subrutina para crear configuración inicial aleatoria en una celda dos-dimensional
!   sin traslapes entre partículas. Condición: los centros de las partículas están 
!   dentro de la celda.
!                                                           UNIDADES REDUCIDAS: SIGMA
!   VARIABLES DE ENTRADA    : N, BOXL, X, Y
!   VARIABLES DE SALIDA     : X, Y
!   DESCRIPCIÓN DE VARIABLES:
!       Físicas     : 
!                       Xij, Yij = distancias en X, Y entre la partícula I y la J
!                             RO = distancia**2 entre la partícula I y la J
!       Contador    : I, J
!       Aleatorias  : R, S números aleatorios entre -1/2 y 1/2
!====================================================================================|

SUBROUTINE CONFIGINI(N, BOXL, X, Y)

IMPLICIT NONE
        REAL, INTENT(IN)                :: BOXL
        INTEGER, INTENT(IN)             :: N
        REAL                            :: SIGMA, R, S, Xij, Yij, RO
        INTEGER                         :: I, J
        REAL, DIMENSION(N), INTENT(OUT) :: X, Y

!-------------------- Output data into a file ---------------------------------------|
        open (unit = 10, file = "01confi_in.dat", status = 'unknown')
! 10: Archivo donde se almacena la configuración inicial.
!------------------------------------------------------------------------------------|

! Preeliminares ---------------------------------------------------------------------|
    SIGMA=1.0

! Genedador de números aleatorios ---------------------------------------------------|
    Call init_random_seed()

! Ubicar cada partícula "aleatoriamente" y sin traslape
    DO 10 I=1, N
        2   Call random_number(X(I))
            Call random_number(Y(I))
                R=X(I)-0.5 
                S=Y(I)-0.5
                    X(I)=R*BOXL
                    Y(I)=S*BOXL
! ANOTACIÓN: X(I), Y(I) están entre -L*/2 y L*/2
            DO 9 J=1, I-1
                Xij=X(I)-X(J)
                Yij=Y(I)-Y(J)
                    RO=Xij**2 + Yij**2 
                        IF (RO<=SIGMA) THEN
                            !WRITE(*,*) 'Hay traslape para:', I, J
                            GO TO 2
                        END IF
            9 CONTINUE
        WRITE(10,*) I, X(I), Y(I)
    10 CONTINUE

RETURN
END SUBROUTINE CONFIGINI
