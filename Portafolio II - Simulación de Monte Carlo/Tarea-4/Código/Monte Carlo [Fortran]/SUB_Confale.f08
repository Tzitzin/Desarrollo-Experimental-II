!=====================================================================================================|
! Thursday, April 11, 2019  
!   Sistema en 3D: X, Y y Z. 
!                                                                 UNIDADES REDUCIDAS: SIGMA-BETA (σ, β) 
!   Subrutina para crear configuración inicial aleatoria en un cubo (tres dimensiones) de lado L*
!   sin traslapes entre partículas. Condición: los centros de las partículas están dentro del cubo.
!       VARIABLES DE ENTRADA    :   N, BOXL
!       VARIABLES DE SALIDA     :   X, Y, Z
!       DESCRIPCIÓN DE VARIABLES:
!           Físicas     : 
!                                   Xij, Yij, Zij = distancias en X, Y, Z entre la partícula I y la J
!                                   R = distancia**2 entre la partícula I y la J
!           Contador    :           I, J
!=====================================================================================================|

SUBROUTINE CONFIGINI(N, BOXL, X, Y, Z)

IMPLICIT NONE
        REAL(KIND=8), INTENT(IN)                :: BOXL
        REAL(KIND=8)                            :: SIGMA, Xij, Yij, Zij, RO
        REAL(KIND=8), DIMENSION(N), INTENT(OUT) :: X, Y, Z
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
            Call random_number(Z(I))
                X(I) = (X(I) - 0.5)*BOXL
                Y(I) = (Y(I) - 0.5)*BOXL
                Z(I) = (Z(I) - 0.5)*BOXL
!   Anotación: X(I), Y(I) y Z(I) están entre -L*/2 y L*/2
                    DO 9 J=1, I-1
                        Xij = X(I) - X(J)
                        Yij = Y(I) - Y(J)
                        Zij = Z(I) - Z(J)
                        RO=Xij**2 + Yij**2 + Zij**2 
                        IF (RO<=SIGMA) THEN
                            !WRITE(*,*) 'Hay traslape para:', I, J
                            GO TO 2
                        END IF
                   9 CONTINUE
                    WRITE(10,*) X(I), Y(I), Z(I)
    10 CONTINUE

! REGRESAR AL PROGRAMA DE DONDE SE LLAMÓ A CONFIGINI -----------------------------------|
RETURN


! FIN DE LA SUBRUTINA ------------------------------------------------------------------|
END SUBROUTINE CONFIGINI
