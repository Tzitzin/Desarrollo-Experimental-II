!============================================================================================|
! Thursday, January 31, 2019  
! Subrutina para crear configuración inicial aleatoria en una celda tres-dimensional "sin 
! traslapes". 
! Condición: los centros de las partículas se están dentro de la celda
!============================================================================================|

SUBROUTINE CONFIGINI(N, BOXL, X, Y, Z)

IMPLICIT NONE
        REAL, INTENT(IN)                :: BOXL
        INTEGER, INTENT(IN)             :: N
        REAL, DIMENSION(N), INTENT(OUT) :: X, Y, Z
        REAL                            :: SIGMA, R, S, T, Xij, Yij, Zij, RO
        REAL                            :: ISEED1, ISEED2, ISEED3
        INTEGER                         :: I, J

!-------------------- Output data into a file -----------------------------------------------|
        open (unit = 10, file = "confi_in.dat", status = 'unknown')
!--------------------------------------------------------------------------------------------|

! Semillas ----------------------------------------------------------------------------------|
    SIGMA=1.0
    ISEED1=456.0
    ISEED2=784.0
    ISEED3=7267.2

! Genedador de números aleatorios -----------------------------------------------------------|
    Call init_random_seed()

DO 10 I=1, N
2   Call random_number(ISEED1)
    Call random_number(ISEED2)
    Call random_number(ISEED3)
    R=ISEED1-0.5
    S=ISEED2-0.5
    T=ISEED3-0.5
        X(I)=R*BOXL
        Y(I)=S*BOXL
        Z(I)=T*BOXL
            DO 9 J=1, I-1
                Xij=X(I)-X(J)
                Yij=Y(I)-Y(J)
                Zij=Z(I)-Z(J)
                RO=Xij**2 + Yij**2 + Zij**2
                        IF (RO<=SIGMA) THEN
                            !WRITE(*,*) 'Hay traslape para:', I, J
                            GO TO 2
                        END IF
            9 CONTINUE
                WRITE(10,*) I, SNGL(X(I)), SNGL(Y(I)), SNGL(Z(I))
10 CONTINUE

RETURN
END SUBROUTINE CONFIGINI
