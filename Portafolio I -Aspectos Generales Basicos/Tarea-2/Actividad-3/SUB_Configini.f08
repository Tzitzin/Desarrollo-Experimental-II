!=========================================================================|
! Thursday, January 31, 2019  
! Subrutina para crear configuración inicial aleatoria en una celda tridi-
! mensional "sin traslapes". 
! Condición: los centros de las partículas se están dentro de la celda
!=========================================================================|

SUBROUTINE CONFIGINI(BOXL, N)

IMPLICIT NONE
        REAL, INTENT(IN)        :: BOXL
        INTEGER, INTENT(IN)     :: N
        REAL                    :: SIGMA, R, S, Xij, Yij, RO, ISEED1, ISEED2
        INTEGER                 :: I, J
        REAL, ALLOCATABLE       :: X(:), Y(:)

!-------------------- Output data into a file --------------------------S1|
        open (unit = 10, file = "confi_in.dat", status = 'unknown')
!-----------------------------------------------------------------------E1|

! Allocate positions -----------------------------------------------------|
  allocate(X(N))
  allocate(Y(N))

! Genedador de números aleatorios ----------------------------------------|
  Call init_random_seed()

! Semillas ---------------------------------------------------------------|
    SIGMA=1.0
    ISEED1=456.0
    ISEED2=784.0

DO 10 I=1, N
2   Call random_number(ISEED1)
    Call random_number(ISEED2)
    R=ISEED1-0.5
    S=ISEED2-0.5
        X(I)=R*BOXL
        Y(I)=S*BOXL
            DO 9 J=1, I-1
                Xij=X(I)-X(J)
                Yij=Y(I)-Y(J)
                RO=Xij**2 + Yij**2 
                        IF (RO<=SIGMA) THEN
                            !WRITE(*,*) 'Hay traslape para:', I, J
                            GO TO 2
                        END IF
            9 CONTINUE
                WRITE(10,*) I, SNGL(X(I)), SNGL(Y(I))
10 CONTINUE

RETURN
END SUBROUTINE CONFIGINI
