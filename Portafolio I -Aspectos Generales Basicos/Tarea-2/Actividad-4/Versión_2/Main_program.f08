! ====================================================================|
! Programa para construir una configuración regular cúbica.
!                                            UNIDADES REDUCIDAS: SIGMA
! ====================================================================|

PROGRAM CI_CUBICA
    IMPLICIT NONE
        REAL       :: DENS, A, BOXL, DA, XX, YY, ZZ
        INTEGER    :: N, NPS, I, J, L, KK
        REAL, ALLOCATABLE  :: X(:), Y(:), Z(:)

!-------------------- Output data into a file ---------------------------------------|
        open (unit = 10, file = "01confi.dat", status = 'unknown')
! 10: Archivo donde se almacena la configuración.
!------------------------------------------------------------------------------------|

! Solicitar datos de entrada -----------------------------------------|
WRITE(*,*) 'Introduzca el numero de particulas'
    READ(*,*) N
WRITE(*,*) 'Introduzca la densidad reducida'
    READ(*,*) DENS

! Asignar posiciones -------------------------------------------------|
    allocate(X(N))
    allocate(Y(N))
    allocate(Z(N))

! Cálculos preeliminares    
    A=1.0/3.0
    BOXL=(1.0*N/DENS)**A
    NPS=NINT(N**A)
    KK=0
    DA=BOXL/(NPS)

! Información leída
    WRITE(*,*) '-----------------------------------------------------'
    WRITE(*,*) 'Numero de particulas por arista : ', NPS
    WRITE(*,*) 'Longitud de la celda            : ', BOXL
    WRITE(*,*) 'Densidad reducida               : ', DENS
    WRITE(*,*) '-----------------------------------------------------'

! Cálculo de posiciones    
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
! Escribir las coordenadas en un archivo
WRITE(10,*) KK, X(KK), Y(KK), Z(KK)
    END DO
    END DO
    END DO
! Información
    WRITE(*,*) 'El programa ha terminado de manera exitosa. Se ha generado el archivo'
    WRITE(*,*) '"01confi.dat" con la informacion calculada, cuyo formato es:'
    WRITE(*,*) '--------------------------------'
    WRITE(*,*) '   I   |   X   |   Y    |   Z   '
    WRITE(*,*) '--------------------------------'
 END PROGRAM CI_CUBICA