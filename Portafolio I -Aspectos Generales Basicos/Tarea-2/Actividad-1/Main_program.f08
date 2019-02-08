!===================================================================================|
! Monday, January 28, 2019  
! Portafolio 1                                     
! Tarea 2 - Actividad 1                                            
! Descripción: Este programa permite calcular una estimación del número pi
!              usando la generación de números aleatorios.
! Por WyL Félix
!===================================================================================|

PROGRAM pi
IMPLICIT NONE
    INTEGER :: i, M, NDC, N
    REAL:: d, x, y, pi_approx, e, ISEED1, ISEED2, R, S
    REAL, PARAMETER :: pi_real=3.1415

!-------------------- Output data into a file --------------------------------------|
    open (unit = 10, file = "pi_approx.dat", status = 'unknown')
!-----------------------------------------------------------------------------------|

WRITE(*,*)  'Introduzca el numero de veces que desea "lanzar los dados"'
READ (*,*)  N

! Genedador de números aleatorios --------------------------------------------------|
Call init_random_seed()

! Semillas -------------------------------------------------------------------------|
ISEED1=457.3
ISEED2=342.8

! Iniciar M y NDC en 0 -------------------------------------------------------------|
M=0
NDC=0

! Iniciar contéo de M y NDC --------------------------------------------------------|
DO i=1, N
    Call random_number(ISEED1)
    Call random_number(ISEED2)
        R=ISEED1
        S=ISEED2
            x=2.0*R-1.0
            y=2.0*S-1.0
! Calcular distancia d del origen al punto (X,Y)
                d=x**2+y**2
! Verificar si el punto se encuentra dentro del círculo de radio 1.0 
                    IF(d<=1.0) THEN
                        M=M+1
                    ELSE 
                        NDC=NDC+1
                    END IF
! Cáculo de pi 
pi_approx=4.0*M/N

! Error relativo
e=ABS((pi_approx-pi_real))/pi_real

! Guardar el cálculo de PI en el archivo "pi_approximation.dat"
    WRITE(10,*) i, pi_approx, e*100, M, NDC
END DO 

! Valor estimado de PI
    WRITE(*,*) '--------------------------------------------------------------------'
    WRITE(*,*) 'la aproximacion de PI, con', N, 'lanzamientos, es:', pi_approx
    WRITE(*,*) '--------------------------------------------------------------------'

    WRITE(*,*) 'El programa ha terminado de manera exitosa. La informacion calculada'
    WRITE(*,*) 'se encuentra en el archivo "pi_approx.dat" con la siguiente estruc-'
    WRITE(*,*) 'tura:'
    WRITE(*,*) '--------------------------------------------------------------------'
    WRITE(*,*) '  Lanzamiento | Pi Approx. | Error relativo (%) | Dentro | Fuera'
    WRITE(*,*) '--------------------------------------------------------------------'

! Cierres --------------------------------------------------------------------------|    
CLOSE(10)

END PROGRAM pi
