PROGRAM PRESION
    IMPLICIT NONE
        REAL(KIND=8), ALLOCATABLE       :: n(:), asa(:), asd(:), PHS(:)
        REAL(KIND=8)                    :: PS, PI, DA, PE
        INTEGER                         :: I, ND

!   PS = Presión aproximada de potencial de pozo cuadrado
!   PE = Presión exacta del potencial de pozo cuadrado usando la ecuación de estado
!   DA = Derivada de a*(n*) con respecto a n*        
        
!   Apertura de archivos        
    OPEN (10, FILE = 'a_star.dat',      STATUS = 'old')
    OPEN (11, FILE = '06g(r).dat',      STATUS = 'old')
    OPEN (12, FILE = '08PSW_UHS.dat',   STATUS = 'unknown')
!   Solicitar información de entrada
    WRITE(*,*) 'Escribe el numero de datos'
        READ(*,*) ND
    WRITE(*,*) 'Introduce el valor de la derivada'
        READ(*,*) DA
!   Cálculos preeliminares    
    PI=4.E0*ATAN(1.E0)
!   Asignar dimensiones    
    allocate(n(ND))
    allocate(asa(ND))
    allocate(asd(ND))
    allocate(PHS(ND))
!   Leer lo referente a a*(n*)
    DO I = 1, ND
        READ(10,*) n(I), asa(I), asd(I), PHS(I)
        PS = PHS(I) - n(I)*n(I)*(asa(I)+n(I)*DA)
        WRITE(12, *) n(I), PS
    END DO
!   Cerrar archivos
    CLOSE(10)
    CLOSE(11)
    CLOSE(12)
!   Terminar programa
END PROGRAM PRESION