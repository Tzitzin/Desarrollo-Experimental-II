PROGRAM PROMEDIO
        IMPLICIT NONE
            INTEGER                     :: I, ISAVE, J, NBIN, TOTAL, N, MAXBIN, REZI
            REAL(KIND=8)                :: MEDIA, SUMA, DELTAR, BOXL, DENS, LECTURA
            REAL(KIND=8), ALLOCATABLE   :: RT(:), GDRTA(:)


! APERTURA DE ARCHIVOS ---------------------------------------------------------------------------|            
    OPEN (20, FILE = '06g(r).dat',      STATUS = 'old') 
    OPEN (21, FILE = '08MEDIA_g(r).dat',   STATUS = 'unknown')


! LECTURA DE DATOS DE ENTRADA --------------------------------------------------------------------|            
    WRITE(*,*) 'Introduce el numero de particulas (N)'
        READ(*,*) N
    WRITE(*,*) 'Introduce la densidad'
        READ(*,*) DENS
    WRITE(*,*) 'Introduce desde donde se hara el calculo (en terminos de RCUT)'
        READ(*,*) LECTURA
    WRITE(*,*) 'De cuanto en cuanto (ISAVE)'
        READ(*,*) ISAVE


! ASIGNAR DIMENSIONES ----------------------------------------------------------------------------|
    allocate(RT(MAXBIN))
    allocate(GDRTA(MAXBIN))


! VALORES Y CÁLCULOS PREELIMINARES ---------------------------------------------------------------|
    BOXL = ((1.0*N)/DENS)**(1.0/3.0) 
    DELTAR = 0.0005E0
    MAXBIN = INT(BOXL/2.E0/DELTAR)
    NBIN = INT(MAXBIN*LECTURA)
    TOTAL=(MAXBIN-NBIN)/ISAVE
!   Condiciones iniciales    
    SUMA=0.0


! LECTURA DEL ARCHIVO ----------------------------------------------------------------------------|  
    DO I=1, MAXBIN
        READ(20,*) RT(I), GDRTA(I)
    END DO


! CALCULAR LA SUMA -------------------------------------------------------------------------------|
    DO J=0, TOTAL
        SUMA=SUMA+ABS(GDRTA(NBIN+J*ISAVE))
    END DO

    MEDIA=SUMA/total

    WRITE(*,*) MEDIA
    WRITE(21,*) 'El valor promedio de la g(r*) para r*>', MAXBIN*DELTAR*LECTURA, 'es', MEDIA
    WRITE(21,*) 'El número de datos de la g(r*) es MAXBIN = ', MAXBIN
    

! CERRAR ARCHIVOS --------------------------------------------------------------------------------|
    CLOSE(20)
    CLOSE(21)


! FIN DEL PROGRAMA -------------------------------------------------------------------------------|
END PROGRAM PROMEDIO        
