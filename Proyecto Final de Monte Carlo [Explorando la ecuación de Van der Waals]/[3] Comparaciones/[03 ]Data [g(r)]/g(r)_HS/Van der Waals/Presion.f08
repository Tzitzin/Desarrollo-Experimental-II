!=====================================================================================================     
!   PS  = Presión aproximada de potencial de pozo cuadrado
!   PSW = Presión exacta del potencial de pozo cuadrado usando la ecuación de estado
!   DA  = Derivada de a*(n*) con respecto a n*        
        
!   FORMATO DE ARCHIVOS
!       1) 07AS.dat
!               n*  |   a*(n*) con ajuste polinomial    |   a*(n*) con datos    |   Otra cosa    |   Presión de HS
!       2) 08PSW_UHS.dat
!               n*  |   Presión usando aproximación, ecuación (3.15)    |   Presión de Van der Waals                   
!       3) 09GDRSW_CONTACTOS.dat
!               n*  |   g(σ)    |   g(λσ-)   |   g(λσ+)
!       4) 10PSW.dat
!               n*  |   Ecuación de estado de SW                    
!=====================================================================================================
PROGRAM PRESION
    IMPLICIT NONE
        REAL(KIND=8), ALLOCATABLE       :: n(:), asa(:), asd(:), PHS(:), test(:)
        REAL(KIND=8), ALLOCATABLE       :: GDC1(:), GDC2(:), GDC3(:)
        REAL(KIND=8)                    :: PS, PI, DA, PE, PSW, LAMBDA, PSV
        REAL(KIND=8)                    :: a, b, c, d
        INTEGER                         :: I, ND


!   Apertura de archivos --------------------------------------------   
    OPEN (10, FILE = '07AS.dat',                STATUS = 'old')
    OPEN (12, FILE = '08VDW.dat',               STATUS = 'unknown')
    !OPEN (13, FILE = '09GDRSW_CONTACTOS.dat',   STATUS = 'old')
    !OPEN (14, FILE = '10PSW.dat',               STATUS = 'unknown')
!   -----------------------------------------------------------------


!   Solicitar información de entrada ----------------
    WRITE(*,*) 'Introduce el numero de datos'
        READ(*,*) ND
    !WRITE(*,*) 'Introduce el valor de la derivada'
    !    READ(*,*) DA
    !WRITE(*,*) 'Introduce el valor de lambda'
    !    READ(*,*) LAMBDA
!   -------------------------------------------------


!   Cálculos y definiciones preeliminares ------  
    PI = 4.E0*ATAN(1.E0)
    a = 4.32628
    b = 1.42503
    c = 0.791015
    d = 1.13789
    LAMBDA = 1.25
!   --------------------------------------------


!   Asignar dimensiones -------   
    allocate(n(ND))
    allocate(asa(ND))
    allocate(asd(ND))
    allocate(test(ND))
    allocate(PHS(ND))
    allocate(GDC1(ND))
    allocate(GDC2(ND))
    allocate(GDC3(ND))
!   ---------------------------
    

!   -------------------------------------------------------------------------------------------------------
    DO I = 1, ND
!   Cálculo de la presión usando aproximación, ecuación (3.15)        
        READ(10,*) n(I), asd(I), PHS(I)
        !READ(10,*) n(I), asa(I), asd(I), PHS(I), test(I)
        !PS = PHS(I) - n(I)*n(I)*(asa(I)+n(I)*(-(a*( n(I)-b)/c**2)*EXP(-(( n(I)-b)*( n(I)-b)/(2.0*c**2)))))
        PSV = n(I)/(1.0-((2*PI*n(I))/3.0)) - n(I)*n(I)*asd(I)
        WRITE(12, *) n(I), PSV

!   Cálculo de la presión usando ecuación exacta para SW.
        !READ(13,*) n(I), GDC1(I), GDC2(I), GDC3(I)
        !PSW = n(I)+(2.0/3.0)*PI*n(I)*n(I)*(GDC1(I)-(LAMBDA**3)*(GDC2(I)-GDC3(I)))
        !WRITE(14,*) n(I), PSW
    END DO
!   -------------------------------------------------------------------------------------------------------
    

!   Cerrar archivos --
    CLOSE(10)
    CLOSE(12)
    CLOSE(13)
    CLOSE(14)
!   ------------------
    

!   Terminar programa
END PROGRAM PRESION