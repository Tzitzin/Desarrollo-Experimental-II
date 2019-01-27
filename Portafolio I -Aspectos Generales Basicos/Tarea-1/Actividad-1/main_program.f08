!====================================================================|
! Saturday, January 19, 2019                                         
! Tarea 1 - Actividad 1                                              
! Descripción:  Este programa evalua una función real de una         
!               variable y muestra el resultado en pantalla.         
!               Es posible también, generar una tabla de la forma    
!               x , f(x) para x∈[xi,xf].   
! Por WyL Félix                           
!====================================================================|

PROGRAM     functions
    REAL :: xi, xf
    INTEGER :: n, i, choose
    REAL, EXTERNAL  :: f

!--------------------Output data into a file-------------------------|
open (unit = 1, file = "data_file.dat", status = 'unknown')
!--------------------------------------------------------------------|

WRITE (*,*) 'Este programa evalua la funcion f(x)=aLn(b+cx), con'
WRITE (*,*) 'a, b, c constantes reales y xeR. Elige una opcion:'
WRITE (*,*) '    1 Para evaluar f(x) para x=x0 arbitrario.'
WRITE (*,*) '    2 Para evaluar f(x) para cada valor de xe[xi,xf]'
WRITE (*,*) 'separados por un paso de tamano (xf-xi)/n, con neZ+ '
WRITE (*,*) 'dada por el usuario.'
    READ (*,*) choose 
        IF (choose==1) THEN 
            WRITE (*,*) 'Introduce el valor de a, b, c y x'
                READ (*,*) a, b, c, x
            WRITE (*,*) 'f(',x,')=', f(a,b,c,x)
        ELSE IF (choose==2) THEN 
        WRITE (*,*) 'Introduce el valor de a, b, c'
        READ (*,*) a, b, c
            WRITE (*,*) 'Introduce el valor de x, xi, xf y n para'
            WRITE (*,*) 'obtener f(x) y una serie de puntos sobre f(x)'
                READ (*,*) x, xi, xf, n
                res= abs((abs(xf)-abs(xi)))/n
                    IF ((x>=xi) .AND. (x<=xf)) THEN
                        DO, i=0, n
                            WRITE(1,*) xi+i*res, F(a,b,c,xi+i*res)
                        END DO
                        WRITE (*,*) 'Se ha generado el archivo "data_file.dat"'
                        WRITE (*,*) 'utilizando la informacion anterior'
                        WRITE (*,*) 'La funcion evaluada en x=', x, 'es'
                        WRITE (*,*) 'f(',x,')=', f(a,b,c,x)
                    ELSE
                            WRITE(*,*) 'x/e[xi,xf]'
                    END IF 
        ELSE 
            WRITE (*,*) 'Escoge una de las opciones escribiendo 1 o 2'
        END IF
END PROGRAM functions

!--------------------Function to evaluate----------------------------|
REAL FUNCTION f(a,b,c,x)
  IMPLICIT NONE
  REAL, INTENT (IN) :: a, b, c,  x
  F=a*LOG(b+c*x)
END FUNCTION f