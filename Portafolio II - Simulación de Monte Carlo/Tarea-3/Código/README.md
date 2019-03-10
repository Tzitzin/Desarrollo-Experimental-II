# Cambios al código segun la actividad

**Actividad 1:** En el archivo *Main_program.f08* sustituir las lineas:


    IF(DENS<=0.75) THEN
        CALL CONFIGINI(N, BOXL, X, Y)
    ELSE
        CALL CONFINI_REGULAR_2D(N, BOXL, X, Y)
    END IF
    
    
por


        CALL CONFINI_REGULAR_2D(N, BOXL, X, Y)
        

**Actividad 2:** En el archivo *SUB_Configini_aleatoria.f08* eliminar las lineas:


     DO 9 J=1, I-1
         Xij=X(I)-X(J)
         Yij=Y(I)-Y(J)
         RO=Xij**2 + Yij**2 
            IF (RO<=SIGMA) THEN
                WRITE(*,*) 'Hay traslape para:', I, J
                GO TO 2
            END IF
     9 CONTINUE


**Actividad 3:** En el archivo *SUB_g(r).f08* cambiar el valor de DELTAR=0.01E0 por DELTAR=0.1E0. 


**Actividad 4:** No hacer modificaciones. 


**Actividad 5:** En el archivo *SUB_g(r).f08* agregar las siguientes lineas después de calcular la g(r),


! APERTURA DE ARCHIVOS -----------------------------------------------------------------|


    OPEN (50, FILE = "06g(r).dat", STATUS = "unknown")


! IDENTIFICAR g(1+) (g(r) DE CONTACTO) Y CALCULAR LA PRESIÓN ---------------------------|


    DO I= 1, MAXBIN
        READ(50,*) gX(i), gY(i)
            IF(gY(i)/=0.0) THEN 
            PSS = 1.0 + (1.0/2.0)*PI*DENS*gY(i)
                WRITE(17,*) DENS, gY(i), PSS
            EXIT
            END IF  
    END DO


Finalmente sustituir PSS = 1.0 + (1.0/2.0)*PI*DENS*gY(i) por *PSS = DENS + (1.0/2.0)*PI*(DENS*DENS)*gY(i)* para la segunda parte de la actividad 5.

