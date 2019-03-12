#Tarea Econometria
#Alejandra Campo Archbold

rm(list=ls())

A = matrix(c(0.3, 0.1, -0.2, -0.1, 0.05, -0.02, 0.03, 
             1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
             0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 
             0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0), 7, 7,byrow = TRUE)

eigen(A)

B = matrix(c(0.5, 0.2, -0.3, 1, 0, 0, 0, 1, 0), 3, 3, byrow = T)

eigen(B)

C = matrix(c(0.7, 0.2, 1, 0), 2, 2, byrow = T)

eigen(C)

F1 = matrix(c(0.3, 0.2 , 0.1, -0.2,  1, 0, 0, 0, 0, 1, 0, 0,  0, 0, 1, 0 ), 4, 4, byrow = T)
eigen(F1)
