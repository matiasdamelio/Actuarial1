library(readxl)
library(dplyr)
options(scipen = 999)

cso80<-read_excel("C:/Users/mamelio/Desktop/cso80.xlsx", sheet = "Sheet1")

#####################################
######### Valores iniciales #########
#####################################

porcentaje <- 1
lx_inicial <- 10000000
qx_inicial <- cso80[,"qx"]*porcentaje
i <- 0.04

#####################################
# Crear tabla con valores iniciales #
#####################################

cso80 <- data.frame(x = seq(0, 99), qx = qx_inicial, lx = lx_inicial)
cso80$dx <- cso80$lx*cso80$qx 
for(j in 2:nrow(cso80)){
  
  cso80$lx[j] <- cso80$lx[j-1] - cso80$dx[j-1]
  cso80$dx[j] <- cso80$lx[j]*cso80$qx[j]
  
}
cso80$lx <- round(cso80$lx)
cso80$dx <- round(cso80$dx)

#####################################

# D(x)
cso80$Dx <- round(cso80$lx*(1+i)^-cso80$x)

# N(x)
cso80$Nx <- rev(cumsum(rev(cso80$Dx)))

# S(x)
cso80$Sx <- rev(cumsum(rev(cso80$Nx)))

# C(x)
cso80$Cx <- round(cso80$dx*(1+i)^-(cso80$x+1))

# M(x)
cso80$Mx <- rev(cumsum(rev(cso80$Cx)))

# R(x)
cso80$Rx <- rev(cumsum(rev(cso80$Mx)))


#####################################
### Probabilidad de Supervivencia ###
#####################################

#p(x,t)
surv <- function (x, t = 1, k = NA){
  if (is.na(k)) {
    probabilities = cso80[cso80$x %in% c(x, x + t),]  %>% summarise(ratio = lx[2]/lx[1])
  }
  else {
    probabilities = cso80[cso80$x %in% c(x, x + t, x + t + k), ]  %>% summarise(ratio = (lx[2] - lx[3])/lx[1])
  }
  probabilities %>% unlist
}

#####################################
##### Capital Diferido de Vida ######
#####################################

#E(x,t)
faa <- function(x, t){
  aux = data.frame("Conmutacion" = NA, "Exacta" = NA)
  aux$Conmutacion <- cso80[which(cso80$x == (x+t)), "Dx"]/cso80[which(cso80$x == x), "Dx"]
  aux$Exacta <- ((1/(1+i))^t)*surv(x,t)
  return(aux)
}


#####################################
##### Capital Diferido de Muerte ####
#####################################

#A(x,t,1)
A_muerte <- function(x, t){
  aux = data.frame(Conmutacion = NA, Exacta = NA, Relacion = NA)
  aux$Conmutacion <- cso80[which(cso80$x == (x+t)), "Cx"]/cso80[which(cso80$x == x), "Dx"]
  aux$Exacta <- ((1/(1+i))^(t+1))*surv(x,t)*cso80[which(cso80$x == (x+t)), "qx"]
  aux$Relacion <- (1/(1+i))*faa(x,t) - faa(x,t+1)
  return(aux)
}



