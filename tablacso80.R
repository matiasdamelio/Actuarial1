library(readxl)
library(dplyr)

cso80<-read_excel("C:\\Users\\Matias DAmelio\\Desktop\\cso80.xlsx", sheet = "Sheet1")

#p(x,t)
surv<-function (x, t = 1, k = NA) 
   {
         if (is.na(k)) {
               probabilities = cso80[cso80$x %in% c(x, x + t),]  %>% summarise(ratio = lx[2]/lx[1])
           }
         else {
               probabilities = cso80[cso80$x %in% c(x, x + t, x + t + k), ]  %>% summarise(ratio = (lx[2] - lx[3])/lx[1])
           }
         probabilities %>% unlist
}

i <- 0.04

cso80$Dx <- round(cso80$lx*(1+i)^-cso80$x)

cso80$Nx <- rev(cumsum(rev(cso80$Dx)))

cso80$Sx <- rev(cumsum(rev(cso80$Nx)))

cso80$Cx <- round(cso80$dx*(1+i)^-(cso80$x+1))

cso80$Mx <- rev(cumsum(rev(cso80$Cx)))

cso80$Rx <- rev(cumsum(rev(cso80$Mx)))
