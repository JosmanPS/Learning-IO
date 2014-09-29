setwd("/Users/josmanps/REPOS/Learning-IO")
library(reshape)

# Parámetros
t2 <- data.frame(mean = c(-2.2751, -31.6864, 0.1327, -0.14), income = c(3.5819, 21.6313, -0.2216, 1.442), income2 = c(0, -1.0233, 0, 0), age = c(0.2847, 0, 0.0536, -0.9902), child = c(0, 4.587, 0, 0), sigma = c(0.2581, 2.4317, 0.0075, 0.1238))

# SD's
v1 <- c()
v2 <- c()
v3 <- c()
v4 <- c()
for(i in 1:dim(v)[1]){
  v1 <- c(v1, v[i, 1:20])
  v2 <- c(v2, v[i, 21:40])
  v3 <- c(v3, v[i, 41:60])
  v4 <- c(v4, v[i, 61:80])
}

# Añadimos las SD's a los datos demográficos
demogr <- read.csv("/Users/josmanps/REPOS/Learning-IO/Data/demogr.csv", head = T, row.names = 1)
demogr <- data.frame(demogr, v1, v2, v3, v4)
write.csv(demogr, "demogr.csv")

cereal <- read.csv("/Users/josmanps/REPOS/Learning-IO/Data/cereal.csv", head = T, row.names = 1)

attach(cereal)
attach(demogr)

# ***************************************************************************************************
#We compute the individual probabilities
s_ijt <- matrix(0, 1880, 24)

# Mean Utility
delta <- theta[1,1] + theta[2,1] * price + theta[3,1] * sugar + theta[4,1] * mushy
deltaD <- cast(data.frame(cereal$mkt, cereal$brand, delta), cereal.mkt ~ cereal.brand, value = "delta")
deltaD <- deltaD[,-1]

#Anadimos las variables de los datos de las empresas que son necesarias para la interaccion
#con demograficas.
priceD <- cast(data.frame(cereal$mkt, cereal$brand, cereal$price), cereal.mkt ~ cereal.brand)
sugarD <- cast(data.frame(cereal$mkt, cereal$brand, cereal$sugar), cereal.mkt ~ cereal.brand)
mushyD <- cast(data.frame(cereal$mkt, cereal$brand, cereal$mushy), cereal.mkt ~ cereal.brand)

priceD <- priceD[,-1]
sugarD <- sugarD[,-1] 
mushyD <- mushyD[,-1]

priceD2 <- matrix(0, dim(demogr)[1], dim(priceD)[2])
sugarD2 <- priceD2
mushyD2 <- priceD2
deltaD2 <- priceD2

for(j in 1:dim(priceD2)[2]){
  priceD2[,j] <- rep(priceD[,j], each=20)
  sugarD2[,j] <- rep(sugarD[,j], each=20)
  mushyD2[,j] <- rep(mushyD[,j], each=20)
  deltaD2[,j] <- rep(deltaD[,j], each=20)
}

priceD <- priceD2
sugarD <- sugarD2
mushyD <- mushyD2
deltaD <- deltaD2

rm("priceD2", "sugarD2", "mushyD2", "deltaD2")
# ======

#Calculamos el numerador
for(j in 1:24){
  s_ijt[,j] <- exp(deltaD[,j] + 1*(t2[1,6]*v1 + t2[1,2]*income + t2[1,4]*age) 
                   + priceD[,j]*(t2[2,6]*v2 + t2[2,2]*income + t2[2,3]*sq.income + t2[2,5]*child)
                   + sugarD[,j]*(t2[3,6]*v3 + t2[3,2]*income + t2[3,4]*age)
                   + mushyD[,j]*(t2[4,6]*v4 + t2[4,2]*income + t2[4,4]*age))
}

#Calculamos el denominador
denom <- 1 + rowSums(s_ijt)

#Dividimos para obtener las probabilidades individuales
for(j in 1:24){
  s_ijt[,j] <- s_ijt[,j]/denom
}

# Predict choice
choice <- apply(s_ijt, 1, function(x) (1:24)[x == max(x)])
choice <- as.factor(paste("marca ", choice, sep = ""))

demogr <- data.frame(demogr, choice = choice)
write.csv(demogr, "demogr.csv")
