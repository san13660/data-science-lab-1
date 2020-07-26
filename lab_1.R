install.packages("rela")
install.packages("psych")
install.packages("FactoMineR")
install.packages("corrplot")
install.packages("arules")
install.packages("paf")
install.packages("mnormt")
install.packages("factoextra")

library(arules)
library(rela)
library(psych)
library(FactoMineR)
library("dplyr")
library(corrplot)
library(ggplot2)
library(factoextra)



data_training <- read.csv("train.csv", stringsAsFactors = FALSE)

summary(data_training)

# Analisis exploratorio - Variables numericas

data_training_numeric <- select_if(data_training, is.numeric)
data_training_numeric_clean <- na.omit(data_training_numeric)

# Matriz de correlacion

matriz_cor <- cor(data_training_numeric_clean)
corrplot(matriz_cor)

ggplot(data_training_numeric_clean, aes(x = LotArea)) +
  geom_boxplot()

hist(data_training_numeric_clean$LotArea, main = "Histograma a?o", xlab = "A?o")

summary(data_training_numeric_clean$LotArea)

ggplot(data_training_numeric_clean, aes(x = LotArea)) +
  geom_histogram(aes(y = stat(count)), bins = 12, color="black", fill="grey") +
  scale_y_continuous()+
  xlim(c(1300, 30000))

ggplot(data_training_numeric_clean, aes(x = LotArea)) +
  geom_boxplot()+
  xlim(c(1300, 30000))

summary(data_training_numeric_clean$X1stFlrSF)

ggplot(data_training_numeric_clean, aes(x = X1stFlrSF)) +
  geom_histogram(aes(y = stat(count)), bins = 12, color="black", fill="grey") +
  scale_y_continuous()+
  xlim(c(438, 2500))

ggplot(data_training_numeric_clean, aes(x = X1stFlrSF)) +
  geom_boxplot()+
  xlim(c(438, 2500))

summary(data_training_numeric_clean$SalePrice)

ggplot(data_training_numeric_clean, aes(x = SalePrice)) +
  geom_histogram(aes(y = stat(count)), bins = 12, color="black", fill="grey") +
  scale_y_continuous()+
  xlim(c(35311, 450000))

ggplot(data_training_numeric_clean, aes(x = SalePrice)) +
  geom_boxplot()+
  xlim(c(35311, 450000))

summary(data_training_numeric_clean$MoSold)

ggplot(data_training_numeric_clean, aes(x = MoSold)) +
  geom_histogram(aes(y = stat(count)), bins = 12, color="black", fill="grey") +
  scale_y_continuous()+
  xlim(c(1, 12))

ggplot(data_training_numeric_clean, aes(x = MoSold)) +
  geom_boxplot()+
  xlim(c(1, 12))

ggplot(data_training_numeric_clean, aes(x = SalePrice, y = GrLivArea)) +
  geom_point(size=1.6, shape=1)

ggplot(data_training_numeric_clean, aes(x = SalePrice, y = GarageArea)) +
  geom_point(size=1.6, shape=1)


# Analisis de componentes





# Analizar si se puede usar el analisis factorial para formar combinaciones lineales

data_training_numeric_clean_no_factors <- subset(data_training_numeric_clean, select = -c(Id, OverallQual, OverallCond, BsmtFullBath, BsmtHalfBath, FullBath, HalfBath, BedroomAbvGr, KitchenAbvGr, Fireplaces, GarageCars))

data_training_numeric_clean_no_factors <- subset(data_training_numeric_clean, select = c(GrLivArea, LotArea, GarageArea, X1stFlrSF, YearBuilt, YrSold, MoSold, OpenPorchSF, GarageYrBlt, LotFrontage, TotalBsmtSF, SalePrice))

pafDatos<-paf(as.matrix(data_training_numeric_clean_no_factors))
pafDatos$KMO #0.78787
pafDatos$Bartlett #6359.1
summary(pafDatos)

#se muestra la matriz de correlaciÃ³n
cor(data_training_numeric_clean_no_factors,use = "pairwise.complete.obs")
#se puede ver que el dioxido de azufre estÃ¡ correlacionado con 
# las empresas que tienen mÃ¡s de 20 personas trabajando y posiblemente 
# con la poblaciÃ³n del lugar, a su vez estÃ¡s dos Ãºltimas estÃ¡n
#altamente relacionadas entre si

#Esta funciÃ³n normaliza los datos de una vez
compPrinc<-prcomp(data_training_numeric_clean_no_factors, scale = T)
compPrinc

summary(compPrinc)

#Se obtiene el scree plot de las componentes principales.
# Como se ve hacen falta 4 de las 7 componentes para explicar mï¿½s del 80% de la variabilidad
fviz_eig(compPrinc, addlabels = TRUE, ylim = c(0, 80))

# En la siguiente grÃ¡fica se ilustra la calidad de la representaciÃ³n de los componentes en las dos primeras dimensiones.
fviz_pca_var(compPrinc, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

# Reglas de asociacion


data_training_factors <- subset(data_training, select = -c(Alley, Id, MSSubClass, LotFrontage, LotArea, Neighborhood, GarageArea, GarageYrBlt, WoodDeckSF, OpenPorchSF, YearBuilt, EnclosedPorch, X3SsnPorch, YearRemodAdd, MasVnrArea, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, X1stFlrSF, X2ndFlrSF, LowQualFinSF, GrLivArea, ScreenPorch, PoolArea, PoolQC, Fence, MiscFeature, MiscVal, MoSold, YrSold, SalePrice, TotRmsAbvGrd))


data_training_factors <- subset(data_training_factors, select = c(LotShape, LandContour, RoofStyle, Electrical, GarageType, SaleCondition))


data_training_factors <- na.omit(data_training_factors)
data_training_factors <- as.data.frame(lapply(data_training_factors, factor))

reglas<-apriori(data_training_factors, parameter = list(support = 0.2,
                                                       confidence = 0.70,
                                                       target = "rules"))

options(max.print=100000)

inspect(reglas)


#data_training_factors <- subset(data_training, select = c(MSSubClass, MSZoning, Street, LotShape, LandContour, Utilities, LotConfig, LandSlope, Condition1, Condition2, BldgType, HouseStyle, OverallQual, OverallCond, RoofStyle, RoofMatl, Exterior1st, Exterior2nd, MasVnrType, ExterQual, ExterCond, Foundation, OverallQual, OveralCond, ))

