# Universidad del Valle de Guatemala
# Data Science 1 - Seccion 10
# Christopher Sandoval
# Fernanda Estrada
# Luis Delgado
# 25/07/2020


# Instalacion de paquetes
install.packages("rela")
install.packages("psych")
install.packages("FactoMineR")
install.packages("corrplot")
install.packages("arules")
install.packages("paf")
install.packages("mnormt")
install.packages("factoextra")
install.packages("treemap")

# Importacion de paquetes
library(arules)
library(rela)
library(psych)
library(FactoMineR)
library("dplyr")
library(corrplot)
library(ggplot2)
library(factoextra)
library('treemap')


# Importar datos
data_training <- read.csv("train.csv", stringsAsFactors = FALSE)

# Vista rapida de los datos
summary(data_training)


# --------------- Analisis exploratorio - Variables numericas ---------------

data_training_numeric <- select_if(data_training, is.numeric)
data_training_numeric_clean <- na.omit(data_training_numeric)

# Matriz de correlacion
matriz_cor <- cor(data_training_numeric_clean)
corrplot(matriz_cor)

# Variable LotArea
summary(data_training_numeric_clean$LotArea)
# Histograma
ggplot(data_training_numeric_clean, aes(x = LotArea)) +
  geom_histogram(aes(y = stat(count)), bins = 12, color="black", fill="grey") +
  scale_y_continuous()+
  xlim(c(1300, 30000))
# Caja y bigotes
ggplot(data_training_numeric_clean, aes(x = LotArea)) +
  geom_boxplot()+
  xlim(c(1300, 30000))

# Variable X1stFlrSF
summary(data_training_numeric_clean$X1stFlrSF)
# Histograma
ggplot(data_training_numeric_clean, aes(x = X1stFlrSF)) +
  geom_histogram(aes(y = stat(count)), bins = 12, color="black", fill="grey") +
  scale_y_continuous()+
  xlim(c(438, 2500))
# Caja y bigotes
ggplot(data_training_numeric_clean, aes(x = X1stFlrSF)) +
  geom_boxplot()+
  xlim(c(438, 2500))

# Variable SalePrice
summary(data_training_numeric_clean$SalePrice)
# Histograma
ggplot(data_training_numeric_clean, aes(x = SalePrice)) +
  geom_histogram(aes(y = stat(count)), bins = 12, color="black", fill="grey") +
  scale_y_continuous()+
  xlim(c(35311, 450000))
# Caja y Bigotes
ggplot(data_training_numeric_clean, aes(x = SalePrice)) +
  geom_boxplot()+
  xlim(c(35311, 450000))

# Variable MoSold
summary(data_training_numeric_clean$MoSold)
# Histograma
ggplot(data_training_numeric_clean, aes(x = MoSold)) +
  geom_histogram(aes(y = stat(count)), bins = 12, color="black", fill="grey") +
  scale_y_continuous()+
  xlim(c(1, 12))
# Caja y bigotes
ggplot(data_training_numeric_clean, aes(x = MoSold)) +
  geom_boxplot()+
  xlim(c(1, 12))

# Grafico dispersion SalePrice vs GrLivArea
ggplot(data_training_numeric_clean, aes(x = SalePrice, y = GrLivArea)) +
  geom_point(size=1.6, shape=1)

# Grafico dispersion SalePrice vs GarageArea
ggplot(data_training_numeric_clean, aes(x = SalePrice, y = GarageArea)) +
  geom_point(size=1.6, shape=1)


# --------------- Analisis exploratorio - Variables categoricas ---------------

# Graficos de barras
barplot(table(train$MSSubClass), ylim=c(0,600))
barplot(table(train$YrSold), ylim=c(0,350))
barplot(table(train$MoSold), ylim=c(0,300))
barplot(table(train$OverallQual), ylim=c(0,500))

# Graficos de pie
pie(table(train$Alley), labels = c("Gravel","Pavement"))
pie(table(train$BsmtQual),labels=c("Excelent","Fair","Good","Average/Typical"))
pie(table(train$BldgType),labels = c("Single-Family detatched","Two-family Conversion","Duplex","Townhouse Inside Unit","Townhouse End Unit"))
pie(table(train$HeatingQC), labels = c("Excelent","Fair","Good","Poor","Average/Typical"))

# Grafico de proporcion
treemap(as.data.frame(table(train$Neighborhood)),index=c("Var1"),vSize = c("Freq"))

# Tablas de frecuencia
table(train$Utilities)
table(train$LotConfig)
table(train$Heating)
table(train$GarageType)


# --------------- Analisis de componentes ---------------

# Analizar si se puede usar el analisis factorial para formar combinaciones lineales
data_training_numeric_clean_no_factors <- subset(data_training_numeric_clean, select = -c(Id, OverallQual, OverallCond, BsmtFullBath, BsmtHalfBath, FullBath, HalfBath, BedroomAbvGr, KitchenAbvGr, Fireplaces, GarageCars))
data_training_numeric_clean_no_factors <- subset(data_training_numeric_clean, select = c(GrLivArea, LotArea, GarageArea, X1stFlrSF, YearBuilt, YrSold, MoSold, OpenPorchSF, GarageYrBlt, LotFrontage, TotalBsmtSF, SalePrice))
pafDatos<-paf(as.matrix(data_training_numeric_clean_no_factors))
pafDatos$KMO #0.78787
pafDatos$Bartlett #6359.1
summary(pafDatos)

# Se muestra la matriz de correlacion
cor(data_training_numeric_clean_no_factors,use = "pairwise.complete.obs")

# Normalizar los datos
compPrinc<-prcomp(data_training_numeric_clean_no_factors, scale = T)
compPrinc
summary(compPrinc)

# Se obtiene el scree plot de las componentes principales
fviz_eig(compPrinc, addlabels = TRUE, ylim = c(0, 80))

# Calidad de la representacion de los componentes en las dos primeras dimensiones
fviz_pca_var(compPrinc, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)


# --------------- Reglas de asociacion ---------------
data_training_factors <- subset(data_training, select = -c(Alley, Id, MSSubClass, LotFrontage, LotArea, Neighborhood, GarageArea, GarageYrBlt, WoodDeckSF, OpenPorchSF, YearBuilt, EnclosedPorch, X3SsnPorch, YearRemodAdd, MasVnrArea, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, X1stFlrSF, X2ndFlrSF, LowQualFinSF, GrLivArea, ScreenPorch, PoolArea, PoolQC, Fence, MiscFeature, MiscVal, MoSold, YrSold, SalePrice, TotRmsAbvGrd))
data_training_factors <- subset(data_training_factors, select = c(LotShape, LandContour, RoofStyle, Electrical, GarageType, SaleCondition))
data_training_factors <- na.omit(data_training_factors)
data_training_factors <- as.data.frame(lapply(data_training_factors, factor))

# Soporte 0.2 y Confidence 0.7
reglas<-apriori(data_training_factors, parameter = list(support = 0.2,
                                                       confidence = 0.70,
                                                       target = "rules"))

options(max.print=100000)
inspect(reglas)
