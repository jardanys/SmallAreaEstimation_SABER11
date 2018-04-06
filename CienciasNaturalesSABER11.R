#********************************************************************
# 0. LIBRERIAS ####
#********************************************************************

rm(list = ls())
library(survey)
library(sae)
library(TeachingSampling)
library(dplyr)
library(progress)
options(scipen = 999)
options(survey.lonely.psu="adjust")

#********************************************************************
# 1. MUESTRA PRUEBAS SABER ####
#********************************************************************

est <- readRDS("./data/estudiantes.rds")
muestraXest <- readRDS("./data/EC1muestraXest.rds")
names(est)

diseno_muestral <- svydesign(ids = ~ CODIGOMUNICIPIO + CODIGO_ICFES + ID_estud,
                             strata = ~estrato_mpio + EstratoColegio,
                             fpc = ~ NI + NII + N_i, data = muestraXest,
                             nest = T)

# Distribución de la variable total en la población
hist(est$CIENCIAS_NATURALES_PUNT, xlab = "Puntaje Ciencias Naturales", ylab = "Frecuencia", main = "Población", col = "orange")
# Distribución de la variable total en la muestra
hist(muestraXest$CIENCIAS_NATURALES_PUNT, xlab = "Puntaje Ciencias Naturales", ylab = "Frecuencia", main = "Muestra", col = "orange")

#********************************************************************
# 2. ESTIMADOR GLOBAL DE LA MEDIA DEL PUNTAJE CIENCIAS NATURALES ####
#********************************************************************

real_mean <- mean(est$CIENCIAS_NATURALES_PUNT)
names(real_mean) <- "CIENCIAS_NATURALES_MEAN"
saveRDS(real_mean, "./rds/real_mean.rds")

#********************************************************************
# 2.1. ESTIMADOR DIRECTO PARA PROMEDIO GLOBAL ####
#********************************************************************

Est_glo_medi_dir <- as.data.frame(svymean(~CIENCIAS_NATURALES_PUNT, diseno_muestral))
Est_glo_medi_dir$cve <- 100 * cv(svymean(~CIENCIAS_NATURALES_PUNT, diseno_muestral))[1,1]
names(Est_glo_medi_dir) <- c("mean", "se", "cve")
saveRDS(Est_glo_medi_dir, "./rds/Est_glo_medi_dir.rds")

#********************************************************************
# 2.2. ESTIMADOR SINTÉTICO PARA PROMEDIO GLOBAL  ####
#********************************************************************

# Estimación
y_barra <- as.data.frame(svytotal(~CIENCIAS_NATURALES_PUNT, diseno_muestral))[,1]
N_d <- length(est$CIENCIAS_NATURALES_PUNT)
Ysynth_d <- y_barra / N_d

# Real
mean(est$CIENCIAS_NATURALES_PUNT)

# Varianza
var_y_barra <- as.data.frame(svymean(~CIENCIAS_NATURALES_PUNT, diseno_muestral))[,2]^2
N_d2 <- length(est$CIENCIAS_NATURALES_PUNT)^2
var_Ysynth_d <- var_y_barra * N_d2

cve <- (sqrt(var_Ysynth_d) / y_barra )* 100
cve

est_mean_sint <- data.frame(mean=Ysynth_d, cve=cve)
row.names(est_mean_sint) <- "CIENCIAS_NATURALES_MEAN"

saveRDS(est_mean_sint, "./rds/est_mean_sint.rds")

#********************************************************************
# 2.3. ESTIMADORDE RAZÓN PARA PROMEDIO GLOBAL ####
#********************************************************************

# La variable auxiliar es PUNTAJE SOCIALES_CIUDADANAS_PUNT porque es la de mayor correlación
# con CIENCIAS NATURALES

corre <- cor(est[,c(18:22)])
corre <- corre[,c("CIENCIAS_NATURALES_PUNT")]
saveRDS(corre, "./rds/corre.rds")

estimaGlobal_ciencias <- as.data.frame(svymean(~CIENCIAS_NATURALES_PUNT, diseno_muestral))
hatY <- estimaGlobal_ciencias[,1]
hatY

estimaGlobal_sociales <- as.data.frame(svymean(~SOCIALES_CIUDADANAS_PUNT, diseno_muestral))
hatX <- estimaGlobal_sociales[,1]
hatX

X_U <- mean(est$SOCIALES_CIUDADANAS_PUNT)

# Estimador de razón
Y_ratio <- hatY * (X_U / hatX)

# Real
mean(est$CIENCIAS_NATURALES_PUNT)

# cve

Var_Y_ratio <- X_U^2 *SE(svyratio(numerator = ~CIENCIAS_NATURALES_PUNT, 
                                  denominator = ~SOCIALES_CIUDADANAS_PUNT, 
                                  diseno_muestral)) ^ 2

cve <- sqrt(Var_Y_ratio) / Y_ratio * 100

est_mean_razon <- data.frame(mean=Y_ratio, cve=cve)

saveRDS(est_mean_razon, "./rds/est_mean_razon.rds")

#********************************************************************
# 2.4. ESTIMADOR POSESTRATIFICADO PARA PROMEDIO GLOBAL ####
#********************************************************************

# El estimador global del promedio posestratificado es el mismo del estimador directo HT

svymean(~CIENCIAS_NATURALES_PUNT, diseno_muestral)
100 * cv(svymean(~CIENCIAS_NATURALES_PUNT, diseno_muestral))
mean(est$INGLES_PUNT)

#********************************************************************
# 2.5. ESTIMADOR GREG PARA PROMEDIO GLOBAL ####
#********************************************************************

cor(est[,c(18:22)])
# Calibrar usando MATEMATICAS_PUNT

muestraXest$fexp <- weights(diseno_muestral)

mod1_mue <- lm(CIENCIAS_NATURALES_PUNT ~ MATEMATICAS_PUNT, data=muestraXest, 
               weights = fexp)

saveRDS(mod1_mue, "./rds/mod1_mue_greg_mean.rds")

summary(mod1_mue)
plot(muestraXest$CIENCIAS_NATURALES_PUNT ~ muestraXest$MATEMATICAS_PUNT, xlab="Matemáticas",
     ylab = "Ciencias Naturales", main = "Ciencias Naturales vs Matemáticas", col="orange")
abline(mod1_mue, col="red")

Xu <- as.numeric(c(nrow(est), sum(est$MATEMATICAS_PUNT)))

diseno_calibrado <- calibrate(diseno_muestral, ~ MATEMATICAS_PUNT, calfun = "linear" , 
                              population = Xu)

muestraXest$pesocalib <- weights(diseno_calibrado)
g_k <- muestraXest$pesocalib / muestraXest$fexp
# Mediana y promedio cercanos a 1
summary(g_k)

est_mean_greg <- as.data.frame(svymean(~CIENCIAS_NATURALES_PUNT, diseno_calibrado))
est_mean_greg$cve <- 100*as.data.frame(cv(svymean(~CIENCIAS_NATURALES_PUNT, diseno_calibrado)))[1,1]

saveRDS(est_mean_greg, "./rds/est_mean_greg.rds")

#********************************************************************
# 2.6. ESTIMADOR HBF PARA PROMEDIO GLOBAL  ####
#********************************************************************

#*********************
# DOMINIO: MUNICIPIO 
#*********************

# Dominio Depto(Cod mpio)
# y_est: Puntaje Ciencias Naturales
# x1: puntaje sociales
# x2: Estrato eneriga
# x3: Naturaleza

# Transformar variables, quitar nulos

# Variables para x2:
est$FINS_ESTRATOVIVIENDAENERGIA[is.na(est$FINS_ESTRATOVIVIENDAENERGIA)] <- 1

# Crear Dumies
Dummies_estrato_energia  <- as.data.frame(Domains(est$FINS_ESTRATOVIVIENDAENERGIA))
est <- cbind(est,Dummies_estrato_energia)

unique(est$NATURALEZA)
Dummies_Naturaleza  <- as.data.frame(Domains(est$NATURALEZA))
est <- cbind(est,Dummies_Naturaleza)

Infoaux <- est %>% group_by(CODIGOMUNICIPIO) %>% 
  summarise(Prom_SOCIALES_CIUDADANAS_PUNT = mean(SOCIALES_CIUDADANAS_PUNT),
            Prop_Estrato1 = mean(`1`),
            Prop_Estrato2 = mean(`2`),
            Prop_Estrato3 = mean(`3`),
            Prop_Estrato4 = mean(`4`),
            Prop_Estrato5 = mean(`5`),
            Prop_Estrato6 = mean(`6`),
            Prop_Naturaleza_No_Oficial = mean(`No oficial`),
            Prop_Naturaleza_Oficial = mean(`Oficial`),
            N_d = n() )

# Convertir en factores las categoricas de la muestra
muestraXest$CALENDARIO <- as.factor(muestraXest$CALENDARIO)
muestraXest$NATURALEZA <- as.factor(muestraXest$NATURALEZA)
muestraXest$JORNADA <- as.factor(muestraXest$JORNADA)
muestraXest$PERS_GENERO <- as.factor(muestraXest$PERS_GENERO)
muestraXest$FINS_ESTRATOVIVIENDAENERGIA <- as.factor(muestraXest$FINS_ESTRATOVIVIENDAENERGIA)
muestraXest$FINS_PISOSHOGAR <- as.factor(muestraXest$FINS_PISOSHOGAR)
muestraXest$FINS_TIENEINTERNET <- as.factor(muestraXest$FINS_TIENEINTERNET)
muestraXest$FINS_TIENECOMPUTADOR <- as.factor(muestraXest$FINS_TIENECOMPUTADOR)
muestraXest$FINS_TIENEAUTOMOVILPARTICULAR <- as.factor(muestraXest$FINS_TIENEAUTOMOVILPARTICULAR)
muestraXest$INGLES_DESEM <- as.factor(muestraXest$INGLES_DESEM)

Tamanos <- Infoaux[,c("CODIGOMUNICIPIO", "N_d")]
names(Infoaux)
Medias <- Infoaux[,c("CODIGOMUNICIPIO", "Prom_SOCIALES_CIUDADANAS_PUNT",
                     "Prop_Estrato2", "Prop_Estrato3", "Prop_Estrato4", "Prop_Estrato5", "Prop_Estrato6", 
                     "Prop_Naturaleza_Oficial")]

Tamanos$CODIGOMUNICIPIO <- as.character(Tamanos$CODIGOMUNICIPIO)
Medias$CODIGOMUNICIPIO <- as.character(Medias$CODIGOMUNICIPIO)
muestraXest$CODIGOMUNICIPIO <- as.character(muestraXest$CODIGOMUNICIPIO)

BHF <- pbmseBHF(CIENCIAS_NATURALES_PUNT ~ SOCIALES_CIUDADANAS_PUNT + FINS_ESTRATOVIVIENDAENERGIA + NATURALEZA, 
                dom = CODIGOMUNICIPIO, 
                meanxpop = Medias,
                popnsize = Tamanos,
                B = 200, data = muestraXest)
#?pbmseBHF
# Estimación para dominios observados
BHF$est$eblup

# Estimación del error cuadrático medio
BHF$mse

# cv
sqrt(BHF$mse$mse) / BHF$est$eblup$eblup * 100


# Estimaciones para dominios externos (que no salieron en la muestra)
Beta_est <- BHF$est$fit$fixed
names(Beta_est) <- gsub("XsXs", "", names(Beta_est) )
names(Beta_est)[1] <-" Intercepto" 
Beta_est <- as.matrix(Beta_est)

# Totales por dominio
x_1 <- "Prom_SOCIALES_CIUDADANAS_PUNT"
x_2 <- c("Prop_Estrato2", "Prop_Estrato3", "Prop_Estrato4", "Prop_Estrato5", "Prop_Estrato6")
x_3 <- c("Prop_Naturaleza_Oficial")

Xbar_d <- Infoaux[ c(x_1, x_2, x_3)]
Unos <- as.data.frame(as.matrix(rep(1, nrow(Infoaux))))
Xbar_d <- cbind(Unos, Xbar_d)
Xbar_d <- as.matrix(Xbar_d)
rownames(Xbar_d) <- Infoaux$CODIGOMUNICIPIO
head(Xbar_d)

Prom_dominios <- Xbar_d %*% Beta_est
rownames(Prom_dominios) <- Infoaux$CODIGOMUNICIPIO
Prom_dominios <- as.data.frame(Prom_dominios)
Prom_dominios$domain <- row.names(Prom_dominios)
colnames(Prom_dominios)[1] <- "Ybar_efectosfijos" 
head(Prom_dominios)


# Conservar los dominios no observados 
Prom_dominios_observados <- BHF$est$eblup
Prom_dominios <- merge(Prom_dominios, Prom_dominios_observados, by = "domain", all.x = T)
names(Prom_dominios)[1] <- "MUNICIPIO"
head(Prom_dominios)
# Estimación MSE para dominios no observados 

library(nlme)
modelo_mixto <- lme(CIENCIAS_NATURALES_PUNT ~ SOCIALES_CIUDADANAS_PUNT + FINS_ESTRATOVIVIENDAENERGIA + NATURALEZA, 
                    random = ~1 | as.factor(CODIGOMUNICIPIO), data = muestraXest)


#\hat{V}(\hat{\bodysymbol{\beta}}):
Varest_betaest <- vcov(modelo_mixto)

# \hat{sigma}^2_u
sigma2est_u <- BHF[[1]]$fit$refvar #29.66217^2 #EN modelo_mixto observese que es la misma estimacion

# Identificar los dominios no observados
dominios_noobservados <- unique(est$CODIGOMUNICIPIO)[!(unique(est$CODIGOMUNICIPIO) %in% unique(muestraXest$CODIGOMUNICIPIO))]
Xbar_d_noobs <- Xbar_d[row.names(Xbar_d) %in% dominios_noobservados,]
MSE_DominiosNoobservados <- diag((Xbar_d_noobs %*% Varest_betaest %*% t(Xbar_d_noobs)) + sigma2est_u)
MSE_DominiosNoobservados <- as.table(MSE_DominiosNoobservados)
df_MSE_DominiosNoobservados <- as.data.frame(MSE_DominiosNoobservados)
names(df_MSE_DominiosNoobservados) <- c("MUNICIPIO", "MSE")
df_MSE_DominiosNoobservados$ClaseDominio <- "No observado"

head(df_MSE_DominiosNoobservados)

df_MSE_Dominiosobservados <- BHF$mse
names(df_MSE_Dominiosobservados) <- c("MUNICIPIO", "MSE")
df_MSE_Dominiosobservados$ClaseDominio <- "Observado"

head(df_MSE_Dominiosobservados)

df_MSE_Dominios <- bind_rows(df_MSE_DominiosNoobservados, df_MSE_Dominiosobservados)
df_MSE_Dominios <- df_MSE_Dominios[order(df_MSE_Dominios$MUNICIPIO),]

# Tienden a dar más MSE los dominios no obsevados
boxplot(MSE ~ ClaseDominio, data = df_MSE_Dominios)

# Resultados finales

Resultados <- merge(Prom_dominios, df_MSE_Dominios, by = "MUNICIPIO")
Resultados$Yhat_BHF <- ifelse(Resultados$ClaseDominio == "No observado", Resultados$Ybar_efectosfijos,
                              Resultados$eblup)
Resultados$cve <- 100 * sqrt(Resultados$MSE) / Resultados$Yhat_BHF

head(Resultados)

est_mean_HBF <- data.frame(mean=mean(Resultados$Yhat_BHF), cve=mean(Resultados$cve))
row.names(est_mean_HBF) <- "CIENCIAS_NATURALES_MEAN"
saveRDS(est_mean_HBF, "./rds/est_mean_HBF.rds")


#**************************************************************************
# 3. ESTIMACIÓN DEL PUNTAJE PROMEDIO CIENCIAS NATURALES POR DOMINIOS ####
#**************************************************************************

real_dominios <- aggregate(CIENCIAS_NATURALES_PUNT ~ NATURALEZA, data=est, FUN=mean)
saveRDS(real_dominios, "./rds/real_dominios.rds")

#********************************************************************
# 3.1. ESTIMADOR DIRECTO DEL PROMEDIO POR DOMINIOS ####
#********************************************************************

#**********************
# Dominio: Naturaleza
#**********************
est_dom_mean_HT <- as.data.frame(svyby(~CIENCIAS_NATURALES_PUNT, ~NATURALEZA, diseno_muestral, FUN = svymean))[,c(2,3)]
est_dom_mean_HT$cve <- 100 * cv(svyby(~CIENCIAS_NATURALES_PUNT, ~NATURALEZA, diseno_muestral, FUN = svymean))

saveRDS(est_dom_mean_HT, "./rds/est_dom_mean_HT.rds")


#********************************************************************
# 3.2. ESTIMADOR SINTÉTICO DEL PROMEDIO POR DOMINIOS ####
#********************************************************************

#*************************
# Dominio: NATURALEZA
#*************************

# Estimación
y_bar <- as.data.frame(svyby(~CIENCIAS_NATURALES_PUNT, ~NATURALEZA, diseno_muestral, FUN=svytotal))[,2]
G <- length(unique(est$NATURALEZA))
Ybarpron <- t(y_bar * t(matrix(1, nrow = G, ncol = 1 )))
N_d <- as.data.frame(sort(table(est$NATURALEZA)))[,2]
Ysynth_d <- y_bar / N_d

# Real
aggregate(CIENCIAS_NATURALES_PUNT ~ NATURALEZA, data=est, FUN=mean)

# Varianza
var_y_barra <- as.data.frame(svyby(~CIENCIAS_NATURALES_PUNT, ~NATURALEZA, diseno_muestral, FUN=svytotal))[,3]^2
VarYbarpron <- t(var_y_barra * t(matrix(1, nrow = G, ncol = 1 )))
N_d2 <- as.data.frame(sort(table(est$NATURALEZA)))[,2]^2
var_Ysynth_d <- var_y_barra / N_d2

cve <- sqrt(var_Ysynth_d)
cve

est_dom_mean_dintetico <- data.frame(Total=Ysynth_d, cve=cve)
rownames(est_dom_mean_dintetico) <- c("No Oficial", "Oficial")

saveRDS(est_dom_mean_dintetico, "./rds/est_dom_mean_dintetico.rds")

#********************************************************************
# 3.3. ESTIMADOR DE RAZÓN PARA PROMEDIO POR DOMINIOS ####
#********************************************************************

# La variable auxiliar es PUNTAJE MATEMÄTICAS porque es la de mayor correlación
# con CIENCIAS NATURALES
#************************
# dominio: Naturaleza
#************************

estimaGlobal_ciencias <- as.data.frame(svyby(~CIENCIAS_NATURALES_PUNT, ~NATURALEZA, diseno_muestral, FUN=svymean))
hatY <- estimaGlobal_ciencias[,2]
names(hatY) <- estimaGlobal_ciencias[,1]

estimaGlobal_sociales <- as.data.frame(svyby(~SOCIALES_CIUDADANAS_PUNT,  ~NATURALEZA, diseno_muestral, FUN=svymean))
hatX <- estimaGlobal_sociales[,2]
names(hatX) <- estimaGlobal_sociales[,1]

X_U <- aggregate(SOCIALES_CIUDADANAS_PUNT ~ NATURALEZA, FUN = mean, data = est)
X_U_ <- sort(X_U$SOCIALES_CIUDADANAS_PUNT, decreasing = T)
names(X_U_) <- estimaGlobal_sociales[,1]

# Estimador de razón
Y_ratio <- hatY * (X_U_ / hatX)

# Real
aggregate(CIENCIAS_NATURALES_PUNT ~ NATURALEZA, FUN = mean, data = est)

# cve
Var_Y_ratio <- X_U_^2 * SE(svyby(~CIENCIAS_NATURALES_PUNT, by=~NATURALEZA,
                                  denominator = ~SOCIALES_CIUDADANAS_PUNT, 
                                  diseno_muestral, FUN=svyratio))^2

cve <- sqrt(Var_Y_ratio) / Y_ratio * 100

est_dom_tot_razon <- data.frame(Total=Y_ratio, cve=cve)

saveRDS(est_dom_tot_razon, "./rds/est_dom_tot_razon.rds")

#********************************************************************
# 3.4. ESTIMADOR POSESTRATIFICADO DEL PROMEDIO POR DOMINIOS ####
#********************************************************************

table(est$NATURALEZA)
table(est$FINS_ESTRATOVIVIENDAENERGIA)
muestraXest$natu_estrato <- paste(muestraXest$NATURALEZA, muestraXest$FINS_ESTRATOVIVIENDAENERGIA
                                     , sep="_")

est$natu_estrato <- paste(est$NATURALEZA, est$FINS_ESTRATOVIVIENDAENERGIA
                                     , sep="_")

table(muestraXest$natu_estrato)

diseno_muestral <- svydesign(ids = ~CODIGOMUNICIPIO + CODIGO_ICFES + ID_estud,
                             strata = ~estrato_mpio + EstratoColegio,
                             fpc = ~ NI + NII + N_i, data = muestraXest,
                             nest = T)

naturaleza_estrato_est <- as.data.frame(svyby(~CIENCIAS_NATURALES_PUNT, ~natu_estrato, 
                                              diseno_muestral, FUN=svymean))[,c(2,3)]
naturaleza_estrato_est$cve <- 100 * cv(svyby(~CIENCIAS_NATURALES_PUNT, ~natu_estrato, diseno_muestral, FUN=svymean))

saveRDS(naturaleza_estrato_est, "./rds/naturaleza_estrato_est_dom.rds")

real_natu_est <- aggregate(CIENCIAS_NATURALES_PUNT~natu_estrato, data=est, FUN=mean)

saveRDS(real_natu_est, "./rds/real_natu_est.rds")


#********************************************************************
# 3.5. ESTIMADOR GREG PARA PROMEDIO POR DOMINIOS ####
#********************************************************************
#**********************
# Dominio NATURALEZA
#**********************
cor(est[,c(18:22)])

# Se calibra usando MATEMATICAS_PUNT
muestraXest$fexp <- weights(diseno_muestral)

mod1_mue <- lm(CIENCIAS_NATURALES_PUNT ~ MATEMATICAS_PUNT, data=muestraXest, 
               weights = fexp)

summary(mod1_mue)
e <- mod1_mue$residuals

# Crear g
modeloU <- lm(CIENCIAS_NATURALES_PUNT ~ MATEMATICAS_PUNT, data = est)
X_U <-  model.matrix(modeloU)
X_s <- model.matrix(mod1_mue)

diseno <- diseno_muestral
fexp_k <- weights(diseno) # 1/pi_k
# summary(pi_k)
W <- diag(fexp_k)
g <- rep(NA, nrow(muestraXest))

#Dominio
unique(est$NATURALEZA)

#***********************************
# Dominio NATURALEZA = "No oficial"
#***********************************
SumUd_X <- as.matrix(colSums(X_U[est$NATURALEZA == "No oficial", ]))
SumSd_X <- as.matrix(colSums(X_s[muestraXest$NATURALEZA == "No oficial", ]*
                               fexp_k[muestraXest$NATURALEZA == "No oficial"]))

# Este zeta es el delta de las ecuaciones del cuaderno
z_dk <- as.numeric(muestraXest$NATURALEZA == "No oficial")

t0<-Sys.time()
pb <- progress_bar$new(format = " processing data (:spin) [:bar] :percent time left: :eta", 
                       total = nrow(muestraXest))
g_NO <- NULL
for(i in 1:nrow(muestraXest)){
  g_NO[i] <-   z_dk[i] + t(SumUd_X - SumSd_X) %*%
    solve(t(X_s) %*% W %*% X_s) %*% as.matrix(X_s[i,])
  pb$tick()
}
t1<-Sys.time(); t1-t0

summary(g_NO)

Colegios_dk_NO <- as.numeric(est$NATURALEZA == "No oficial")
ybar_Greg_NOoficial <- sum(muestraXest$CIENCIAS_NATURALES_PUNT *  g_NO  * fexp_k)/sum(Colegios_dk_NO)
ybar_Greg_NOoficial
aggregate(CIENCIAS_NATURALES_PUNT ~ NATURALEZA, FUN = mean, data = est)

muestra <- muestraXest
muestra$U_NO <- g_NO * mod1_mue$residuals
diseno <- svydesign(ids = ~ CODIGOMUNICIPIO + CODIGO_ICFES + ID_estud,
                    strata = ~estrato_mpio + EstratoColegio,
                    fpc = ~ NI + NII + N_i, data = muestra,
                    nest = T)

svymean(~U_NO, diseno)
cv__NOoficial <- 100 * svymean(~U_NO, diseno) / ybar_Greg_NOoficial

greg_NOoficial_mean <- c(ybar_Greg_NOoficial, cv__NOoficial)
names(greg_NOoficial_mean) <- c("Prom_No_Oficial", "cv")
greg_NOoficial_mean

saveRDS(greg_NOoficial_mean, file = "./rds/greg_NOoficial_mean.rds")

#***********************************
# Dominio NATURALEZA = "Oficial"
#***********************************

SumUd_X <- as.matrix(colSums(X_U[est$NATURALEZA == "Oficial", ]))
SumSd_X <- as.matrix(colSums(X_s[muestraXest$NATURALEZA == "Oficial", ]*
                               fexp_k[muestraXest$NATURALEZA == "Oficial"]))

# Este zeta es el delta de las ecuaciones del cuaderno
z_dk <- as.numeric(muestraXest$NATURALEZA == "Oficial")

t0<-Sys.time()
pb <- progress_bar$new(format = " processing data (:spin) [:bar] :percent time left: :eta", 
                       total = nrow(muestraXest))
g_Of <- NULL
for(i in 1:nrow(muestraXest)){
  g_Of[i] <-   z_dk[i] + t(SumUd_X - SumSd_X) %*%
    solve(t(X_s) %*% W %*% X_s) %*% as.matrix(X_s[i,])
  pb$tick()
}
t1<-Sys.time(); t1-t0

summary(g_Of)

Colegios_dk_Of <- as.numeric(est$NATURALEZA == "Oficial")
ybar_Greg_Oficial <- sum(muestraXest$CIENCIAS_NATURALES_PUNT *  g_Of  * fexp_k)/sum(Colegios_dk_Of)
ybar_Greg_Oficial
aggregate(CIENCIAS_NATURALES_PUNT ~ NATURALEZA, FUN = mean, data = est)

#muestra <- muestraXest
muestra$U_Of <- g_Of * mod1_mue$residuals
diseno <- svydesign(ids = ~ CODIGOMUNICIPIO + CODIGO_ICFES + ID_estud,
                    strata = ~estrato_mpio + EstratoColegio,
                    fpc = ~ NI + NII + N_i, data = muestra,
                    nest = T)

svymean(~U_Of, diseno)
cv__Oficial <- 100 * svymean(~U_Of, diseno) / ybar_Greg_Oficial

greg_Oficial_mean <- c(ybar_Greg_Oficial, cv__Oficial)
names(greg_Oficial_mean) <- c("Prom_Oficial", "cv")
greg_Oficial_mean

saveRDS(greg_Oficial_mean, file = "./rds/greg_Oficial_mean.rds")

#********************************
# PRINCIPIO REPRESENTATIVIDAD
#********************************
Est_matematicas <- c(sum(muestraXest$MATEMATICAS_PUNT *  g_Of  * fexp_k)/sum(Colegios_dk_Of), 
                     sum(muestraXest$MATEMATICAS_PUNT *  g_NO  * fexp_k)/sum(Colegios_dk_NO))
Est_matematicas

real_mat <- as.data.frame(aggregate(MATEMATICAS_PUNT ~ NATURALEZA, FUN = mean, data = est))
names(real_mat)[2] <- "Promedio real"

repre_3.5 <- real_mat
repre_3.5$Prom_estimado <- Est_matematicas
repre_3.5

saveRDS(repre_3.5, file = "./rds/repre_3.5.rds")


#********************************************************************
# 3.6. ESTIMADOR HBF PARA PROMEDIO POR DOMINIOS ####
#********************************************************************

# Dominio MUNICIPIO (Cod mpio)
# y_est: Puntaje Ciencias Naturales
# x1: puntaje sociales
# x2: Estrato eneriga
# x3: Naturaleza

# Transformar variables, quitar nulos

# Variables para x2:
est$FINS_ESTRATOVIVIENDAENERGIA[is.na(est$FINS_ESTRATOVIVIENDAENERGIA)] <- 1

Infoaux <- est %>% group_by(CODIGOMUNICIPIO) %>% 
  summarise(Prom_SOCIALES_CIUDADANAS_PUNT = mean(SOCIALES_CIUDADANAS_PUNT),
            Prop_Estrato1 = mean(`1`),
            Prop_Estrato2 = mean(`2`),
            Prop_Estrato3 = mean(`3`),
            Prop_Estrato4 = mean(`4`),
            Prop_Estrato5 = mean(`5`),
            Prop_Estrato6 = mean(`6`),
            Prop_Naturaleza_No_Oficial = mean(`No oficial`),
            Prop_Naturaleza_Oficial = mean(`Oficial`),
            N_d = n() )

# Convertir en factores las categoricas de la muestra

muestraXest$CALENDARIO <- as.factor(muestraXest$CALENDARIO)
muestraXest$NATURALEZA <- as.factor(muestraXest$NATURALEZA)
muestraXest$JORNADA <- as.factor(muestraXest$JORNADA)
muestraXest$PERS_GENERO <- as.factor(muestraXest$PERS_GENERO)
muestraXest$FINS_ESTRATOVIVIENDAENERGIA <- as.factor(muestraXest$FINS_ESTRATOVIVIENDAENERGIA)
muestraXest$FINS_PISOSHOGAR <- as.factor(muestraXest$FINS_PISOSHOGAR)
muestraXest$FINS_TIENEINTERNET <- as.factor(muestraXest$FINS_TIENEINTERNET)
muestraXest$FINS_TIENECOMPUTADOR <- as.factor(muestraXest$FINS_TIENECOMPUTADOR)
muestraXest$FINS_TIENEAUTOMOVILPARTICULAR <- as.factor(muestraXest$FINS_TIENEAUTOMOVILPARTICULAR)
muestraXest$INGLES_DESEM <- as.factor(muestraXest$INGLES_DESEM)

Tamanos <- Infoaux[,c("CODIGOMUNICIPIO", "N_d")]
names(Infoaux)
Medias <- Infoaux[,c("CODIGOMUNICIPIO", "Prom_SOCIALES_CIUDADANAS_PUNT",
                     "Prop_Estrato2", "Prop_Estrato3", "Prop_Estrato4", "Prop_Estrato5", "Prop_Estrato6", 
                     "Prop_Naturaleza_Oficial")]

Tamanos$CODIGOMUNICIPIO <- as.character(Tamanos$CODIGOMUNICIPIO)
Medias$CODIGOMUNICIPIO <- as.character(Medias$CODIGOMUNICIPIO)
muestraXest$CODIGOMUNICIPIO <- as.character(muestraXest$CODIGOMUNICIPIO)

BHF <- pbmseBHF(CIENCIAS_NATURALES_PUNT ~ SOCIALES_CIUDADANAS_PUNT + FINS_ESTRATOVIVIENDAENERGIA + NATURALEZA, 
                dom = CODIGOMUNICIPIO, 
                meanxpop = Medias,
                popnsize = Tamanos,
                B = 200, data = muestraXest)

# Estimación para dominios observados
BHF$est$eblup

# Estimación del error cuadrático medio
BHF$mse

# cv
sqrt(BHF$mse$mse) / BHF$est$eblup$eblup * 100


# Estimaciones para dominios externos (que no salieron en la muestra)
Beta_est <- BHF$est$fit$fixed
names(Beta_est) <- gsub("XsXs", "", names(Beta_est) )
names(Beta_est)[1] <-" Intercepto" 
Beta_est <- as.matrix(Beta_est)

# Totales por dominio
x_1 <- "Prom_SOCIALES_CIUDADANAS_PUNT"
x_2 <- c("Prop_Estrato2", "Prop_Estrato3", "Prop_Estrato4", "Prop_Estrato5", "Prop_Estrato6")
x_3 <- c("Prop_Naturaleza_Oficial")

Xbar_d <- Infoaux[ c(x_1, x_2, x_3)]
Unos <- as.data.frame(as.matrix(rep(1, nrow(Infoaux))))
Xbar_d <- cbind(Unos, Xbar_d)
Xbar_d <- as.matrix(Xbar_d)
rownames(Xbar_d) <- Infoaux$CODIGOMUNICIPIO
head(Xbar_d)

Prom_dominios <- Xbar_d %*% Beta_est
rownames(Prom_dominios) <- Infoaux$CODIGOMUNICIPIO
Prom_dominios <- as.data.frame(Prom_dominios)
Prom_dominios$domain <- row.names(Prom_dominios)
colnames(Prom_dominios)[1] <- "Ybar_efectosfijos" 
head(Prom_dominios)


# Conservar los dominios no observados 
Prom_dominios_observados <- BHF$est$eblup
Prom_dominios <- merge(Prom_dominios, Prom_dominios_observados, by = "domain", all.x = T)
names(Prom_dominios)[1] <- "MUNICIPIO"
head(Prom_dominios)
# Estimación MSE para dominios no observados 

library(nlme)
modelo_mixto <- lme(CIENCIAS_NATURALES_PUNT ~ SOCIALES_CIUDADANAS_PUNT + FINS_ESTRATOVIVIENDAENERGIA + NATURALEZA, 
                    random = ~1 | as.factor(CODIGOMUNICIPIO), data = muestraXest)


#\hat{V}(\hat{\bodysymbol{\beta}}):
Varest_betaest <- vcov(modelo_mixto)

# \hat{sigma}^2_u
sigma2est_u <- BHF[[1]]$fit$refvar #29.66217^2 #EN modelo_mixto observese que es la misma estimacion

# Identificar los dominios no observados
dominios_noobservados <- unique(est$CODIGOMUNICIPIO)[!(unique(est$CODIGOMUNICIPIO) %in% unique(muestraXest$CODIGOMUNICIPIO))]
Xbar_d_noobs <- Xbar_d[row.names(Xbar_d) %in% dominios_noobservados,]
MSE_DominiosNoobservados <- diag((Xbar_d_noobs %*% Varest_betaest %*% t(Xbar_d_noobs)) + sigma2est_u)
MSE_DominiosNoobservados <- as.table(MSE_DominiosNoobservados)
df_MSE_DominiosNoobservados <- as.data.frame(MSE_DominiosNoobservados)
names(df_MSE_DominiosNoobservados) <- c("MUNICIPIO", "MSE")
df_MSE_DominiosNoobservados$ClaseDominio <- "No observado"

head(df_MSE_DominiosNoobservados)

df_MSE_Dominiosobservados <- BHF$mse
names(df_MSE_Dominiosobservados) <- c("MUNICIPIO", "MSE")
df_MSE_Dominiosobservados$ClaseDominio <- "Observado"

head(df_MSE_Dominiosobservados)

df_MSE_Dominios <- bind_rows(df_MSE_DominiosNoobservados, df_MSE_Dominiosobservados)
df_MSE_Dominios <- df_MSE_Dominios[order(df_MSE_Dominios$MUNICIPIO),]

# Tienden a dar más MSE los dominios no obsevados
boxplot(MSE ~ ClaseDominio, data = df_MSE_Dominios)

# Resultados finales

Resultados <- merge(Prom_dominios, df_MSE_Dominios, by = "MUNICIPIO")
Resultados$Yhat_BHF <- ifelse(Resultados$ClaseDominio == "No observado", Resultados$Ybar_efectosfijos,
                              Resultados$eblup)
Resultados$cve <- 100 * sqrt(Resultados$MSE) / Resultados$Yhat_BHF

head_resultados_HBF <- head(Resultados)

saveRDS(head_resultados_HBF, "./rds/head_resultados_HBF.rds")
saveRDS(Resultados, "./rds/Resultados_BHF.rds")

mean(Resultados$Yhat_BHF)
mean(Resultados$cve)



#********************************************************************
# 4. ESTIMADOR PARA TOTAL DEL PUNTAJE CIENCIAS NATURALES ####
#********************************************************************

real_tot <- sum(est$CIENCIAS_NATURALES_PUNT)
names(real_tot) <- "CIENCIAS_NATURALES_PUNT"
saveRDS(real_tot, "./rds/real_tot.rds")

# 4.1. ESTIMADOR DIRECTO PARA TOTAL GLOBAL ####
#********************************************************************

Est_glo_dir_tot <- as.data.frame(svytotal(~CIENCIAS_NATURALES_PUNT, diseno_muestral))
names(Est_glo_dir_tot) <- c("total", "se")
Est_glo_dir_tot$cve <- 100 * cv(svytotal(~CIENCIAS_NATURALES_PUNT, diseno_muestral))[1,1]
Est_glo_dir_tot

saveRDS(Est_glo_dir_tot, "./rds/Est_glo_dir_tot.rds")


#********************************************************************
# 4.2. ESTIMADOR SINTÉTICO PARA TOTAL GLOBAL  ####
#********************************************************************

# Estimación
y_barra <- as.data.frame(svymean(~CIENCIAS_NATURALES_PUNT, diseno_muestral))[,1]
N_d <- length(est$CIENCIAS_NATURALES_PUNT)
Ysynth_d <- y_barra * N_d

# Real
sum(est$CIENCIAS_NATURALES_PUNT)

# Varianza
var_y_barra <- as.data.frame(svytotal(~CIENCIAS_NATURALES_PUNT, diseno_muestral))[,2]^2
N_d2 <- length(est$CIENCIAS_NATURALES_PUNT)^2
var_Ysynth_d <- var_y_barra / N_d2

cve <- (sqrt(var_Ysynth_d) / y_barra )* 100
cve

est_sintetico_tot <- data.frame(Total=Ysynth_d, cve=cve)
saveRDS(est_sintetico_tot, "./rds/est_sintetico_tot.rds")

#********************************************************************
# 4.3. ESTIMADOR DE RAZÓN PARA TOTAL GLOBAL ####
#********************************************************************

# La variable auxiliar es PUNTAJE SOCIALES_CIUDADANAS_PUNT porque es la de mayor correlación
# con CIENCIAS NATURALES

cor(est[,c(18:22)])

estimaGlobal_ciencias <- as.data.frame(svytotal(~CIENCIAS_NATURALES_PUNT, diseno_muestral))
hatY <- estimaGlobal_ciencias[,1]
hatY

estimaGlobal_sociales <- as.data.frame(svytotal(~SOCIALES_CIUDADANAS_PUNT, diseno_muestral))
hatX <- estimaGlobal_sociales[,1]
hatX

X_U <- sum(est$SOCIALES_CIUDADANAS_PUNT)

# Estimador de razón
Y_ratio <- hatY * (X_U / hatX)

# Real
sum(est$CIENCIAS_NATURALES_PUNT)

# cve

Var_Y_ratio <- X_U^2 * SE(svyratio(numerator = ~CIENCIAS_NATURALES_PUNT, 
                                  denominator = ~SOCIALES_CIUDADANAS_PUNT, 
                                  diseno_muestral)) ^ 2

cve <- sqrt(Var_Y_ratio) / Y_ratio * 100

est_razon_tot <- data.frame(Total=Y_ratio, cve=cve)
rownames(est_razon_tot) <- "CIENCIAS_NATURALES_PUNT / SOCIALES_CIUDADANAS_PUNT"
est_razon_tot

saveRDS(est_razon_tot, "./rds/est_razon_tot.rds")

#********************************************************************
# 4.4. ESTIMADOR POSESTRATIFICADO DEL TOTAL GLOBAL ####
#********************************************************************

# El estimador global del promedio posestratificado es el mismo del estimador directo HT

est_estra_tot <- as.data.frame(svytotal(~CIENCIAS_NATURALES_PUNT, diseno_muestral))
est_estra_tot$cve <- 100 * cv(svytotal(~CIENCIAS_NATURALES_PUNT, diseno_muestral))
names(est_estra_tot) <- c("Total", "se", "cve")
saveRDS(est_estra_tot, "./rds/est_estra_tot.rds")

#********************************************************************
# 4.5. ESTIMADOR GREG DEL TOTAL GLOBAL ####
#********************************************************************

cor(est[,c(18:22)])

# Se calibra usando MATEMATICAS_PUNT

muestraXest$fexp <- weights(diseno_muestral)

mod1_mue <- lm(CIENCIAS_NATURALES_PUNT ~ MATEMATICAS_PUNT, data=muestraXest, 
               weights = fexp)

summary(mod1_mue)
plot(muestraXest$CIENCIAS_NATURALES_PUNT ~ muestraXest$MATEMATICAS_PUNT)
abline(mod1_mue)

Xu <- as.numeric(c(nrow(est), sum(est$MATEMATICAS_PUNT)))

diseno_calibrado <- calibrate(diseno_muestral, ~ MATEMATICAS_PUNT, calfun = "linear" , 
                              population = Xu)

muestraXest$pesocalib <- weights(diseno_calibrado)
g_k <- muestraXest$pesocalib / muestraXest$fexp
# Mediana y promedio cercanos a 1
summary(g_k)

est_greg_tot <- as.data.frame(svytotal(~CIENCIAS_NATURALES_PUNT, diseno_calibrado))
est_greg_tot$cve <- 100*cv(svytotal(~CIENCIAS_NATURALES_PUNT, diseno_calibrado))
names(est_greg_tot) <- c("Total", "se", "cv")
saveRDS(est_greg_tot, "./rds/est_greg_tot.rds")
sum(est$CIENCIAS_NATURALES_PUNT)

#***********************************
# Principio de representatividad  
#***********************************
real_4.5 <- est %>% summarise(Ciencias=sum(CIENCIAS_NATURALES_PUNT), Matemáticas=sum(MATEMATICAS_PUNT))
repre_4.5 <- as.data.frame(svytotal(~CIENCIAS_NATURALES_PUNT + MATEMATICAS_PUNT, diseno_calibrado))

repre_4.5$Total_real <- c(real_4.5$Ciencias, real_4.5$Matemáticas)
repre_4.5 <- repre_4.5 %>% select(Estimación=total, Total_real)
saveRDS(repre_4.5, "./rds/repre_4.5.rds")


#********************************************************************
# 4.6. ESTIMADOR HBF DEL TOTAL GLOBAL  ####
#********************************************************************

#**********************
# DOMINIO: MUNICIPIO 
#**********************

# Dominio Depto(Cod mpio)
# y_est: Puntaje Ciencias Naturales
# x1: puntaje sociales
# x2: Estrato eneriga
# x3: Naturaleza

Infoaux <- est %>% group_by(CODIGOMUNICIPIO) %>% 
  summarise(Prom_SOCIALES_CIUDADANAS_PUNT = mean(SOCIALES_CIUDADANAS_PUNT),
            Prop_Estrato1 = mean(`1`),
            Prop_Estrato2 = mean(`2`),
            Prop_Estrato3 = mean(`3`),
            Prop_Estrato4 = mean(`4`),
            Prop_Estrato5 = mean(`5`),
            Prop_Estrato6 = mean(`6`),
            Prop_Naturaleza_No_Oficial = mean(`No oficial`),
            Prop_Naturaleza_Oficial = mean(`Oficial`),
            N_d = n())

Tamanos <- Infoaux[,c("CODIGOMUNICIPIO", "N_d")]
names(Infoaux)
Medias <- Infoaux[,c("CODIGOMUNICIPIO", "Prom_SOCIALES_CIUDADANAS_PUNT",
                     "Prop_Estrato2", "Prop_Estrato3", "Prop_Estrato4", "Prop_Estrato5", "Prop_Estrato6", 
                     "Prop_Naturaleza_Oficial")]

Tamanos$CODIGOMUNICIPIO <- as.character(Tamanos$CODIGOMUNICIPIO)
Medias$CODIGOMUNICIPIO <- as.character(Medias$CODIGOMUNICIPIO)
muestraXest$CODIGOMUNICIPIO <- as.character(muestraXest$CODIGOMUNICIPIO)

BHF <- pbmseBHF(CIENCIAS_NATURALES_PUNT ~ SOCIALES_CIUDADANAS_PUNT + FINS_ESTRATOVIVIENDAENERGIA + NATURALEZA, 
                dom = CODIGOMUNICIPIO, 
                meanxpop = Medias,
                popnsize = Tamanos,
                B = 200, data = muestraXest)


# Estimación para dominios observados
BHF$est$eblup

# Estimación del error cuadrático medio
BHF$mse

# cv
sqrt(BHF$mse$mse) / BHF$est$eblup$eblup * 100


# Estimaciones para dominios externos (que no salieron en la muestra)
Beta_est <- BHF$est$fit$fixed
names(Beta_est) <- gsub("XsXs", "", names(Beta_est) )
names(Beta_est)[1] <-" Intercepto" 
Beta_est <- as.matrix(Beta_est)

# Totales por dominio
x_1 <- "Prom_SOCIALES_CIUDADANAS_PUNT"
x_2 <- c("Prop_Estrato2", "Prop_Estrato3", "Prop_Estrato4", "Prop_Estrato5", "Prop_Estrato6")
x_3 <- c("Prop_Naturaleza_Oficial")

Xbar_d <- Infoaux[ c(x_1, x_2, x_3)]
Unos <- as.data.frame(as.matrix(rep(1, nrow(Infoaux))))
Xbar_d <- cbind(Unos, Xbar_d)
Xbar_d <- as.matrix(Xbar_d)
rownames(Xbar_d) <- Infoaux$CODIGOMUNICIPIO
head(Xbar_d)

Prom_dominios <- Xbar_d %*% Beta_est
rownames(Prom_dominios) <- Infoaux$CODIGOMUNICIPIO
Prom_dominios <- as.data.frame(Prom_dominios)
Prom_dominios$domain <- row.names(Prom_dominios)
colnames(Prom_dominios)[1] <- "Ybar_efectosfijos" 
head(Prom_dominios)


# Conservar los dominios no observados 
Prom_dominios_observados <- BHF$est$eblup
Prom_dominios <- merge(Prom_dominios, Prom_dominios_observados, by = "domain", all.x = T)
names(Prom_dominios)[1] <- "MUNICIPIO"
head(Prom_dominios)

# Estimación MSE para dominios no observados 
library(nlme)
modelo_mixto <- lme(CIENCIAS_NATURALES_PUNT ~ SOCIALES_CIUDADANAS_PUNT + FINS_ESTRATOVIVIENDAENERGIA + NATURALEZA, 
                    random = ~1 | as.factor(CODIGOMUNICIPIO), data = muestraXest)


#\hat{V}(\hat{\bodysymbol{\beta}}):
Varest_betaest <- vcov(modelo_mixto)

# \hat{sigma}^2_u
sigma2est_u <- BHF[[1]]$fit$refvar #29.66217^2 #EN modelo_mixto observese que es la misma estimacion

# Identificar los dominios no observados
dominios_noobservados <- unique(est$CODIGOMUNICIPIO)[!(unique(est$CODIGOMUNICIPIO) %in% unique(muestraXest$CODIGOMUNICIPIO))]
Xbar_d_noobs <- Xbar_d[row.names(Xbar_d) %in% dominios_noobservados,]
MSE_DominiosNoobservados <- diag((Xbar_d_noobs %*% Varest_betaest %*% t(Xbar_d_noobs)) + sigma2est_u)
MSE_DominiosNoobservados <- as.table(MSE_DominiosNoobservados)
df_MSE_DominiosNoobservados <- as.data.frame(MSE_DominiosNoobservados)
names(df_MSE_DominiosNoobservados) <- c("MUNICIPIO", "MSE")
df_MSE_DominiosNoobservados$ClaseDominio <- "No observado"

head(df_MSE_DominiosNoobservados)

df_MSE_Dominiosobservados <- BHF$mse
names(df_MSE_Dominiosobservados) <- c("MUNICIPIO", "MSE")
df_MSE_Dominiosobservados$ClaseDominio <- "Observado"

head(df_MSE_Dominiosobservados)

df_MSE_Dominios <- bind_rows(df_MSE_DominiosNoobservados, df_MSE_Dominiosobservados)
df_MSE_Dominios <- df_MSE_Dominios[order(df_MSE_Dominios$MUNICIPIO),]

# Tienden a dar más MSE los dominios no obsevados
boxplot(MSE ~ ClaseDominio, data = df_MSE_Dominios)

# Resultados finales (de la media)
Resultados <- merge(Prom_dominios, df_MSE_Dominios, by = "MUNICIPIO")
Resultados$Yhat_BHF <- ifelse(Resultados$ClaseDominio == "No observado", Resultados$Ybar_efectosfijos,
                              Resultados$eblup)
Resultados$cve <- 100 * sqrt(Resultados$MSE) / Resultados$Yhat_BHF

head(Resultados)

# Totales
est_tot_HBF <- data.frame(CODIGOMUNICIPIO=Resultados$MUNICIPIO, media=Resultados$Yhat_BHF, cve=Resultados$cve)
Tam_Mun <- est %>% group_by(CODIGOMUNICIPIO) %>%
           summarise(Nd=n())

est_tot_HBF <- merge(est_tot_HBF, Tam_Mun, by="CODIGOMUNICIPIO") %>%
               mutate(Total=media*Nd)

est_tot_HBF <- data.frame(Est_total=sum(est_tot_HBF$Total), cve=mean(est_tot_HBF$cve), 
                          real_total=sum(est$CIENCIAS_NATURALES_PUNT))

rownames(est_tot_HBF) <- "CIENCIAS_NATURALES_PUNT"
saveRDS(est_tot_HBF, "./rds/est_tot_HBF.rds")


#*************************************************************************
# 5. ESTIMADOR DEL PUNTAJE TOTAL DE  CIENCIAS NATURALES POR DOMINIOS ####
#*************************************************************************

real_dominios_tot <- aggregate(CIENCIAS_NATURALES_PUNT ~ NATURALEZA, data=est, FUN=sum)
saveRDS(real_dominios_tot, "./rds/real_dominios_tot.rds")

#********************************************************************
# 5.1. ESTIMADOR DIRECTO DEL TOTAL POR DOMINIOS ####
#********************************************************************

# Dominio: Naturaleza

est_dir_toto <- as.data.frame(svyby(~CIENCIAS_NATURALES_PUNT, ~NATURALEZA, diseno_muestral, FUN = svytotal))[,c(2,3)]
est_dir_toto$cve <-100 * cv(svyby(~CIENCIAS_NATURALES_PUNT, ~NATURALEZA, diseno_muestral, FUN = svytotal))
est_dir_toto
saveRDS(est_dir_toto, "./rds/est_dir_toto.rds")
aggregate(CIENCIAS_NATURALES_PUNT ~ NATURALEZA, data=est, FUN=sum)


#********************************************************************
# 5.2. ESTIMADOR SINTÉTICO DEL TOTAL POR DOMINIOS ####
#********************************************************************

# Dominio: NATURALEZA

# Estimación
y_bar <- as.data.frame(svyby(~CIENCIAS_NATURALES_PUNT, ~NATURALEZA, diseno_muestral, FUN=svymean))[,2]
G <- length(unique(est$NATURALEZA))
Ybarpron <- t(y_bar * t(matrix(1, nrow = G, ncol = 1 )))
N_d <- as.data.frame(sort(table(est$NATURALEZA)))[,2]
Ysynth_d <- y_bar * N_d
Ysynth_d

# Real
aggregate(CIENCIAS_NATURALES_PUNT ~ NATURALEZA, data=est, FUN=sum)

# Varianza
var_y_barra <- as.data.frame(svyby(~CIENCIAS_NATURALES_PUNT, ~NATURALEZA, diseno_muestral, FUN=svymean))[,3]^2
VarYbarpron <- t(var_y_barra * t(matrix(1, nrow = G, ncol = 1 )))
N_d2 <- as.data.frame(sort(table(est$NATURALEZA)))[,2]^2
var_Ysynth_d <- var_y_barra / N_d2

cve <- (sqrt(var_Ysynth_d))*100000
cve

est_dom_tot_dintetico <- data.frame(total=Ysynth_d, cve=cve)
rownames(est_dom_tot_dintetico) <- c("No Oficial", "Oficial")
est_dom_tot_dintetico

saveRDS(est_dom_tot_dintetico, "./rds/est_dom_tot_dintetico.rds")

#********************************************************************
# 5.3. ESTIMADOR DE RAZÓN DEL TOTAL POR DOMINIOS ####
#********************************************************************

# La variable auxiliar es PUNTAJE MATEMÁTICAS porque es la de mayor correlación
# con CIENCIAS NATURALES
# dominio: Naturaleza

estimaGlobal_ciencias <- as.data.frame(svyby(~CIENCIAS_NATURALES_PUNT, ~NATURALEZA, diseno_muestral, FUN=svytotal))
hatY <- estimaGlobal_ciencias[,2]
names(hatY) <- estimaGlobal_ciencias[,1]

estimaGlobal_sociales <- as.data.frame(svyby(~SOCIALES_CIUDADANAS_PUNT,  ~NATURALEZA, diseno_muestral, FUN=svytotal))
hatX <- estimaGlobal_sociales[,2]
names(hatX) <- estimaGlobal_sociales[,1]

X_U <- aggregate(SOCIALES_CIUDADANAS_PUNT ~ NATURALEZA, FUN = sum, data = est)
X_U_ <- sort(X_U$SOCIALES_CIUDADANAS_PUNT, decreasing = F)
names(X_U_) <- estimaGlobal_sociales[,1]

# Estimador de razón
Y_ratio <- hatY * (X_U_ / hatX)
Y_ratio

# Real
aggregate(CIENCIAS_NATURALES_PUNT ~ NATURALEZA, FUN = sum, data = est)

# cve
Var_Y_ratio <- X_U_^2 * SE(svyby(~CIENCIAS_NATURALES_PUNT, by=~NATURALEZA,
                                 denominator = ~SOCIALES_CIUDADANAS_PUNT, 
                                 diseno_muestral, FUN=svyratio))^2

cve=sqrt(Var_Y_ratio) / Y_ratio * 100

est_razon_tot_dom <- data.frame(Total=Y_ratio, cve=cve)
est_razon_tot_dom
saveRDS(est_razon_tot_dom, "./rds/est_razon_tot_dom.rds")



#********************************************************************
# 5.4. ESTIMADOR POSESTRATIFICADO DEL TOTAL POR DOMINIOS ####
#********************************************************************

muestraXest$natu_estrato <- paste(muestraXest$NATURALEZA, muestraXest$FINS_ESTRATOVIVIENDAENERGIA
                                     , sep="_")

est$natu_estrato <- paste(est$NATURALEZA, est$FINS_ESTRATOVIVIENDAENERGIA, sep="_")

table(muestraXest$natu_estrato)

diseno_muestral <- svydesign(ids = ~CODIGOMUNICIPIO + CODIGO_ICFES + ID_estud,
                             strata = ~estrato_mpio + EstratoColegio,
                             fpc = ~ NI + NII + N_i, data = muestraXest,
                             nest = T)

naturaleza_estrato_est <- svyby(~CIENCIAS_NATURALES_PUNT, ~natu_estrato, diseno_muestral, FUN=svytotal)
naturaleza_estrato_cv <- 100 * cv(svyby(~CIENCIAS_NATURALES_PUNT, ~natu_estrato, diseno_muestral, FUN=svytotal))

real_natu_est <- aggregate(CIENCIAS_NATURALES_PUNT~natu_estrato, data=est, FUN=sum)

naturaleza_estrato_est_tot <- as.data.frame(svyby(~CIENCIAS_NATURALES_PUNT, ~natu_estrato, 
                                              diseno_muestral, FUN=svytotal))[,c(2,3)]

naturaleza_estrato_est_tot$cve <- 100 * cv(svyby(~CIENCIAS_NATURALES_PUNT, ~natu_estrato, diseno_muestral, FUN=svytotal))
naturaleza_estrato_est_tot
saveRDS(naturaleza_estrato_est_tot, "./rds/naturaleza_estrato_est_tot.rds")

real_natu_est_tot <- aggregate(CIENCIAS_NATURALES_PUNT~natu_estrato, data=est, FUN=sum)
saveRDS(real_natu_est_tot, "./rds/real_natu_est_tot.rds")


#********************************************************************
# 5.5. ESTIMADOR GREG DEL TOTAL POR DOMINIOS ####
#********************************************************************
#*********************
# Dominio NATURALEZA
#*********************
cor(est[,c(18:22)])

# Calibración usando MATEMATICAS_PUNT
muestraXest$fexp <- weights(diseno_muestral)

mod1_mue <- lm(CIENCIAS_NATURALES_PUNT ~ MATEMATICAS_PUNT, data=muestraXest, 
               weights = fexp)

summary(mod1_mue)
e <- mod1_mue$residuals

# Crear g
modeloU <- lm(CIENCIAS_NATURALES_PUNT ~ MATEMATICAS_PUNT, data = est)
X_U <-  model.matrix(modeloU)
X_s <- model.matrix(mod1_mue)

diseno <- diseno_muestral
fexp_k <- weights(diseno) # 1/pi_k
# summary(pi_k)
W <- diag(fexp_k)
g <- rep(NA, nrow(muestraXest))

#Dominio
unique(est$NATURALEZA)

#***********************************
# Dominio NATURALEZA = "No oficial"
#***********************************
SumUd_X <- as.matrix(colSums(X_U[est$NATURALEZA == "No oficial", ]))
SumSd_X <- as.matrix(colSums(X_s[muestraXest$NATURALEZA == "No oficial", ]*
                               fexp_k[muestraXest$NATURALEZA == "No oficial"]))

# Este zeta es el delta de las ecuaciones del cuaderno
z_dk <- as.numeric(muestraXest$NATURALEZA == "No oficial")

if(is.null(g_NO)){ ## En el numeral 3.5 ya se había calculado
  t0<-Sys.time()
  pb <- progress_bar$new(format = " processing data (:spin) [:bar] :percent time left: :eta", 
                         total = nrow(muestraXest))
  
  for(i in 1:nrow(muestraXest)){
      g_NO[i] <-   z_dk[i] + t(SumUd_X - SumSd_X) %*%
        solve(t(X_s) %*% W %*% X_s) %*% as.matrix(X_s[i,])
      pb$tick()
  }
  t1<-Sys.time(); t1-t0
}

summary(g_NO)

yGreg_NOoficial <- sum(muestraXest$CIENCIAS_NATURALES_PUNT *  g_NO  * fexp_k)
yGreg_NOoficial
aggregate(CIENCIAS_NATURALES_PUNT ~ NATURALEZA, FUN = sum, data = est)

muestra <- muestraXest
muestra$U <- g_NO * mod1_mue$residuals
diseno <- svydesign(ids = ~ CODIGOMUNICIPIO + CODIGO_ICFES + ID_estud,
                             strata = ~estrato_mpio + EstratoColegio,
                             fpc = ~ NI + NII + N_i, data = muestra,
                             nest = T)

svytotal(~U, diseno)
cv__NOoficial <- 100 * svytotal(~U, diseno) / yGreg_NOoficial

greg_NOoficial_total <- c(yGreg_NOoficial, cv__NOoficial)
names(greg_NOoficial_total) <- c("Total_No_Oficial", "cv")
greg_NOoficial_total

saveRDS(greg_NOoficial_total, file = "./rds/greg_NOoficial_total.rds")

#***********************************
# Dominio NATURALEZA = "Oficial"
#***********************************
SumUd_X <- as.matrix(colSums(X_U[est$NATURALEZA == "Oficial", ]))
SumSd_X <- as.matrix(colSums(X_s[muestraXest$NATURALEZA == "Oficial", ]*
                               fexp_k[muestraXest$NATURALEZA == "Oficial"]))

# Este zeta es el delta de las ecuciones del cuaderno
z_dk <- as.numeric(muestraXest$NATURALEZA == "Oficial")

if(is.null(g_Of)){ ## En el numeral 3.5 ya se había calculado
  t0<-Sys.time()
  pb <- progress_bar$new(format = " processing data (:spin) [:bar] :percent time left: :eta", 
                         total = nrow(muestraXest))
  
  for(i in 1:nrow(muestraXest)){
    g_Of[i] <-   z_dk[i] + t(SumUd_X - SumSd_X) %*%
      solve(t(X_s) %*% W %*% X_s) %*% as.matrix(X_s[i,])
    pb$tick()
  }
  t1<-Sys.time(); t1-t0
}

summary(g_Of)

yGreg_Oficial <- sum(muestraXest$CIENCIAS_NATURALES_PUNT *  g_Of  * fexp_k)
yGreg_Oficial
aggregate(CIENCIAS_NATURALES_PUNT ~ NATURALEZA, FUN = sum, data = est)

muestra <- muestraXest
muestra$U <- g_Of * mod1_mue$residuals
diseno <- svydesign(ids = ~ CODIGOMUNICIPIO + CODIGO_ICFES + ID_estud,
                    strata = ~estrato_mpio + EstratoColegio,
                    fpc = ~ NI + NII + N_i, data = muestra,
                    nest = T)

svytotal(~U, diseno)
cv__Oficial <- 100 * svytotal(~U, diseno) / yGreg_Oficial

greg_oficial_total <- c(yGreg_Oficial, cv__Oficial)
names(greg_oficial_total) <- c("Total_Oficial", "cv")
greg_oficial_total

saveRDS(greg_oficial_total, file = "./rds/greg_oficial_total.rds")

#********************************
# PRINCIPIO REPRESENTATIVIDAD
#********************************
Est_matematicas <- c(sum(muestraXest$MATEMATICAS_PUNT *  g_Of  * fexp_k), 
                     sum(muestraXest$MATEMATICAS_PUNT *  g_NO  * fexp_k))
Est_matematicas

real_mat <- as.data.frame(aggregate(MATEMATICAS_PUNT ~ NATURALEZA, FUN = sum, data = est))
names(real_mat)[2] <- "total real"

repre_5.5 <- real_mat
repre_5.5$total_estimado <- Est_matematicas
repre_5.5

saveRDS(repre_5.5, file = "./rds/repre_5.5.rds")

#

#********************************************************************
# 5.6. ESTIMADOR HBF DEL TOTAL POR DOMINIOS ####
#********************************************************************

# Dominio Depto(Cod mpio)
# y_est: Puntaje Ciencias Naturales
# x1: puntaje sociales
# x2: Estrato eneriga
# x3: Naturaleza

# Transformar variables, quitar nulos

Infoaux <- est %>% group_by(CODIGOMUNICIPIO) %>% 
  summarise(Prom_SOCIALES_CIUDADANAS_PUNT = sum(SOCIALES_CIUDADANAS_PUNT),
            Prop_Estrato1 = sum(`1`),
            Prop_Estrato2 = sum(`2`),
            Prop_Estrato3 = sum(`3`),
            Prop_Estrato4 = sum(`4`),
            Prop_Estrato5 = sum(`5`),
            Prop_Estrato6 = sum(`6`),
            Prop_Naturaleza_No_Oficial = sum(`No oficial`),
            Prop_Naturaleza_Oficial = sum(`Oficial`),
            N_d = n() )

Tamanos <- Infoaux[,c("CODIGOMUNICIPIO", "N_d")]
names(Infoaux)
Medias <- Infoaux[,c("CODIGOMUNICIPIO", "Prom_SOCIALES_CIUDADANAS_PUNT",
                     "Prop_Estrato2", "Prop_Estrato3", "Prop_Estrato4", "Prop_Estrato5", "Prop_Estrato6", 
                     "Prop_Naturaleza_Oficial")]

Tamanos$CODIGOMUNICIPIO <- as.character(Tamanos$CODIGOMUNICIPIO)
Medias$CODIGOMUNICIPIO <- as.character(Medias$CODIGOMUNICIPIO)
muestraXest$CODIGOMUNICIPIO <- as.character(muestraXest$CODIGOMUNICIPIO)

BHF <- pbmseBHF(CIENCIAS_NATURALES_PUNT ~ SOCIALES_CIUDADANAS_PUNT + FINS_ESTRATOVIVIENDAENERGIA + NATURALEZA, 
                dom = CODIGOMUNICIPIO, 
                meanxpop = Medias,
                popnsize = Tamanos,
                B = 200, data = muestraXest)

# Estimaci�n para dominios observados
BHF$est$eblup

# Estimaci�n del error cuadr�tico medio
BHF$mse

# cv
sqrt(BHF$mse$mse) / BHF$est$eblup$eblup * 100


# Estimaciones para dominios externos (que no salieron en la muestra)
Beta_est <- BHF$est$fit$fixed
names(Beta_est) <- gsub("XsXs", "", names(Beta_est) )
names(Beta_est)[1] <-" Intercepto" 
Beta_est <- as.matrix(Beta_est)

# Totales por dominio
x_1 <- "Prom_SOCIALES_CIUDADANAS_PUNT"
x_2 <- c("Prop_Estrato2", "Prop_Estrato3", "Prop_Estrato4", "Prop_Estrato5", "Prop_Estrato6")
x_3 <- c("Prop_Naturaleza_Oficial")

Xbar_d <- Infoaux[ c(x_1, x_2, x_3)]
Unos <- as.data.frame(as.matrix(rep(1, nrow(Infoaux))))
Xbar_d <- cbind(Unos, Xbar_d)
Xbar_d <- as.matrix(Xbar_d)
rownames(Xbar_d) <- Infoaux$CODIGOMUNICIPIO
head(Xbar_d)

Prom_dominios <- Xbar_d %*% Beta_est
rownames(Prom_dominios) <- Infoaux$CODIGOMUNICIPIO
Prom_dominios <- as.data.frame(Prom_dominios)
Prom_dominios$domain <- row.names(Prom_dominios)
colnames(Prom_dominios)[1] <- "Ybar_efectosfijos" 
head(Prom_dominios)


# Conservar los dominios no observados 
Prom_dominios_observados <- BHF$est$eblup
Prom_dominios <- merge(Prom_dominios, Prom_dominios_observados, by = "domain", all.x = T)
names(Prom_dominios)[1] <- "MUNICIPIO"
head(Prom_dominios)
# Estimaci�n MSE para dominios no observados 

library(nlme)
modelo_mixto <- lme(CIENCIAS_NATURALES_PUNT ~ SOCIALES_CIUDADANAS_PUNT + FINS_ESTRATOVIVIENDAENERGIA + NATURALEZA, 
                    random = ~1 | as.factor(CODIGOMUNICIPIO), data = muestraXest)


#\hat{V}(\hat{\bodysymbol{\beta}}):
Varest_betaest <- vcov(modelo_mixto)

# \hat{sigma}^2_u
sigma2est_u <- BHF[[1]]$fit$refvar #29.66217^2 #EN modelo_mixto observese que es la misma estimacion

# Identificar los dominios no observados
dominios_noobservados <- unique(est$CODIGOMUNICIPIO)[!(unique(est$CODIGOMUNICIPIO) %in% unique(muestraXest$CODIGOMUNICIPIO))]
Xbar_d_noobs <- Xbar_d[row.names(Xbar_d) %in% dominios_noobservados,]
MSE_DominiosNoobservados <- diag((Xbar_d_noobs %*% Varest_betaest %*% t(Xbar_d_noobs)) + sigma2est_u)
MSE_DominiosNoobservados <- as.table(MSE_DominiosNoobservados)
df_MSE_DominiosNoobservados <- as.data.frame(MSE_DominiosNoobservados)
names(df_MSE_DominiosNoobservados) <- c("MUNICIPIO", "MSE")
df_MSE_DominiosNoobservados$ClaseDominio <- "No observado"

head(df_MSE_DominiosNoobservados)

df_MSE_Dominiosobservados <- BHF$mse
names(df_MSE_Dominiosobservados) <- c("MUNICIPIO", "MSE")
df_MSE_Dominiosobservados$ClaseDominio <- "Observado"

head(df_MSE_Dominiosobservados)

df_MSE_Dominios <- bind_rows(df_MSE_DominiosNoobservados, df_MSE_Dominiosobservados)
df_MSE_Dominios <- df_MSE_Dominios[order(df_MSE_Dominios$MUNICIPIO),]

# Tienden a dar m�s MSE los dominios no obsevados
boxplot(MSE ~ ClaseDominio, data = df_MSE_Dominios)

# Resultados finales

Resultados <- merge(Prom_dominios, df_MSE_Dominios, by = "MUNICIPIO")
Resultados$Yhat_BHF <- ifelse(Resultados$ClaseDominio == "No observado", Resultados$Ybar_efectosfijos,
                              Resultados$eblup)
Resultados$cve <- 100 * sqrt(Resultados$MSE) / Resultados$Yhat_BHF

head(Resultados)

Resultados <- merge(Prom_dominios, df_MSE_Dominios, by = "MUNICIPIO")
Resultados$Yhat_BHF <- ifelse(Resultados$ClaseDominio == "No observado", Resultados$Ybar_efectosfijos,
                              Resultados$eblup)
Resultados$cve <- 100 * sqrt(Resultados$MSE) / Resultados$Yhat_BHF

head_resultados_HBF_tot <- head(Resultados)

saveRDS(head_resultados_HBF_tot, "./rds/head_resultados_HBF_tot.rds")
saveRDS(Resultados, "./rds/resultados_HBF_tot.rds")

est_HBF_tot <- data.frame(Total=sum(Resultados$Yhat_BHF), cve=mean(Resultados$cve))
rownames(est_HBF_tot) <- "CIENCIAS_NATURALES_PUNT"

saveRDS(est_HBF_tot, "./rds/est_HBF_tot.rds")


#

