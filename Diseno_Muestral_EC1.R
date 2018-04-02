#******************** Diseño de Muestreo *****************************
#************** Por Conglomerados (ESTMAS) ***************************
# UPM: Municipio - ESTMAS (tx: Numero de estudiantes)
# USM: Colegio - ESTMAS (tx: Numero de estudiantes)
# UTM: Estudiante - MAS

rm(list = ls())
library(survey)
library(dplyr)
library(stratification)
options(survey.lonely.psu="adjust")


Marcoest <- readRDS("./data/estudiantes.rds")
Marcoest <- Marcoest[c("CODIGOMUNICIPIO", "NOMBREMUNICIPIO" ,"CODIGO_ICFES","ID_estud",
                       "AGSB_NOMBREINSTITUCION", "DEPARTAMENTO", "CALENDARIO",
                       "NATURALEZA", "JORNADA", "PERS_GENERO", "FINS_ESTRATOVIVIENDAENERGIA", "FINS_PERSONASHOGARACTUAL", 
                       "FINS_CUARTOSHOGARACTUAL", "FINS_PISOSHOGAR", "FINS_TIENEINTERNET", 
                       "FINS_TIENECOMPUTADOR", "FINS_TIENEAUTOMOVILPARTICULAR", "LECTURA_CRITICA_PUNT", 
                       "MATEMATICAS_PUNT", "SOCIALES_CIUDADANAS_PUNT", "CIENCIAS_NATURALES_PUNT", 
                       "INGLES_PUNT", "INGLES_DESEM", "EVALUADOS")]
Marcoest$CALENDARIO <- as.character(Marcoest$CALENDARIO)
Marcoest$NATURALEZA <- as.character(Marcoest$NATURALEZA)
Marcoest$JORNADA <- as.character(Marcoest$JORNADA)

#************************************************************************************************************
# UPM: Municipio - ESTMAS (tx: Numero de estudiantes) ####
#************************************************************************************************************
mpios <- Marcoest %>% group_by(CODIGOMUNICIPIO) %>% summarise(tx = n())
sum(mpios$tx)
set.seed(60316)
numestratos <- 7
LH <- strata.LH(x = mpios$tx, CV = 0.03, Ls = numestratos, takeall = T)

cortes <- c(min(mpios$tx), LH$bh, max(mpios$tx))
mpios$estrato_mpio <- cut(mpios$tx, breaks = cortes, include.lowest = T, right = F,
                          label = paste0("Estrato", 1:numestratos))

mpios <- arrange(mpios, tx)
set.seed(60316)
indica_estratifMpio <- sampling::strata(mpios, "estrato_mpio", size = LH$nh,
                                        method = "srswor", description = T)
mue_mpios <- sampling::getdata(mpios, indica_estratifMpio)
mue_mpios <- mue_mpios[c("estrato_mpio", "CODIGOMUNICIPIO")]

#*******************************************************
# Selección de colegios ####
#*******************************************************
# Variale auxiliar de estratificación, el total
Tamanos_mpio <- data.frame(estrato_mpio = paste0("Estrato",1:numestratos), NI = LH$Nh, nI = LH$nh)
### Marco Muestral POR estudiante, COLEGIOS y MUNICIPIOS  
marco_mpio <- merge(Marcoest, mue_mpios, all.y = T, by = "CODIGOMUNICIPIO") 
marco_mpio <- merge(marco_mpio, Tamanos_mpio, by = "estrato_mpio" )

## Selección
colegios <- marco_mpio %>% 
            group_by(estrato_mpio, CODIGOMUNICIPIO, CODIGO_ICFES) %>% 
            summarise(tx = n())
sum(colegios$tx)
head(colegios)

mue_mpio_col <- colegios %>% 
                group_by(estrato_mpio, CODIGOMUNICIPIO) %>% 
                summarise(numColegios = n()) %>% 
                arrange(-numColegios)

#******************************************************
## GRUPOS DE COLEGIOS Y ESTUDIANTES #### 
#******************************************************
# SE ORGANIZAN 4 GRUPOS, ASI:
# Menos o igual de 3 colegios selecciona todos
# De 4 a 12 colegios hacer la mitad con MAS
# De 13 a 29 colegios hacer la tercera parte con MAS
# DE 30 Colegios en adelante hacer la octava parte con MAS
# nivel de precisión del 3%

# Grupos de Muncipios
mpios_grupo1 <- mue_mpio_col$CODIGOMUNICIPIO[mue_mpio_col$numColegios <= 3]
mpios_grupo2 <- mue_mpio_col$CODIGOMUNICIPIO[mue_mpio_col$numColegios >= 4 &
                                               mue_mpio_col$numColegios <= 12 ]
mpios_grupo3 <- mue_mpio_col$CODIGOMUNICIPIO[mue_mpio_col$numColegios >= 13 &
                                               mue_mpio_col$numColegios <= 29 ]
mpios_grupo4 <- mue_mpio_col$CODIGOMUNICIPIO[mue_mpio_col$numColegios >= 30]

# Selección de municipios del grupo 1
MueColegios_mpiosgrupo1 <- subset(marco_mpio, 
                                  marco_mpio$CODIGOMUNICIPIO %in% mpios_grupo1)
# Grupo 1 MAS (9 Colegios)
mue_ColA <- mue_mpio_col[mue_mpio_col$CODIGOMUNICIPIO %in% mpios_grupo1, ]
mue_ColA$NII <- mue_ColA$numColegios
mue_ColA$nII <- mue_ColA$numColegios
mue_ColA$numColegios <- NULL
mue_ColA$EstratoColegio <- "GRUPO1"

## Muestra por Estudiante (411)
mue_colgrupo1 <- merge(marco_mpio, mue_ColA, 
                       by = c("estrato_mpio","CODIGOMUNICIPIO"), all.y = T)

# Selecci?n de municipios del grupo 2
MarcoColegios_mpiosgrupo2 <- subset(marco_mpio, 
                                    marco_mpio$CODIGOMUNICIPIO %in% mpios_grupo2)
cons_colegA_grupo2 <- MarcoColegios_mpiosgrupo2 %>% 
  group_by(CODIGOMUNICIPIO,CODIGO_ICFES) %>% 
  summarise(temp = n())
cons_colB_grupo2 <- cons_colegA_grupo2 %>% group_by(CODIGOMUNICIPIO) %>% 
  summarise(Num_col = n())

# MAS mitad de los colegios (Grupo2)
cons_colB_grupo2$nNum_col <- ceiling(cons_colB_grupo2$Num_col * 0.5)

cons_colegA_grupo2 <- cons_colegA_grupo2[order(cons_colegA_grupo2$CODIGOMUNICIPIO),]
cons_colB_grupo2 <- cons_colB_grupo2[order(cons_colB_grupo2$CODIGOMUNICIPIO),]

set.seed(60316)
indicamue_grup2 <- sampling::strata(data = cons_colegA_grupo2, stratanames = "CODIGOMUNICIPIO",
                                    size = cons_colB_grupo2$nNum_col, method="srswor", 
                                    description = T)

mue_Col2 <- cons_colegA_grupo2[indicamue_grup2$ID_unit,] 

mue_Col2$temp <- NULL; 
mue_Col2 <- merge(mue_Col2, cons_colB_grupo2, by = "CODIGOMUNICIPIO")

names(mue_Col2)[3:4] <- c("NII", "nII")
mue_Col2$EstratoColegio <- "GRUPO2"

###Muestra grupo 2 (151 Colegios, 1680 Estudiantes)
mue_colgrupo2 <- merge(marco_mpio, mue_Col2, 
                       by = c("CODIGOMUNICIPIO", "CODIGO_ICFES"))
View(mue_colgrupo2)

# Selecci?n de municipios del grupo 3
### Grupo 3 ,arco 14062 estudiantes 
MarcoColegios_mpiosgrupo3 <- subset(marco_mpio, 
                                    marco_mpio$CODIGOMUNICIPIO %in% mpios_grupo3)

### grupo 3 (87 Colegios)
cons_colegA_grupo3 <- MarcoColegios_mpiosgrupo3 %>% 
  group_by(CODIGOMUNICIPIO,CODIGO_ICFES) %>% 
  summarise(temp = n())
cons_colB_grupo3 <- cons_colegA_grupo3 %>% group_by(CODIGOMUNICIPIO) %>% 
  summarise(Num_col = n())

### MAS (Tercera Parte de los colegios, 31 Colegios)
cons_colB_grupo3$nNum_col <- ceiling(cons_colB_grupo3$Num_col * (1/3))

cons_colegA_grupo3 <- cons_colegA_grupo3[order(cons_colegA_grupo3$CODIGOMUNICIPIO),]

###Muestra Grupo 3 (31 Colegios, 4 Municipios)
cons_colB_grupo3 <- cons_colB_grupo3[order(cons_colB_grupo3$CODIGOMUNICIPIO),]

set.seed(60316)

### Grupo 3 (Muestra 31 Clegios, 4 Municipios y probabilidad de Inclusi?n)
indicamue_grup3 <- sampling::strata(data = cons_colegA_grupo3, stratanames = "CODIGOMUNICIPIO",
                                    size = cons_colB_grupo3$nNum_col, method="srswor", 
                                    description = T)

### Grupo 3 (Muestra 31 Colegios, 4 Municipios y 1661 Estudiantes)
mue_Col3 <- cons_colegA_grupo3[indicamue_grup3$ID_unit,] 

mue_Col3$temp <- NULL; 

### Grupo 3 MAS TERCERA PARTE (31 colegios)
mue_Col3 <- merge(mue_Col3, cons_colB_grupo3, by = "CODIGOMUNICIPIO")

names(mue_Col3)[3:4] <- c("NII", "nII")
mue_Col3$EstratoColegio <- "GRUPO3"

### MARCO MUESTRA GRUPO 3 (1661 Estudiantes 31 colegios)
mue_colgrupo3 <- merge(marco_mpio, mue_Col3, 
                       by = c("CODIGOMUNICIPIO", "CODIGO_ICFES"))

#Selecci?n Colegios Grupo 4. Septima Parte 

MarcoColegios_mpiosgrupo4 <- subset(marco_mpio, 
                                    marco_mpio$CODIGOMUNICIPIO %in% mpios_grupo4)

### Grupo 4. 3633 Colegios, 17 municipios
cons_colegA_grupo4 <- MarcoColegios_mpiosgrupo4 %>% 
  group_by(CODIGOMUNICIPIO,CODIGO_ICFES) %>% 
  summarise(temp = n())
cons_colB_grupo4 <- cons_colegA_grupo4 %>% group_by(CODIGOMUNICIPIO) %>% 
  summarise(Num_col = n())

###Grupo 3 (octava parte de 3633 colegios a 461 colegios, en 17 municipios   )
cons_colB_grupo4$nNum_col <- ceiling(cons_colB_grupo4$Num_col * (1/8))


cons_colegA_grupo4 <- cons_colegA_grupo4[order(cons_colegA_grupo4$CODIGOMUNICIPIO),]
View(cons_colegA_grupo4)

cons_colB_grupo4 <- cons_colB_grupo4[order(cons_colB_grupo4$CODIGOMUNICIPIO),]

set.seed(60316)

### Gripo 4 (461 Colegios, probaInc)
indicamue_grup4 <- sampling::strata(data = cons_colegA_grupo4, stratanames = "CODIGOMUNICIPIO",
                                    size = cons_colB_grupo4$nNum_col, method="srswor", 
                                    description = T)

###Grupo 4 Muestra por Colegios (461)
mue_Col4 <- cons_colegA_grupo4[indicamue_grup4$ID_unit,] 

mue_Col4$temp <- NULL; 
mue_Col4 <- merge(mue_Col4, cons_colB_grupo4, by = "CODIGOMUNICIPIO")
sum(mue_Col4$nNum_col)
names(mue_Col4)[3:4] <- c("NII", "nII")
mue_Col4$EstratoColegio <- "GRUPO4"

mue_colgrupo4 <- merge(marco_mpio, mue_Col4, 
                       by = c("CODIGOMUNICIPIO", "CODIGO_ICFES"))
View(mue_colgrupo4)

mue_colgrupo4$EstratoColegio <- as.character(mue_colgrupo4$EstratoColegio)

############3333############ MUESTRA #################################################### 
muestra <- bind_rows(mue_colgrupo1, mue_colgrupo2,mue_colgrupo3, mue_colgrupo4)

View(muestra)
muestra <- muestra[c("estrato_mpio", "CODIGOMUNICIPIO", "NOMBREMUNICIPIO",  
                     "EstratoColegio", "CODIGO_ICFES", "ID_estud",
                     "AGSB_NOMBREINSTITUCION", "DEPARTAMENTO", "CALENDARIO", 
                     "NATURALEZA",  "JORNADA", "PERS_GENERO", "FINS_ESTRATOVIVIENDAENERGIA",
                     "FINS_PERSONASHOGARACTUAL", "FINS_CUARTOSHOGARACTUAL", "FINS_PISOSHOGAR", 
                     "FINS_TIENEINTERNET",  "FINS_TIENECOMPUTADOR", "FINS_TIENEAUTOMOVILPARTICULAR", 
                     "LECTURA_CRITICA_PUNT", "MATEMATICAS_PUNT", "SOCIALES_CIUDADANAS_PUNT",
                     "CIENCIAS_NATURALES_PUNT",  "INGLES_PUNT", "INGLES_DESEM", "EVALUADOS", "NI", "nI", 
                     "NII", "nII")]
# Cuantos municipios
length(unique(muestra$CODIGOMUNICIPIO))
# Cuantos colegios (628)
length(unique(muestra$CODIGO_ICFES))
length(unique(paste(muestra$CODIGOMUNICIPIO,muestra$CODIGO_ICFES, sep = "_")))

# Seleccionar estudiantes (MAS)
### 536 Colegios :::: 32763 Estudiantes 
consulta_estud <- muestra %>% group_by(CODIGOMUNICIPIO, CODIGO_ICFES) %>% 
  summarise(Num_est = n()) %>% arrange(-Num_est)

### Distribuci?n del Numero de estudiantes 
summary(consulta_estud$Num_est)

# 20% de los estudiantes (para 6768 Estudiantes)
consulta_estud$n_i <- ceiling(consulta_estud$Num_est * 0.2) 
sum(consulta_estud$n_i)

### Estudiantes por Colegios (6768) 
consulta_estud <- consulta_estud %>% arrange(CODIGO_ICFES)

### Num_est por N_i
names(consulta_estud)[3] <- c("N_i")
View(consulta_estud)
muestra <- muestra %>% arrange(CODIGO_ICFES)

### estudiantes
indica_mueestu <- sampling::strata(muestra, stratanames = "CODIGO_ICFES",
                                   size = consulta_estud$n_i,
                                   method = "srswor", description = T)

EC1muestraXest <- muestra[indica_mueestu$ID_unit,]  

EC1muestraXest <- merge(EC1muestraXest,  consulta_estud)
## Muestra final de 6768 resultados
dim(EC1muestraXest) 


library(survey)
?svydesign
diseno_muestral <- svydesign(ids = ~ CODIGOMUNICIPIO + CODIGO_ICFES + ID_estud,
                             strata = ~estrato_mpio + EstratoColegio,
                             fpc = ~ NI + NII + N_i, data = EC1muestraXest,
                             nest = T)
svymean(~CIENCIAS_NATURALES_PUNT, diseno_muestral)
100 * cv(svymean(~CIENCIAS_NATURALES_PUNT, diseno_muestral))
mean(Marcoest$CIENCIAS_NATURALES_PUNT)
svytotal(~EVALUADOS, diseno_muestral)
100 * cv(svytotal(~EVALUADOS, diseno_muestral))
sum(Marcoest$EVALUADOS)
sum(weights(diseno_muestral))
nrow(Marcoest)

saveRDS(EC1muestraXest, "./data/EC1muestraXest.rds")
