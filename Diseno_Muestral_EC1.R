############### Diseño de Muestreo ###################
########### En Varias Etapas  ####################
# UPM: Municipio - ESTMAS (tx: Numero de estudiantes)
# USM: Colegio - ESTMAS (tx: Numero de Colegios)
# UTM: Estudiante - MAS

rm(list = ls())
options(survey.lonely.psu="adjust")

library(dplyr)
library(stratification)
setwd("D:/Documents/MEA/SAE/Estudio_de_Caso_1")

###################### MARCO DE MUESTREO#############################
# Marco Muestral: 1098 Municipios, 10480 Colegios, 535254 Estudiantes 

Marcoest <- readRDS("5. Marco_estudiantes_Pruebas_saber.rds")
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

###################### UPM: Municipio - ESTMAS (tx: Numero de estudiantes) ##################################
# Marco Muestral: 1098 Municipios, 10480 Colegios, 535254 Estudiantes 

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

Tamanos_mpio <- data.frame(estrato_mpio = paste0("Estrato",1:numestratos), NI = LH$Nh, nI = LH$nh)

# Muestra UPM, 35 Municipios, 3793 Colegios, 234695 Estudiantes   
marco_mpio <- merge(Marcoest, mue_mpios, all.y = T, by = "CODIGOMUNICIPIO") 
marco_mpio <- merge(marco_mpio, Tamanos_mpio, by = "estrato_mpio" )

##################### USM de colegios ######################
# Variale auxiliar de estratificación, total (Colegios)
# UPM: 35 Municipios, 3793 Colegios, 234695 Estudiantes

colegios <- marco_mpio %>% group_by(estrato_mpio,CODIGOMUNICIPIO, CODIGO_ICFES) %>% 
  summarise(tx = n())

mue_mpio_col <- colegios %>% group_by(estrato_mpio,CODIGOMUNICIPIO) %>% 
  summarise(numColegios = n()) %>% arrange(-numColegios)

######### Estratificar (4 Grupos) Según Criterio (Colegios) ############## 
# UPM: 35 Municipios, 3793 Colegios, 234695 Estudiantes

# Menos  o igual de 3 colegios incluir Todos
# De 4 a 12 colegios, Incluir la mitad con MAS
# De 13 a 29 colegios, Incluir la tercera parte  con MAS
# DE 30 Colegios o mas, incluir la octava parte con MAS
# nivel de precisión, 3%

# Grupos de Muncipios
mpios_grupo1 <- mue_mpio_col$CODIGOMUNICIPIO[mue_mpio_col$numColegios <= 3]
mpios_grupo2 <- mue_mpio_col$CODIGOMUNICIPIO[mue_mpio_col$numColegios >= 4 &
                                               mue_mpio_col$numColegios <= 12 ]
mpios_grupo3 <- mue_mpio_col$CODIGOMUNICIPIO[mue_mpio_col$numColegios >= 13 &
                                               mue_mpio_col$numColegios <= 29 ]
mpios_grupo4 <- mue_mpio_col$CODIGOMUNICIPIO[mue_mpio_col$numColegios >= 30]

# Selección de municipios del grupo 1 (Menos  o igual de 3 colegios)
# Censo
#Grupo 1: 5 Municipios, 411 Estudiantes, 9 Colegios 
MueColegios_mpiosgrupo1 <- subset(marco_mpio, 
                                  marco_mpio$CODIGOMUNICIPIO %in% mpios_grupo1)
#Grupo 1
mue_ColA <- mue_mpio_col[mue_mpio_col$CODIGOMUNICIPIO %in% mpios_grupo1, ]
mue_ColA$NII <- mue_ColA$numColegios
mue_ColA$nII <- mue_ColA$numColegios
mue_ColA$numColegios <- NULL
mue_ColA$EstratoColegio <- "GRUPO1"
mue_colgrupo1 <- merge(marco_mpio, mue_ColA, 
                       by = c("estrato_mpio","CODIGOMUNICIPIO"), all.y = T)

# Selección de Colegios del grupo 2 (4 a 12 colegios)
#Grupo 2: 9 Municipios, 64 Colegios, 2753 Estudiantes
MarcoColegios_mpiosgrupo2 <- subset(marco_mpio, 
                                    marco_mpio$CODIGOMUNICIPIO %in% mpios_grupo2)
cons_colegA_grupo2 <- MarcoColegios_mpiosgrupo2 %>% 
  group_by(CODIGOMUNICIPIO,CODIGO_ICFES) %>% 
  summarise(temp = n())
cons_colB_grupo2 <- cons_colegA_grupo2 %>% group_by(CODIGOMUNICIPIO) %>% 
  summarise(Num_col = n())

# Grupo 2: MAS (mitad de los Colegios) 
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

# Muestra grupo 2 (35 Colegios, 9 Municipios, 1680 Estudiantes)
mue_colgrupo2 <- merge(marco_mpio, mue_Col2, 
                       by = c("CODIGOMUNICIPIO", "CODIGO_ICFES"))

# Selección de municipios del grupo 3 (13 a 29 colegios)
### Grupo 3: 87 Colegios
MarcoColegios_mpiosgrupo3 <- subset(marco_mpio, 
                                    marco_mpio$CODIGOMUNICIPIO %in% mpios_grupo3)
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

# Grupo 3: MAS (Tercera Parte) 
# (Muestra 31 Clegios, 4 Municipios) 
indicamue_grup3 <- sampling::strata(data = cons_colegA_grupo3, stratanames = "CODIGOMUNICIPIO",
                                    size = cons_colB_grupo3$nNum_col, method="srswor", 
                                    description = T)

mue_Col3 <- cons_colegA_grupo3[indicamue_grup3$ID_unit,] 
sum(mue_Col3$temp)
mue_Col3$temp <- NULL; 

mue_Col3 <- merge(mue_Col3, cons_colB_grupo3, by = "CODIGOMUNICIPIO")

names(mue_Col3)[3:4] <- c("NII", "nII")
mue_Col3$EstratoColegio <- "GRUPO3"

# Grupo 3 (1661 Estudiantes 31 colegios, 4 Municipios)
mue_colgrupo3 <- merge(marco_mpio, mue_Col3, 
                       by = c("CODIGOMUNICIPIO", "CODIGO_ICFES"))

# Grupo 4: Octava Parte 

MarcoColegios_mpiosgrupo4 <- subset(marco_mpio, 
                                    marco_mpio$CODIGOMUNICIPIO %in% mpios_grupo4)
View(MarcoColegios_mpiosgrupo4)
### Grupo 4. 3633 Colegios, 17 municipios, 227096 Estudiante
cons_colegA_grupo4 <- MarcoColegios_mpiosgrupo4 %>% 
  group_by(CODIGOMUNICIPIO,CODIGO_ICFES) %>% 
  summarise(temp = n())

cons_colB_grupo4 <- cons_colegA_grupo4 %>% group_by(CODIGOMUNICIPIO) %>% 
  summarise(Num_col = n())

# Grupo 4 (octava parte de 3633 colegios, en 17 municipios)

cons_colB_grupo4$nNum_col <- ceiling(cons_colB_grupo4$Num_col * (1/8))


cons_colegA_grupo4 <- cons_colegA_grupo4[order(cons_colegA_grupo4$CODIGOMUNICIPIO),]

cons_colB_grupo4 <- cons_colB_grupo4[order(cons_colB_grupo4$CODIGOMUNICIPIO),]
set.seed(60316)

### Gripo 4 (461 Colegios)
indicamue_grup4 <- sampling::strata(data = cons_colegA_grupo4, stratanames = "CODIGOMUNICIPIO",
                                    size = cons_colB_grupo4$nNum_col, method="srswor", 
                                    description = T)
###Grupo 4
mue_Col4 <- cons_colegA_grupo4[indicamue_grup4$ID_unit,] 
mue_Col4$temp <- NULL; 
mue_Col4 <- merge(mue_Col4, cons_colB_grupo4, by = "CODIGOMUNICIPIO")
sum(mue_Col4$nNum_col)
names(mue_Col4)[3:4] <- c("NII", "nII")
mue_Col4$EstratoColegio <- "GRUPO4"

mue_colgrupo4 <- merge(marco_mpio, mue_Col4, 
                       by = c("CODIGOMUNICIPIO", "CODIGO_ICFES"))

# Grupo 4: 461 Colegios, 17 Municipios, 29011 Estudiantes 
mue_colgrupo4$EstratoColegio <- as.character(mue_colgrupo4$EstratoColegio)

######################## MUESTRA #################################################### 
## Contiene: 536 Colegios, 32763 Estudiantes, 35 mUNICIPIOS 
muestra <- bind_rows(mue_colgrupo1, mue_colgrupo2,mue_colgrupo3, mue_colgrupo4)


muestra <- muestra[c("estrato_mpio", "CODIGOMUNICIPIO", "NOMBREMUNICIPIO",  
                     "EstratoColegio", "CODIGO_ICFES", "ID_estud",
                     "AGSB_NOMBREINSTITUCION", "DEPARTAMENTO", "CALENDARIO", 
                     "NATURALEZA",  "JORNADA", "PERS_GENERO", "FINS_ESTRATOVIVIENDAENERGIA",
                     "FINS_PERSONASHOGARACTUAL", "FINS_CUARTOSHOGARACTUAL", "FINS_PISOSHOGAR", 
                     "FINS_TIENEINTERNET",  "FINS_TIENECOMPUTADOR", "FINS_TIENEAUTOMOVILPARTICULAR", 
                     "LECTURA_CRITICA_PUNT", "MATEMATICAS_PUNT", "SOCIALES_CIUDADANAS_PUNT",
                     "CIENCIAS_NATURALES_PUNT",  "INGLES_PUNT", "INGLES_DESEM", "EVALUADOS", "NI", "nI", 
                     "NII", "nII")]

# Seleccionar estudiantes (MAS)
consulta_estud <- muestra %>% group_by(CODIGOMUNICIPIO, CODIGO_ICFES) %>% 
  summarise(Num_est = n()) %>% arrange(-Num_est)

### Distribución del Numero de estudiantes 
summary(consulta_estud$Num_est)

# 20% de los estudiantes (para 6768 Estudiantes)
consulta_estud$n_i <- ceiling(consulta_estud$Num_est * 0.2) 

### Estudiantes por Colegios (6768) 
consulta_estud <- consulta_estud %>% arrange(CODIGO_ICFES)

### Num_est por N_i
names(consulta_estud)[3] <- c("N_i")
muestra <- muestra %>% arrange(CODIGO_ICFES)

### estduaientes y Colegio al que pertenecen
indica_mueestu <- sampling::strata(muestra, stratanames = "CODIGO_ICFES",
                                   size = consulta_estud$n_i,
                                   method = "srswor", description = T)

EC1muestraXest <- muestra[indica_mueestu$ID_unit,]  

EC1muestraXest <- merge(EC1muestraXest,  consulta_estud)

### 6768 encuestas
dim(EC1muestraXest) 

saveRDS(EC1muestraXest, "EC1muestraXest.RDS")
