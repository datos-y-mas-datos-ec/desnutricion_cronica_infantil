setwd("C:/Users/Beto/Desktop/ENSANUT/BDD_ENSANUT_2018_STATA_2")

library(collapse)
library(readstata13)
library(dplyr)
library(tidyr)
library(doBy)

##Base de personas
bd_personas<-read.dta13("1_BDD_ENS2018_f1_personas.dta",  convert.factors = FALSE)

#variable mujer
bd_personas <- bd_personas %>% mutate(mujer = case_when(sexo==2 ~ 1,
                                                          sexo==1 ~ 0))
#Conteo personas
bd_personas <- mutate(bd_personas, conteo=1)

#Meses cumplidos

bd_personas <- bd_personas %>% rename("edad_meses" = "f1_s2_3_2")


#Base de niños menores de 2 años considerados para evaluar si tienen DCI
table(bd_personas$dcronica_2)

dcronica2 <- filter(bd_personas,!is.na(dcronica_2))

#Cambiar todos los NA por ceros
bd_personas <-replace(bd_personas, is.na(bd_personas), 0)

#Cálculo de ingreso per cápita del hogar

collapse_ing <- collap(bd_personas,conteo+f1_s3_15+f1_s3_17+f1_s3_18+f1_s3_22_2+f1_s3_23_2+f1_s3_24_2+f1_s3_25_2+f1_s3_26_2+f1_s3_28_1+f1_s3_30_1 ~ id_hogar, FUN=fsum)

collapse_ing <- mutate(collapse_ing, ingreso_total=f1_s3_15-f1_s3_17+f1_s3_18+f1_s3_22_2+f1_s3_23_2+f1_s3_24_2+f1_s3_25_2+f1_s3_26_2+f1_s3_28_1+f1_s3_30_1)

collapse_ing <- mutate(collapse_ing, ing_percapita=ingreso_total/conteo)

# Hogar pobre por ingresos
# Según el INEC, en diciembre de 2018 la línea de pobreza se ubicó en US$ 84,79
# https://www.ecuadorencifras.gob.ec/documentos/web-inec/POBREZA/2018/Diciembre-2018/Boletin%20tecnico%20de%20pobreza%20diciembre%202018.pdf

collapse_ing <- collapse_ing %>% mutate(pobre = case_when(ing_percapita<84.79 ~ 1,
                           ing_percapita>=84.79 ~ 0))

##Base salud niñez
bd_ninez<-read.dta13("6_BDD_ENS2018_f2_salud_ninez.dta",  convert.factors = FALSE)


# Control prenatal
table(bd_ninez$f2_s4b_406_)

bd_ninez <- bd_ninez %>% mutate(control_prenat = case_when(f2_s4b_406_==1 ~ 1,
                                                           f2_s4b_406_==2 ~ 0))

table(bd_ninez$control_prenat)

# Micronutrientes
table(bd_ninez$f2_s4b_408_)

#Inclusive los que no responden se considerarán como que no recibieron micronutrientes
bd_ninez <- bd_ninez %>% mutate(f2_s4b_408_ = ifelse(is.na(f2_s4b_408_), 99, f2_s4b_408_))

bd_ninez <- bd_ninez %>% mutate(micronut = case_when(f2_s4b_408_<4 ~ 1,
                                                     f2_s4b_408_>=4 ~ 0))
table(bd_ninez$micronut)


##Base hogar
bd_hogar<-read.dta13("2_BDD_ENS2018_f1_hogar.dta",  convert.factors = FALSE)

#Hogar con agua tuberia
table(bd_hogar$f1_s1_21)

bd_hogar <- bd_hogar %>% mutate(agua_tub = case_when(f1_s1_21<=3 ~ 1,
                                                     f1_s1_21>3 ~ 0))
#Hogar con excusado y alcantarillado o pozo céptico-ciego
table(bd_hogar$f1_s1_13)

bd_hogar <- bd_hogar %>% mutate(elim_exc = case_when(f1_s1_13<=3 ~ 1,
                                                     f1_s1_13>3 ~ 0))


#Base final para análisis

dcronica2 <- dcronica2 %>% rename("id_hijo" = "id_per")

table(dcronica2$dcronica_2)
table(dcronica2$mujer)

dcronica2_fin <- left_join(dcronica2, collapse_ing, by = "id_hogar")
dcronica2_fin <- left_join(dcronica2_fin, bd_hogar, by = "id_hogar")
dcronica2_fin <- left_join(dcronica2_fin, bd_ninez, by = "id_hijo")

dcronica2_fin <- select(dcronica2_fin, id_hijo, id_hogar.x, fexp, dcronica_2, mujer, edad_meses, ing_percapita, pobre, control_prenat, micronut,  agua_tub, elim_exc)

table(dcronica2_fin$mujer, useNA = "always")
table(dcronica2_fin$dcronica_2, useNA = "always")
table(dcronica2_fin$pobre, useNA = "always")
table(dcronica2_fin$control_prenat, useNA = "always")
table(dcronica2_fin$micronut, useNA = "always")
table(dcronica2_fin$agua_tub, useNA = "always")
table(dcronica2_fin$elim_exc, useNA = "always")


modelo.logit1 <- glm(dcronica_2 ~ mujer + edad_meses + ing_percapita + pobre + control_prenat + micronut + agua_tub + elim_exc, 
    family = binomial(link = "logit"), data = dcronica2_fin)

summary(modelo.logit1)

exp(coef(modelo.logit1))


modelo.logit2 <- glm(dcronica_2 ~ mujer + edad_meses + pobre + micronut + elim_exc, 
                    family = binomial(link = "logit"), data = dcronica2_fin)

summary(modelo.logit2)

exp(coef(modelo.logit2))
