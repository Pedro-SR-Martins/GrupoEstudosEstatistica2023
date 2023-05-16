####################################### 
#######################################
# 
#   ANÁLISE FATORIAL CONFIRMATÓRIA
#           MULTIGRUPO
# 
#######################################
#######################################

# install.packages("haven")
# install.packages("lavaan")
# install.packages("semTools")

library(haven)
library(lavaan)
library(semTools)


dados <- read_sav("Burnout_inv.sav")
dados[-c(1:2)] <- zap_labels(dados[-c(1:2)])
dados[c(1:2)] <- as_factor(dados[c(1:2)])


############ AFC Geral #######################


modelo<- "
EE =~ burn1 + burn2 + burn3 + burn4 + burn5
RP =~ burn6 + burn7 + burn8 + burn9 + burn10
DESUM =~ burn11 + burn12 + burn13 + burn14 + burn15
DE =~ burn16 + burn17 + burn18 + burn19
"
cfa_geral <- cfa(modelo, data = dados, ordered = names(dados[3:21]), 
              estimator= "wlsmv")

summary(cfa_geral, fit.measures= TRUE, rsquare= TRUE, standardized= TRUE)


############ AFC Multigrupo #######################

# A AFC multigrupo é feita automaticamente quando indicamos o nome 
# da variável categórica no argumento group= da função cfa().
cfa_estrutural <- cfa(modelo, data = dados, ordered = TRUE, 
                 estimator= "wlsmv",
                 group = "sexo")

summary(cfa_estrutural, fit.measures= TRUE, rsquare= TRUE, standardized= TRUE)
# O modelo ajustado demonstra a invariância configural.
# Ou seja, os grupos possuem o mesmo número de fatores e itens, mas
# os valores das cargas fatoriais, correlações, resíduos e thresholds são
# diferentes em cada grupo

# Salvando índices de ajuste
fit_estrutural <- fitmeasures(object = cfa_estrutural, c("chisq.scaled", "df.scaled", 
                                                       "pvalue.scaled", 
                                                       "cfi.scaled", "tli.scaled", 
                                                       "rmsea.scaled") 
                              )




############ Invariância Métrica #######################

cfa_metrica <- cfa(modelo, data = dados, ordered = TRUE, 
                   estimator= "wlsmv", 
                   group = "sexo",
                   group.equal = c("loadings"))

summary(cfa_metrica, fit.measures= TRUE, rsquare= TRUE, standardized= TRUE)

fit_metrica <- fitmeasures(object = cfa_metrica, c("chisq.scaled", "df.scaled", 
                                                         "pvalue.scaled", 
                                                         "cfi.scaled", "tli.scaled", 
                                                         "rmsea.scaled") 
                           )

dif_est_metr <- compareFit(cfa_estrutural, cfa_metrica)
summary(dif_est_metr)





############ Invariância Escalar #######################

cfa_escalar<- cfa(modelo, data = dados, ordered = TRUE, 
                  estimator= "wlsmv",
                  group = "sexo", 
                  group.equal = c("loadings", "thresholds"))

summary(cfa_escalar, fit.measures= TRUE, rsquare= TRUE, standardized= TRUE)


fit_escalar<- fitmeasures(object = cfa_escalar, c("chisq.scaled", "df.scaled", 
                                                         "pvalue.scaled", 
                                                         "cfi.scaled", "tli.scaled", 
                                                         "rmsea.scaled") 
                          )

dif_ajuste <- compareFit(cfa_estrutural, cfa_metrica, cfa_escalar)
summary(dif_ajuste)
# O CFI não deve diminuir mais do que 0,01 (Cheung e Rensvold, 2002)
#   ou 0,002 (Meady et al., 2008)
# O RMSEA não deve aumentar mais do que 0,005 (Chen, 2007). 
# Não é consenso usar o RMSEA como métrica



fit <- data.frame(estrutural= fit_estrutural,
                  metrica= fit_metrica,
                  escalar= fit_escalar)
fit <- round(fit, 3)
fit




############ Invariância parcial ############

cfa_metrica_parcial <- cfa(modelo, data = dados, ordered = names(dados[3:21]), 
                           estimator= "wlsmv", 
                           group = "sexo",
                           meanstructure= TRUE,
                           group.equal = c("loadings"), 
                           group.partial = c("EE =~ burn5", "DE =~ burn18")
)
# Para liberar os thresholds:
# c("burn1|t1", "burn1|t2", "burn1|t3", "burn1|t4")

summary(cfa_metrica_parcial, fit.measures= TRUE, rsquare= TRUE, standardized= TRUE)

fit_metrica_parcial <- fitmeasures(object = cfa_metrica_parcial, c("chisq.scaled", "df.scaled", 
                                                                   "pvalue.scaled", 
                                                                   "cfi.scaled", "tli.scaled", 
                                                                   "rmsea.scaled") 
                                   )




############ Modelo de invariância completa #######################

cfa_media <- cfa(modelo, data = dados, ordered = names(dados[3:21]), 
               estimator= "wlsmv",
               meanstructure= TRUE,
               group = "sexo",
               group.equal = c("loadings", 
                               "thresholds", 
                               "residuals", 
                               "lv.variances",
                               "lv.covariances",
                               "means"
                               )
               )

