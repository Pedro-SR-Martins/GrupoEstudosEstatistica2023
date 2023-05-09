# primeiro instale o pacote pacman, ele facilita a instalação de novos pacotes e para carrega-los
# install.packages("pacman")
pacman::p_load(lavaan, # analise fatorial
               semPlot, # visualizacao do modelo
               tidyverse) # manipulacao dos dados
base <- readRDS(url("https://github.com/Pedro-SR-Martins/GrupoEstudosEstatistica2023/raw/main/bancos/bancobif.rds", method="libcurl"))

# vendo os dados
glimpse(base)

# descritivas basicas
psych::describe(base)

# Construindo o modelo em tres etapas  
# 1. Modelo unidimensional  
# 2. Modelo de fatores correlacionados  
# 3. Modelo bifactor  

# modelo de uma dimensao ------


model1 <- '
geral =~ q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 
'
fit1 <- cfa(model = model1,
            data = base,
            std.lv = TRUE,
            ordered = names(base),
            estimator = "wlsmv")

summary(fit1, fit.measures = T, standardized = T)

# modelo com duas dimensoes ------


model2 <- '
extroversao =~ q1 + q2 + q3 + q4
abertura =~  q5 + q6 + q7 + q8 
'
fit2 <- cfa(model = model2,
            data = base,
            std.lv = T,
            ordered = names(base),
            estimator = "wlsmv")

summary(fit2, fit.measures = T, standardized = T)


# modelo com duas dimensoes retirando um item ------


model2.1 <- '
extroversao =~ q1 + q2 + q3 + q4
abertura =~  q5 + q7 + q8 
'
fit2.1 <- cfa(model = model2.1,
            data = base,
            std.lv = T,
            ordered = names(base),
            estimator = "wlsmv")

summary(fit2.1, fit.measures = T, standardized = T)


# modelo bifatorial ------


# note que aqui vai dar erro porque os fatores estao como correlacionados
model3 <- '
extroversao =~ q1 + q2 + q3 + q4
abertura =~  q5 +  q6 +q7 + q8 
geral =~ q1 + q2 + q3 + q4 + q5 + q6+ q7 + q8 
'
fit3 <- cfa(model = model3,
            data = base,
            ordered = names(base),
            estimator = "wlsmv",
            std.lv = T)


# desligando a correlacao entre os fatores com o argumento orthogonal = TRUE
# assim o modelo vai convergir
model3 <- '
extroversao =~ q1 + q2 + q3 + q4
abertura =~  q5 +  q6 +q7 + q8 
geral =~ q1 + q2 + q3 + q4 + q5 + q6+ q7 + q8  
'
fit3 <- cfa(model = model3,
            data = base,
            ordered = names(base),
            estimator = "wlsmv",
            std.lv = TRUE,
            orthogonal = TRUE)

summary(fit3, fit.measures = T, standardized = T)


# calculando os indices de confiabilidade para os diferentes modelos
semTools::reliability(fit2.1)

semTools::reliability(fit3)

# nota: os desenvolvedores desse pacote semTools estao colocando essa funcao `reliability()` em desuso, ela, teoricamente, sera substituida pela funcao `compRelSEM`

# calculando os indices especificos de bifactor -----
# usamos esse pacote abaixo que calcula todos os indices que vimos na parte teorica usando a funcao `bifactorIndices`
library(BifactorIndicesCalculator)
bifactorIndices(fit3)



# fazendo uma figura ----

# importante, aqui usei varios comandos pra deixar a figura bonita
# um tutorial mais ou menos didatico pode ser achado em: http://www.sachaepskamp.com/files/semPlot.pdf ; http://sachaepskamp.com/documentation/semPlot/semPaths.html


semPaths(fit3,
         whatLabels = "std.all",
         layout = "tree2",bifactor = "geral",
         edge.label.cex = 1.5,
         edge.label.position = .7,
         style = "lisrel",sizeMan=6,sizeMan2=4.5,label.cex = 2,
         edge.color = "black",rotation = 2,esize=4,asize = 4,
         residuals=FALSE,
         thresholds = F, intercepts = F,
         exoCov = FALSE)
