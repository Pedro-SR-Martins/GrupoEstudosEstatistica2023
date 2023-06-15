# carregando pacotes 
pacman::p_load(tidyverse, # manipulacao
               lavaan, # roda cfa
               semTools, # comparacao modelos
               semPlot) # graficos
base <- readRDS(url("https://github.com/Pedro-SR-Martins/GrupoEstudosEstatistica2023/raw/main/bancos/bancomtmm.rds", method="libcurl"))

glimpse(base)

psych::describe(base)
# para organizar, comeco com so os tracos

tracos <- '
dep =~ dass1 + dass2 + dass3 +   dass4 +   ind1 + ind2 + ind3 + ind4
ans =~ dass5 + dass6 + dass7 +   dass8 +   ind5 + ind6 + ind7 + ind8
est =~ dass9 + dass10 + dass11 + dass12 +  ind9 + ind10+ind11+ ind12
'
fit_tracos <- cfa(model = tracos,
                   estimator = "WLSMV",
                   data = base,
                   std.lv = T,
                   ordered = names(base))
summary(fit_tracos,
        standardized = T,
        fit.measures = T)
# depois so os metodos

metodos <- '
dass =~  dass1 + dass2 + dass3 + dass4 + dass5 + dass6 + dass7 + dass8 + dass9 + dass10 + dass11 + dass12
ind =~   ind1 +  ind2 + ind3 +  ind4 +  ind5 +  ind6 +  ind7 +  ind8 +  ind9 +  ind10 +  ind11 +  ind12
'

fit_metodos <- cfa(model = metodos,
                  estimator = "WLSMV",
                  data = base,
                  std.lv = T,
                  ordered = names(base))
summary(fit_metodos,
        standardized = T,
        fit.measures = T)
# Step 1 — The CTCM Model - correlated traits/correlated methods------

model1 <- '
# tracos
dep =~ dass1 + dass2 + dass3 +   dass4 +   ind1 + ind2 + ind3 + ind4
ans =~ dass5 + dass6 + dass7 +   dass8 +   ind5 + ind6 + ind7 + ind8
est =~ dass9 + dass10 + dass11 + dass12 +  ind9 + ind10+ind11+ ind12
# metodos
dass =~  dass1 + dass2 + dass3 + dass4 + dass5 + dass6 + dass7 + dass8 + dass9 + dass10 + dass11 + dass12
ind =~   ind1 +  ind2 + ind3 +  ind4 +  ind5 +  ind6 +  ind7 +  ind8 +  ind9 +  ind10 +  ind11 +  ind12

# tracos e metodos nao correlacionam
dass ~~ 0*dep + 0*ans + 0*est
ind ~~ 0*dep + 0*ans + 0*est

'
fit_model1 <- cfa(model = model1,
                   estimator = "WLSMV",
                   data = base,
                   std.lv = T,
                   ordered = names(base))
summary(fit_model1,
        standardized = T,
        fit.measures = T)

semPaths(fit_model1,
         whatLabels = "std.all",
         layout = "tree2",bifactor = c("est", "ans", "dep"),
         edge.label.cex = .5,
         edge.label.position = .5,
         style = "lisrel",sizeMan=6,sizeMan2=2,label.cex = 1,
         edge.color = "black",rotation = 2,esize=1,asize = 1,
         residuals=FALSE,
         thresholds = F, intercepts = F,
         exoCov = TRUE)


# Step 2 — The NTCM Model - no traits/correlated methods------

model2 <- '
dass =~  dass1 + dass2 + dass3 + dass4 + dass5 + dass6 + dass7 + dass8 + dass9 + dass10 + dass11 + dass12
ind =~   ind1 +  ind2 + ind3 +  ind4 +  ind5 +  ind6 +  ind7 +  ind8 +  ind9 +  ind10 +  ind11 +  ind12
'
fit_model2 <- cfa(model = model2,
                  estimator = "WLSMV",
                  data = base,
                  std.lv = T,
                  ordered = names(base))
summary(fit_model2,
        standardized = T,
        fit.measures = T)

semPaths(fit_model2,
         whatLabels = "std.all",
         layout = "tree2",
         edge.label.cex = .5,
         edge.label.position = .5,
         style = "lisrel",sizeMan=6,sizeMan2=2,label.cex = 1,
         edge.color = "black",rotation = 2,esize=1,asize = 1,
         residuals=FALSE,
         thresholds = F, intercepts = F,
         exoCov = TRUE)

# Step 3 — The PCTCM Model - Perfectly Correlated Traits/Freely Correlated Methods-------
model3 <- '
# tracos
dep =~ dass1 + dass2 + dass3 +   dass4 +   ind1 + ind2 + ind3 + ind4
ans =~ dass5 + dass6 + dass7 +   dass8 +   ind5 + ind6 + ind7 + ind8
est =~ dass9 + dass10 + dass11 + dass12 +  ind9 + ind10+ind11+ ind12
# metodos
dass =~  dass1 + dass2 + dass3 + dass4 + dass5 + dass6 + dass7 + dass8 + dass9 + dass10 + dass11 + dass12
ind =~   ind1 +  ind2 + ind3 +  ind4 +  ind5 +  ind6 +  ind7 +  ind8 +  ind9 +  ind10 +  ind11 +  ind12

# tracos e metodos nao correlacionam
dass ~~ 0*dep + 0*ans + 0*est
ind ~~ 0*dep + 0*ans + 0*est

# tracos perfeitamente correlacionados
dep ~~ 1*ans + 1*est
ans ~~ 1*est
'
fit_model3 <- cfa(model = model3,
                  estimator = "WLSMV",
                  data = base,
                  std.lv = T,
                  ordered = names(base))
summary(fit_model3,
        standardized = T,
        fit.measures = T)

semPaths(fit_model3,
         whatLabels = "std.all",
         layout = "tree2",bifactor = c("est", "ans", "dep"),
         edge.label.cex = .5,
         edge.label.position = .5,
         style = "lisrel",sizeMan=6,sizeMan2=2,label.cex = 1,
         edge.color = "black",rotation = 2,esize=1,asize = 1,
         residuals=FALSE,
         thresholds = F, intercepts = F,
         exoCov = TRUE)
# Step 4 — The CTUM Model - Freely Correlated Traits/Uncorrelated Methods -----

model4 <- '
# tracos
dep =~ dass1 + dass2 + dass3 +   dass4 +   ind1 + ind2 + ind3 + ind4
ans =~ dass5 + dass6 + dass7 +   dass8 +   ind5 + ind6 + ind7 + ind8
est =~ dass9 + dass10 + dass11 + dass12 +  ind9 + ind10+ind11+ ind12
# metodos
dass =~  dass1 + dass2 + dass3 + dass4 + dass5 + dass6 + dass7 + dass8 + dass9 + dass10 + dass11 + dass12
ind =~   ind1 +  ind2 + ind3 +  ind4 +  ind5 +  ind6 +  ind7 +  ind8 +  ind9 +  ind10 +  ind11 +  ind12

# tracos e metodos nao correlacionam
dass ~~ 0*dep + 0*ans + 0*est
ind ~~ 0*dep + 0*ans + 0*est

# metodos nao correlacionam entre si
dass ~~ 0*ind
'
fit_model4 <- cfa(model = model4,
                  estimator = "WLSMV",
                  data = base,
                  std.lv = T,
                  ordered = names(base))
summary(fit_model4,
        standardized = T,
        fit.measures = T)

semPaths(fit_model4,
         whatLabels = "std.all",
         layout = "tree2",bifactor = c("est", "ans", "dep"),
         edge.label.cex = .5,
         edge.label.position = .5,
         style = "lisrel",sizeMan=6,sizeMan2=2,label.cex = 1,
         edge.color = "black",rotation = 2,esize=1,asize = 1,
         residuals=FALSE,
         thresholds = F, intercepts = F,
         exoCov = TRUE)

# avaliacao das evidencias de validade (convergente divergente)------



# validade convergente - se o modelo 1 for melhor -----
summary(compareFit(fit_model1,fit_model2),
        fit.measures = c("chisq.scaled", "df.scaled", 
                         "pvalue.scaled", "cfi.scaled"))
# usar as mesmas regras do teste de invariancia
# diferencas de CFI maiores que 0.01 sao problema
# se o qui-quadrado for sig tambem eh problema
# qui quadrado quanto menor melhor

# se o modelo 1 for melhor, ha indicacao de que ha convergencia entre os construtos, ou seja, nao ha apenas variancia de metodo
# se o modelo dois for melhor, isso indica que so os metodos (escalas, neste caso) explicam o jeito de responder as perguntas


# validade discriminante 1 -------


summary(compareFit(fit_model1, fit_model3),
        fit.measures = c("chisq.scaled", "df.scaled", 
                         "pvalue.scaled", "cfi.scaled"))
# caso os dois modelos sejam bons, indica que os tracos nao se diferenciam entre si
# no modelo 2, os tracos sao iguais entre si, ou seja, nao seriam variaveis latentes diferentes, normalmente, nao eh o que queremos



# validade discriminante 2 -------
summary(compareFit(fit_model1, fit_model4),
        fit.measures = c("chisq.scaled", "df.scaled", 
                         "pvalue.scaled", "cfi.scaled"))
# caso haja diferencas, pode ser que tenha um vies comum de metodo
# desse jeito, fica parecendo que nao ha discriminacao entre os metodos
# voce quer que os modelos sejam iguais, porque nesse caso a correlacao entre os metodos nao impacta a avaliacao dos tracos


# interpretacao das cargas fatoriais -----
# voce pode olhar para as cargas e ver se o efeito do traco eh melhor que o efeito do metodo
# ou seja, voce quer que a carga fatorial do traco seja maior que a do metodo
standardizedsolution(fit_model1)
View(standardizedsolution(fit_model1))
