pacman::p_load(lavaan, tidyverse, easystats)
set.seed(123456)

# banco regressão linear -----
# ansiedade ~ idade + sexo + neuroticismo + extroversao 

popreg <- '
ansiedade ~ .02*idade +  .2*neuroticismo + .97*extroversao 

sexo | -.5*t1 

ansiedade ~~ 3.5*sexo

neuroticismo ~~ .35*extroversao + .29*idade 

neuroticismo ~ 1.2*sexo
extroversao ~~ .29*idade
extroversao ~ .4*sexo

idade ~~ 40*idade
'
bancoreg <- simulateData(model=popreg,
             sample.nobs = 125) %>% 
  mutate(idade = round(idade + 40, 0),
         ansiedade = ansiedade + 40,
         sexo = sexo-1) 

bancoreg <- bancoreg %>% 
  mutate(id = 1:NROW(.),
         sexo = factor(sexo, 
                       levels = c(0,1),
                       labels = c("homem", "mulher"))) %>% 
  select(id, sexo, idade, neuroticismo, extroversao, ansiedade)
summary(bancoreg)
# banco regressão logistica -----

# passoumat ~ sexo + estudo + notaport + interessemat

poplog <- '
passoumat ~ .55*estudo + .27*notaport +  .45*interessemat

sexo | -.35*t1 

sexo ~~ .25*estudo + .45*notaport + .15*interessemat

notaport ~~  .35*interessemat  + .55*estudo

notaport ~~ 100*notaport
estudo ~~ 25*estudo
'

bancolog <- simulateData(model=poplog,
                         sample.nobs = 450) %>% 
  mutate(sexo = sexo-1,
         passoumat = case_when(passoumat > -.8 ~ 1, 
                               TRUE ~0),
         passoumat = factor(passoumat,
                            levels = c(0,1),
                            labels = c("não passou", 
                                       "passou")),
         sexo = factor(sexo, 
                       levels = c(0,1),
                       labels = c("homem", "mulher")),
         notaport = notaport + 70,
         interessemat = case_when(interessemat < -.6 ~ 0,
                                  interessemat > -.6 & 
                                    interessemat < 0.5~ 1,
                                  interessemat > 0.5 ~ 2),
         interessemat = factor(interessemat),
         estudo = estudo + 14) 
summary(bancolog)
psych::describe(bancolog)


# cfa -------------

popcfa <- '
f1 =~ 0.85*q1 + 0.4*q2 + 1.25*q3 + 2*q4 + .4*q9
f2 =~ 0.35*q5 + 1.4*q6 + 1.1*q7 + 0.3*q8 + 1.43*q10


f1 =~ 0.15*q5 + .4*q6 + .1*q7 + 0.8*q8 + .23*q10

f1 + f2 ~ 0*1

f2 ~ 0.65*f1

f1 ~~ 1*f1
f2 ~~ 0.9516*f2

q1 | -2*t1 + -0.8*t2 + 0.2*t3 + 1.7*t4
q2 | -2.5*t1 + -0.9*t2 + 0.3*t3 + 1.9*t4
q3 | -1.5*t1 + -0.2*t2 + 0.8*t3 + 2.3*t4
q4 | -1.7*t1 + 0.3*t2 + 1*t3 + 2.5*t4

q5 | -2*t1 + -0.8*t2 + 0.2*t3 + 1.7*t4
q6 | -2.5*t1 + -0.9*t2 + 0.3*t3 + 1.9*t4
q7 | -1.5*t1 + -0.2*t2 + 0.8*t3 + 2.3*t4
q8 | -1.7*t1 + 0.3*t2 + 1*t3 + 2.5*t4
q9 | -1.7*t1 + 0.3*t2 + 1*t3 + 2.5*t4
q10 | -1.7*t1 + 0.3*t2 + 1*t3 + 2.5*t4
'

bancocfa <- simulateData(model=popcfa,
                         parameterization = "theta",
                         sample.nobs = 250)



# bifactor -------------

popbif <- '
g =~ 0.85*q1 + 0.4*q2 + 1.25*q3 + 2*q4 + 0.35*q5 + 1.4*q6 + 1.1*q7 + 0.3*q8

g ~~ 1*g

f1 =~ 0.65*q1 + 0.5*q2 + 1.85*q3 + .98*q4
f2 =~  0.65*q5 + 1.7*q6 + 1.34*q7 + 1.3*q8 

f2 ~ 0.65*f1

f1 ~~ 1*f1
f2 ~~ 0.9516*f2

q1 | -2*t1 + -0.8*t2 + 0.2*t3 + 1.7*t4
q2 | -2.5*t1 + -0.9*t2 + 0.3*t3 + 1.9*t4
q3 | -1.5*t1 + -0.2*t2 + 0.8*t3 + 2.3*t4
q4 | -1.7*t1 + 0.3*t2 + 1*t3 + 2.5*t4

q5 | -2*t1 + -0.8*t2 + 0.2*t3 + 1.7*t4
q6 | -2.5*t1 + -0.9*t2 + 0.3*t3 + 1.9*t4
q7 | -1.5*t1 + -0.2*t2 + 0.8*t3 + 2.3*t4
q8 | -1.7*t1 + 0.3*t2 + 1*t3 + 2.5*t4
q9 | -1.7*t1 + 0.3*t2 + 1*t3 + 2.5*t4
q10 | -1.7*t1 + 0.3*t2 + 1*t3 + 2.5*t4
'

bancobif <- simulateData(model=popbif,
                         parameterization = "theta",
                         sample.nobs = 250)

# invariance --------



popinv <- '
g =~ 0.85*q1 + 0.4*q2 + 1.25*q3 + 2*q4 + 0.35*q5 + 1.4*q6 + 1.1*q7 + 0.3*q8
g ~~ 1*g
q1 | c(-2, -.1)*t1 + -0.8*t2 + 0.2*t3 + 0.5*t4
q2 | -2.5*t1 + -0.9*t2 + 0.3*t3 + 1.9*t4
q3 | -1.5*t1 + -0.2*t2 + 0.8*t3 + 1.3*t4
q4 | -1.7*t1 + 0.3*t2 + 1*t3 + 1.5*t4

q5 | -2*t1 + -0.8*t2 + 0.2*t3 + 1.7*t4
q6 | -2.5*t1 + -0.9*t2 + 0.3*t3 + c(1.5, .9)*t4
q7 | -1.5*t1 + -0.2*t2 + 0.8*t3 + 1.3*t4
q8 | -1.7*t1 + 0.3*t2 + 1*t3 + 1.5*t4


'

bancoinv <- simulateData(model=popinv,
                         parameterization = "theta",
                         group = "group",
                         group.label = paste("G", 1:2, sep = ""),
                         sample.nobs = c(350,350))
# medidas repetidas ---------

poprep<-'
i1 =~ 1*dep_t1 + 1*dep_t2 + 1*dep_t3 + 1*dep_t4
s1 =~ 0*dep_t1 + 1*dep_t2 + 2*dep_t3 + 3*dep_t4
i1 ~~ 1*i1
s1 ~~ 0.9516*s1
s1 ~~ 0.35*i1

i2 =~ 1*ext_t1 + 1*ext_t2 + 1*ext_t3 + 1*ext_t4
s2 =~ 0*ext_t1 + 1*ext_t2 + 3*ext_t3 + 4*ext_t4
i2 ~~ 1*i2
s2 ~~ 0.9516*s2
s2 ~~ 0.55*i2

i1 ~ .34*i2
s1 ~ .45*s2
sexo ~~ .34*s1 + .37*s2
sexo | -.35*t1 
'
bancorep<-simulateData(model=poprep,
                     model.type = "growth",
                     parameterization = "theta",
                     meanstructure = T,
                     skewness = c(3,4,2,1,0,0,0,0,0),
                     kurtosis = c(2,2,3,0,0,0,0,0,0),
                     sample.nobs = 350)

bancorep <- bancorep %>% 
  mutate(
    dep_t1 = dep_t1 + 8,
    dep_t2 = dep_t2 + 12,
    dep_t3 = dep_t3 + 6,
    dep_t4 = dep_t4 + 16,
    id = 1:NROW(.)) %>% 
  select(id, everything())
psych::pairs.panels(bancorep)
dep <- bancorep %>% 
  select(id, sexo, starts_with("dep")) %>% 
  pivot_longer(cols = c(dep_t1:dep_t4),
               values_to = c("dep"),
               names_to = "time") %>% 
  mutate(time = str_remove_all(time,"dep_"))
ext <- bancorep %>% 
  select(id, sexo, starts_with("ext")) %>% 
  pivot_longer(cols = c(ext_t1:ext_t4),
               values_to = c("ext"),
               names_to = "time") %>% 
  mutate(time = str_remove_all(time,"ext_"))
long_data <- full_join(dep, ext); rm(dep, ext)
long_data <- long_data %>% 
  mutate(id = factor(id))


data_list <- list(bancobif = bancobif,
                  bancocfa = bancocfa,
                  bancoinv = bancoinv,
                  bancolog = bancolog,
                  bancoreg = bancoreg,
                  bancorep = bancorep,
                  long_data = long_data)

for(i in names(data_list)){
  write.csv(data_list[[i]], paste0(i,".csv"))
}

for(i in names(data_list)){
  haven::write_sav(data_list[[i]], paste0(i,".sav"))
}
for(i in names(data_list)){
  write_rds(data_list[[i]], paste0(i,".rds"))
}
