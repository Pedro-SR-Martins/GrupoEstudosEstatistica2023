pacman::p_load(lavaan, tidyverse, easystats)
set.seed(123456)


# MTMM ---------

popmtmm <- '

dep =~ .8*dass1 + .64*dass2 + .58*dass3 +   .49*dass4 +   .75*ind1 + .71*ind2 + .81*ind3 + .56*ind4
ans =~ .8*dass5 + .64*dass6 + .58*dass7 +   .49*dass8 +   .75*ind5 + .71*ind6 + .81*ind7 + .56*ind8
est =~ .8*dass9 + .64*dass10 +.58* dass11 + .49*dass12 +  .75*ind9 + .71*ind10+ .81*ind11+ .56*ind12

dass =~ .25*dass1 + .32*dass2 + .22*dass3 + .15*dass4 + .21*dass5 + .17*dass6 + .23*dass7 + .11*dass8 + .25*dass9 + .16*dass10 + .08*dass11 + .22*dass12
ind =~  .25*ind1 +  .32*ind2 +  .22*ind3 +  .15*ind4 +  .21*ind5 +  .17*ind6 +  .23*ind7 +  .11*ind8 +  .25*ind9 +  .16*ind10 +  .08*ind11 +  .22*ind12


dass ~~ .15*ind

dep ~~ .7*ans +.65*est
est ~~ .7*ans

dass ~~ 0*dep + 0*ans + 0*est

ind ~~ 0*dep + 0*ans + 0*est

dass1 | -2*t1 + -0.8*t2 + 0.2*t3 + 1.7*t4
dass2 | -2.5*t1 + -0.9*t2 + 0.3*t3 +  .9*t4
dass3 | -1.5*t1 + -0.2*t2 + 0.8*t3 + 1.3*t4
dass4 | -1.7*t1 + 0.3*t2 + 1*t3 + 1.5*t4
dass5 | -2*t1 + -0.8*t2 + 0.2*t3 + 1.7*t4
dass6 | -2.5*t1 + -0.9*t2 + 0.3*t3 + .9*t4
dass7 | -1.5*t1 + -0.2*t2 + 0.8*t3 + 1.3*t4
dass8 | -1.7*t1 + 0.3*t2 + 1*t3 + 1.5*t4
dass9 | -2*t1 + -0.8*t2 + 0.2*t3 + 1.7*t4
dass10 | -2.5*t1 + -0.9*t2 + 0.3*t3 + .9*t4
dass11 | -1.5*t1 + -0.2*t2 + 0.8*t3 + 1.3*t4
dass12   | -1.7*t1 + 0.3*t2 + 1*t3 + 1.5*t4

ind1 | -1.5*t1  + 0.8*t3
ind2 | -1.5*t1  + 0.8*t3
ind3 | -1.5*t1  + 0.8*t3
ind4 | -1.5*t1  + 0.8*t3
ind5 | -1.5*t1  + 0.8*t3
ind6 | -1.5*t1  + 0.8*t3
ind7 | -1.5*t1  + 0.8*t3
ind8 | -1.5*t1  + 0.8*t3
ind9 | -1.5*t1  + 0.8*t3
ind10 | -1.5*t1 + 0.8*t3
ind11 | -1.5*t1 + 0.8*t3
ind12 | -1.5*t1 + 0.8*t3


'

bancomtmm <- simulateData(model=popmtmm,
                          parameterization = "theta",
                          sample.nobs = 250)
mod1 <- '
dep =~ dass1 + dass2 + dass3 +   dass4 +   ind1 + ind2 + ind3 + ind4
ans =~ dass5 + dass6 + dass7 +   dass8 +   ind5 + ind6 + ind7 + ind8
est =~ dass9 + dass10 + dass11 + dass12 +  ind9 + ind10+ind11+ ind12

dass =~  NA*dass1 + dass2 + dass3 + dass4 + dass5 + dass6 + dass7 + dass8 + dass9 + dass10 + dass11 + dass12
ind =~   NA*ind1 +  ind2 + ind3 +  ind4 +  ind5 +  ind6 +  ind7 +  ind8 +  ind9 +  ind10 +  ind11 +  ind12
dass ~~ 0*dep + 0*ans + 0*est

ind ~~ 0*dep + 0*ans + 0*est



'
fit1 <- cfa(model = mod1,
            estimator = "wlsmv",
            data = bancomtmm,
            std.lv = T,
            ordered = names(bancomtmm)
)
summary(fit1, 
        standardized = T,
        fit.measures = T)
semPlot::semPaths(fit1,
                  layout = "tree3",
                  bifactor = c("ind", "dass"),
                  edge.label.cex = 1.5,
                  edge.label.position = .7,
                  style = "lisrel",
                  sizeMan=6,sizeMan2=4.5,
                  label.cex = 2,
                  edge.color = "black",
                  rotation = 2,esize=4,asize = 4,
                  residuals=FALSE,
                  thresholds = F, intercepts = F,
                  exoCov = FALSE)
source("https://raw.githubusercontent.com/Pedro-SR-Martins/my_funs/main/aux_fa")
loads(fit1)
