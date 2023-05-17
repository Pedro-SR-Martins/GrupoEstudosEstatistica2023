# Longitudinal invariance testing for categorical items
# example based on the notes of Liu et al. (2017)
# as proposed by the authors, we would follow the basic steps of invariance testing (configural, then metric, then scalar, then residual). 
# also note: the model needs a special case of identification. Such method starts assuming that the intercepts ARE invariant across time.

# data from Newson's 2015 book. http://www.longitudinalsem.com/
library(tidyverse)
# text model:

modelex <- '
fw1 =~ w1dboth + w1dsad + w1dblues + w1ddep   
fw2 =~ w2dboth + w2dsad + w2dblues + w2ddep  
'

# fitting the base line model with semTools and the measEq.syntax function

library(lavaan)
library(semTools)
# names from repeated measures
longFacNames <- list(X = c("fw1","fw2"))
#fit model baseline (model 1)
baseline_fit <- measEq.syntax(configural.model = modelex,
                               data = socex1.1,
                               ordered = c("w1dboth","w1dsad",
                                           "w1dblues","w1ddep",
                                           "w2dboth","w2dsad",
                                           "w2dblues","w2ddep"),
                               parameterization = "theta",
                               estimator="WLSMV",
                               ID.fac = "marker", 
                               ID.cat = "millsap",
                               group = NULL, 
                               longFacNames = longFacNames,
                               return.fit = T)
summary(baseline_fit, fit.measures = T)

# loading invariance (model 2)
loading_fit <- measEq.syntax(configural.model = modelex,
                              data = socex1.1,
                              ordered = c("w1dboth","w1dsad",
                                          "w1dblues","w1ddep",
                                          "w2dboth","w2dsad",
                                          "w2dblues","w2ddep"),
                              parameterization = "theta",
                              estimator="WLSMV",
                              ID.fac = "marker", 
                              ID.cat = "millsap",
                              group = NULL, 
                              longFacNames = longFacNames,
                              long.equal = c("loadings"),
                              return.fit = T)
summary(loading_fit, fit.measures = T)
anova(baseline_fit, loading_fit)

# threshold invariance (model 3)

threshold_fit <- measEq.syntax(configural.model = modelex,
                             data = socex1.1,
                             ordered = c("w1dboth","w1dsad",
                                         "w1dblues","w1ddep",
                                         "w2dboth","w2dsad",
                                         "w2dblues","w2ddep"),
                             parameterization = "theta",
                             estimator="WLSMV",
                             ID.fac = "marker", 
                             ID.cat = "millsap",
                             group = NULL, 
                             longFacNames = longFacNames,
                             long.equal = c("loadings", "thresholds"),
                             return.fit = T)
summary(threshold_fit, fit.measures = T)
anova(baseline_fit, loading_fit, threshold_fit)


# unique factor (residual) invariance (model 4)

unique_fit <- measEq.syntax(configural.model = modelex,
                               data = socex1.1,
                               ordered = c("w1dboth","w1dsad",
                                           "w1dblues","w1ddep",
                                           "w2dboth","w2dsad",
                                           "w2dblues","w2ddep"),
                               parameterization = "theta",
                               estimator="WLSMV",
                               ID.fac = "marker", 
                               ID.cat = "millsap",
                               group = NULL, 
                               longFacNames = longFacNames,
                               long.equal = c("loadings", 
                                              "thresholds",
                                              "residuals"),
                               return.fit = T)
summary(unique_fit, fit.measures = T)
anova(baseline_fit, loading_fit, threshold_fit, unique_fit)


comp_all <- compareFit(baseline_fit, loading_fit, 
           threshold_fit, unique_fit)
#scaled fit indices
comp_all@fit %>% 
  select(ends_with("scaled"))

comp_all@fit.diff %>% 
  select(ends_with("scaled")) %>% round(.,3)


# effect size in case of scalar non-invariance could also be computed using the function provided in the supplemental material from the paper
# important: this automatic way of invariance testing always choose the first item to be the invariant one in the identification process. As pointed out by Liu et al. (2017) if the first item is NOT invariant, this could lead to trouble later. So, it is recommended to inspect the indivual fit for each wave first. Therefore, you could make an educated guess on which item is actually invariant (both metric and threshold should be roughly equal).

# also note: even though this kind of method for identification seems to work just fine, we could also use the Wu (2016) method. In the latter, we should use propositions 4 and 7 (threshold invariance first, then threshold + loadings). 
