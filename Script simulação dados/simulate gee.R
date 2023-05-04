library(simglm)


sim_args <- list(formula = y ~ 1 + time + gpa + sat + (1 + time | id), 
                 fixed = list(time = list(var_type = 'time',
                                          time_levels = c(0, 0.5, 1)),
                              gpa = list(var_type = 'continuous',
                                         mean = 15, 
                                         sd = 5),
                              sat = list(var_type = 'continuous',
                                         mean = 0, 
                                         sd = 1)),
                 
                 randomeffect = list(int_id = list(variance = 8,
                                                   var_level = 2),
                                     act_id = list(variance = 3,
                                                   var_level = 2)),
                 sample_size = list(level1 = 3, level2 = 30),
                 correlate = list(random = data.frame(x = 'int_id',
                                                      y = 'time_id',
                                                      corr = .3)),
                 reg_weights = c(4, 0.5, 0.75, 0.33),
                 transform_outcome = list(var_type = 'continuous',
                                          dist = 'rgamma', 
                                          shape = 2),
                 model_fit = list(
                   model_function = geepack::geeglm,
                   formula = y ~ 1 + time + gpa + sat ,
                   id = 'id',
                   family = Gamma,
                   corstr = 'ar1'
                 )
                 )

random_correlate <- simulate_fixed(data = NULL, sim_args) |>
  simulate_randomeffect(sim_args) |>
  simulate_error(sim_args) |> 
  generate_response(sim_args) %>% tibble()
# summary(random_correlate)
hist((random_correlate$y))

random_correlate <- random_correlate %>% 
  filter(y>12) 
glimpse(random_correlate)

long_data2 <- random_correlate %>% 
  select(id, time, gpa, sat, y) %>% 
  mutate(time = factor(time, 
                       levels = c(0,0.5,1),
                       labels = c("t1","t2","t3"))) %>% 
  rename(extroversao = y,
         satisfacao = gpa,
         depressao_z = sat) 



write.csv(long_data2, "long_data2.csv")
haven::write_sav(long_data2, "long_data2.sav")
write_rds(long_data2, "long_data2.rds")


janitor::tabyl(long_data2,
               id,time)

ggplot(data = random_correlate,
       aes(x = y,
           fill = time))+
  geom_histogram()


fit1 <- geeglm((y) ~ factor(time)+gpa+sat,
               data = random_correlate,
               id = id,
               family = gaussian("identity"),
               corstr = "unstructured",
               std.err = "san.se")


QIC(fit1)
summary(fit1)
standardize_parameters(fit1)
anova(fit1)
qqPlot(fit1$residuals, col.lines = "red")
hist(fit1$residuals)
shapiro.test(fit1$residuals) #Shapiro-Wilk test


fit2 <- geeglm((y) ~ factor(time)+gpa+sat,
               data = random_correlate,
               id = id,
               family = Gamma("identity"),
               corstr = "unstructured",
               std.err = "san.se")
QIC(fit2)
summary(fit2)
standardize_parameters(fit2)
anova(fit2)
qqPlot(fit2$residuals, col.lines = "red")
hist(fit2$residuals)
shapiro.test(fit2$residuals) #Shapiro-Wilk test
