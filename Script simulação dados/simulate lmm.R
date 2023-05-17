my_sim_data <- function(
    n_subj = 100, # number of subjects
    n_ingroup = 25, # number of ingroup stimuli
    n_outgroup = 25, # number of outgroup stimuli
    beta_0 = 800, # grand mean
    beta_1 = 50, # effect of category
    omega_0 = 80, # by-item randomintercept sd,
    tau_0 = 100, # by-subject random intercept sd,
    tau_1 = 40, # by-subject random slope sd
    rho = 0.2, # correlation between intercept and slope
    sigma = 200) { # residual (standarddeviation)
  items <- data.frame(
    item_id = seq_len(n_ingroup +
                        n_outgroup),
    category = rep(c("happy",
                     "sad"), c(n_ingroup, n_outgroup)),
    X_i = rep(c(-0.5, 0.5), c(n_ingroup,
                              n_outgroup)),
    O_0i = rnorm(n = n_ingroup +
                   n_outgroup, mean = 0, sd = omega_0))
  # variance-covariance matrix
  cov_mx <- matrix(
    c(tau_0^2, rho * tau_0 * tau_1,
      rho * tau_0 * tau_1, tau_1^2 ),
    nrow = 2, byrow = TRUE)
  subjects <- data.frame(
    subj_id = seq_len(n_subj),
    MASS::mvrnorm( n = n_subj,
                   mu = c(T_0s = 0, T_1s = 0),
                   Sigma = cov_mx))
  crossing(subjects, items) %>%
    mutate(e_si = rnorm(nrow(.), mean =
                          0, sd = sigma),
           RT = beta_0 + T_0s + O_0i +
             (beta_1 + T_1s) * X_i + e_si) %>%
    select(subj_id, item_id, category,
           X_i, RT)
}

base <- my_sim_data(n_subj = 100, 
                    beta_0 = 800,
                    sigma = 50,
                    beta_1 = 125.5)

base <- base %>% select(-X_i)

write.csv(base, "baseRT.csv")
haven::write_sav(base, "baseRT.sav")
write_rds(base, "baseRT.rds")
