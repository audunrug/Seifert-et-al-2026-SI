source("R/1_Data_setup.R")
TMB::openmp(parallel::detectCores()-1, autopar = TRUE, DLL = "gllvm")

data_env$distance_road_10 <- data_env$distance_road_raw/10
data_env_tr <- data_env |> select(Name, Plot, dist_t1, dist_group)
data_env_tr <- data_env_tr |> 
  mutate(dist_group=fct_recode(dist_group,
                    "Zone 3"=">50",
                    "Zone 2"="10-50",
                    "Zone 1"="<10"),
         dist_group=fct_relevel(dist_group,
                                "Zone 3",
                                "Zone 1",
                                "Zone 2"),
         Name=fct_relevel(Name,
                                "Smøla",
                                "Frøya",
                                "Ytre_vikna"),
         )

data_tr_f <- data_tr |> select(L, R, N, F)
fourth_corner_x <- gllvm::gllvm(data_sp_4th/20,
                         X=data_env_tr,
                         #row.eff = "random",
                         TR=data_tr_f,
                         family = "orderedBeta",
                         formula = ~ (dist_t1 + Name + dist_group) + (dist_t1 + Name + dist_group):(L + R + N + F),
                         num.lv = 0,
                         randomX = ~ dist_t1 + Name + dist_group,
                         disp.formula = rep(1, ncol(data_sp_4th)),
                         n.init = 2,
                         trace=T,
                         sd.errors = T)

# diagnostics
plot(fourth_corner_x) # looks good

#

fc_terms <- fourth_corner_x$fourth.corner
fc_terms_sd <- fourth_corner_x$sd$B[-(1:5)]
fc_terms_long <- fc_terms |> 
  as_tibble(rownames = "env") |> 
  tidyr::pivot_longer(-"env", names_to = "trait", values_to = "estimate") |> 
  mutate( lcl = .data$estimate + qnorm(0.025) * fc_terms_sd,
          ucl = .data$estimate + qnorm(0.975) * fc_terms_sd,
          significant = !(0 > .data$lcl & 0 < .data$ucl)) 

ggplot(fc_terms_long, aes(x=env,y=trait)) +
  geom_tile(aes(fill = estimate)) +
  geom_text(aes(alpha = significant, label="*"), size=10,
             color="black") +
  scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue') +
  scale_alpha_manual(values = c(0, 1)) +
  labs(y="EIV",x="Environmental variable") + 
  theme_minimal() +
  scale_x_discrete(labels=c("Zone 1", "Zone 2", "Distance from turbine",
                            "F4", "Y12"))
  
