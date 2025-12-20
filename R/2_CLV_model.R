source("R/1_Data_setup.R")
TMB::openmp(parallel::detectCores()-1, autopar = TRUE, DLL = "gllvm")
library(splines2)

data_env$distance_road_10 <- data_env$distance_road_raw/10
# change reference categories
data_env <- data_env |> 
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


# tweak data for gllvm
Y <- data_sp_f[,-c(1,2)]
Y <- Y[,order(colSums(Y>0),decreasing = TRUE)] # order species by most common
Y <- rename(Y, `Hieracium sp.` = `Hieracium sp `) |># fix typo
  select(-Tvebladmose)

# concurrent ordination with 2 LVs
mod_cc_bet <- gllvm(y=Y/20,
                    X=data_env,
                    family = "orderedBeta", # ordbet
                    method="EVA",
                    #row.eff = "random",
                    lv.formula = ~ dist_group/distance_road + Bare_rock +
                      Bare_soil + Name,
                    num.lv.c = 2,
                    disp.formula = rep(1, ncol(Y)),
                    n.init =5,
                    trace=T,
                    seed=2,
                    sd.errors = T)

## diagnostics
plot(mod_cc_bet) # ok

# sign of estimator variances
table(sign(diag(mod_cc_bet$Hess$cov.mat.mod))) # good

# plot gradient vector
plot(c(mod_cc_bet$TMBfn$gr(mod_cc_bet$TMBfn$par))) # not great


## summary
summary(mod_cc_bet) # looks fine (no negative probabilities)








