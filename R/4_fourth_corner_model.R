source("Data_setup.R")

data_env_tr <- model.matrix(data=data_env, ~distance_road*dist_group + dist_t1 + Name + Bare_soil + Bare_rock) |> 
  as.data.frame()
data_env_tr <- data_env_tr[,-c(1,3,4,6,7)] |> 
  rename(distX50=`distance_road:dist_group>50`,
         distX1050=`distance_road:dist_group10-50`)

data_env_tr$Name <- data_env$Name
data_env_tr$dist_group <- data_env$dist_group

data_tr_f <- data_tr |> select(Rud, Str, L, R, N)
fourth_corner_x <- gllvm(data_sp_4th/20,
                         X=data_env_tr,
                         #row.eff = "random",
                         TR=data_tr_f,
                         family = "orderedBeta",
                         formula = ~ (Bare_soil + Bare_rock + Name + dist_group + distance_road + distX50 + distX1050 + dist_t1) + (Bare_soil + Bare_rock + Name + dist_group + distance_road + distX50 + distX1050 + dist_t1):(Rud + Str + L + R + N),
                         num.lv = 0,
                         #randomX = ~ distance_road + dist_t1 + Name,
                         disp.formula = rep(1, ncol(data_sp_4th)),
                         n.init = 5,
                         trace=T,
                         sd.errors = T)
coefplot(fourth_corner_r, order = F)
coefplot(fourth_corner_s, order = F)
coefplot(fourth_corner_x, order = F)

# decide ruderality has the lowest AIC/BIC
AICc(fourth_corner_r, fourth_corner_c,fourth_corner_s, fourth_corner_x) #9968.768

BIC(fourth_corner_r, fourth_corner_c,fourth_corner_s, fourth_corner_x) #12650.40

length(fourth_corner_x$params$B)

# make gg levelplot
fc_terms <- coefplot(fourth_corner_x, which.Xcoef = 11:60, get.values = T)
fc_terms_1 <- as.data.frame(fourth_corner_x$fourth.corner)
fc_terms_1[8,] <- fc_terms_1[7,] + fc_terms_1[8,] # add slopes for distance_road
fc_terms_1[9,] <- fc_terms_1[7,] + fc_terms_1[9,]
fc_terms_1 <- fc_terms_1 |> 
  as_tibble(rownames = "env") |> 
  tidyr::pivot_longer(-"env", names_to = "trait", values_to = "estimate") |> 
  mutate( lcl = .data$estimate + qnorm(0.025) * fc_terms$sdXcoef,
          ucl = .data$estimate + qnorm(0.975) * fc_terms$sdXcoef,
          significant = !(0 > .data$lcl & 0 < .data$ucl)) 

ggplot(fc_terms_1, aes(x=env,y=trait)) +
  geom_tile(aes(fill = estimate)) +
  geom_text(aes(alpha = significant, label="X"), size=10, shape=8,
             color="black") +
  scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue') +
  labs(y="Trait", x="Environmental variable") +
  theme_minimal()
  
# make plot of non-interaction
plain_terms <- coefplot(fourth_corner_x, which.Xcoef = 1:8, get.values = T)
data.frame(coef = plain_terms$Xcoef, se = plain_terms$sdXcoef) |> 
  mutate(lcl = .data$coef + qnorm(0.025) * .data$se,
         ucl = .data$coef + qnorm(0.975) * .data$se,
         significant = !(0 > .data$lcl & 0 < .data$ucl)) |>
  # make ggplot
  ggplot(aes(x = .data$estimate, y = .data$param,
             alpha =  .data$significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "darkgrey") +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(xmin = .data$lcl, xmax = .data$ucl),
                position = position_dodge(width = 0.8)) +
  labs(x = "Change in abundance per sd. unit of distance", y = "", colour = "Legend:") +
  theme_minimal() +
  guides(alpha = "none") +
  scale_alpha_manual(values = c(0.2, 1)) +
  theme(legend.position = "bottom",
        legend.position.inside = c(0.8, 0.15),
        #legend.background = element_rect(fill = "white"),
        axis.text.y = element_text(size = 6))
  




a <- 0.25
colort <- colorRampPalette(c("red", "white", "blue"))

plot.4th <- levelplot((as.matrix(fourth_corner_x$fourth.corner)), xlab = "Environmental Variables", 
                      ylab = "Species traits", col.regions = colort(100), cex.lab = 1.3, 
                      at = seq(-a, a, length = 100), scales = list(x = list(rot = 45)))
plot.4th


fourth_corner_c <- gllvm(data_sp_4th/20,
                         X=data_env,
                         row.eff = "random",
                         TR=data_tr,
                         family = "orderedBeta",
                         formula = ~ (distance_road + dist_t1 + Name) + (distance_road + dist_t1 + Name):(Com + L + R + N),
                         num.lv = 0,
                         #randomX = ~ distance_road + dist_t1 + Name,
                         disp.formula = rep(1, ncol(data_sp_4th)),
                         n.init =5,
                         trace=T,
                         sd.errors = T)

fourth_corner_s <- gllvm(data_sp_4th/20,
                         X=data_env,
                         #row.eff = "random",
                         TR=data_tr,
                         family = "orderedBeta",
                         formula = ~ (distance_road + dist_t1 + Name) + (distance_road + dist_t1 + Name):(Str + L + R + N),
                         num.lv = 0,
                         #randomX = ~ distance_road + dist_t1 + Name,
                         disp.formula = rep(1, ncol(data_sp_4th)),
                         n.init =5,
                         trace=T,
                         sd.errors = T)

AICc(fourth_corner_r, fourth_corner_c,fourth_corner_s, fourth_corner_x)
BIC(fourth_corner_r, fourth_corner_c,fourth_corner_s, fourth_corner_x)
