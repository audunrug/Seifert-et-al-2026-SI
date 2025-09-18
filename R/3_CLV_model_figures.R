# load model object from "models" directory
load("models/CLV_model.Rdata")

#### plotting coefs (adapted from Wards code)
# load coef estimates + se
estim <- coefplot.gllvm(mod_cc_bet, get.values=T)
par(mfrow=c(1,1))
estimates <- estim$Xcoef |> 
  as_tibble(rownames = "species")

# combine coefs for distance groups 
estimates$`dist_group>50:distance_road` <- estimates$distance_road + estimates$`dist_group>50:distance_road`
estimates$`dist_group10-50:distance_road` <- estimates$distance_road + estimates$`dist_group10-50:distance_road`

estimates <- estimates |> # coefs
  tidyr::pivot_longer(-"species", names_to = "param", values_to = "estimate") 
ses_gllvm_model <- estim$sdXcoef %>% #s.es
  as_tibble(rownames = "species") %>%
  tidyr::pivot_longer(-"species", names_to = "param", values_to = "se")

# plot coef for distance slopes
p1 <- full_join(estimates, ses_gllvm_model,
          by = join_by("param", "species")) %>%
  mutate(order = .data$estimate[8], .by = "species") %>%
  mutate(
    lcl = .data$estimate + qnorm(0.025) * .data$se,
    ucl = .data$estimate + qnorm(0.975) * .data$se,
    significant = !(0 > .data$lcl & 0 < .data$ucl),
    species = reorder(.data$species, order)
  ) |> 
  filter(param %in% c("distance_road", 
                      "dist_group10-50:distance_road", 
                      "dist_group>50:distance_road")) |> 
  mutate(param = fct_recode(param, 
                            "10-50 m" = "dist_group10-50:distance_road",
                            ">50 m" = "dist_group>50:distance_road",
                            "<10 m" = "distance_road")) |> 
  ggplot(
    aes(x = .data$estimate, y = .data$species,
        colour = .data$param, alpha =  .data$significant)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "darkgrey") +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(xmin = .data$lcl, xmax = .data$ucl),
                position = position_dodge(width = 0.8)) +
  labs(x = "Slope (per 10 meter)", 
       y = "Species",
       title = "(B) Distance to nearest road",
       colour = "Distance interval:") +
  theme_minimal() +
  guides(alpha = "none") +
  scale_alpha_manual(values = c(0.2, 1)) +
  theme(legend.position = "bottom",
        legend.position.inside = c(0.8, 0.15),
        #legend.background = element_rect(fill = "white"),
        axis.text.y = element_text(size = 6)) +
  scale_color_brewer(palette="Set2") 
p1

# plot difference between categories
p2 <- full_join(estimates, ses_gllvm_model,
          by = join_by("param", "species")) %>%
  mutate(order = .data$estimate[8], .by = c("species")) %>%
  mutate(
    lcl = .data$estimate + qnorm(0.025) * .data$se,
    ucl = .data$estimate + qnorm(0.975) * .data$se,
    significant = !(0 > .data$lcl & 0 < .data$ucl),
    species = reorder(.data$species, order)
  ) |> 
  filter(param %in% c("dist_group10-50", "dist_group>50")) |> 
  mutate(param = fct_recode(param, 
                            "10-50 m" = "dist_group10-50",
                            ">50 m" = "dist_group>50")) |>  
  ggplot(
    aes(x = .data$estimate, y = .data$species,
        colour = .data$param, alpha =  .data$significant)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "darkgrey") +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(xmin = .data$lcl, xmax = .data$ucl),
                position = position_dodge(width = 0.8)) +
  labs(x = "Intercept relative to <10 meters ", 
       title = "(A) Distance to nearest road",
       y = "Species", colour = "Distance interval:") +
  theme_minimal() +
  guides(alpha = "none") +
  scale_alpha_manual(values = c(0.2, 1)) +
  theme(legend.position = "bottom",
        #legend.background = element_rect(fill = "white"),
        axis.text.y = element_text(size = 6)) +
  scale_color_brewer(palette="Set2") 
p2

# plot difference between sites
p3 <- full_join(estimates, ses_gllvm_model,
                by = join_by("param", "species")) %>%
  mutate(order = .data$estimate[8], .by = c("species")) %>%
  mutate(
    lcl = .data$estimate + qnorm(0.025) * .data$se,
    ucl = .data$estimate + qnorm(0.975) * .data$se,
    significant = !(0 > .data$lcl & 0 < .data$ucl),
    species = reorder(.data$species, order)
  ) |> 
  filter(param %in% c("NameSmøla", "NameYtre_vikna")) |> 
  mutate(param = fct_recode(param, 
                            "Smøla" = "NameSmøla",
                            "Ytre vikna" = "NameYtre_vikna")) |>  
  ggplot(
    aes(x = .data$estimate, y = .data$species,
        colour = .data$param, alpha =  .data$significant)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "darkgrey") +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(xmin = .data$lcl, xmax = .data$ucl),
                position = position_dodge(width = 0.8)) +
  labs(x = "Intercept relative to Frøya ", 
       title = "(D) Site",
       y = "Species", colour = "Site:") +
  theme_minimal() +
  guides(alpha = "none") +
  scale_alpha_manual(values = c(0.2, 1)) +
  theme(legend.position = "bottom",
        #legend.background = element_rect(fill = "white"),
        axis.text.y = element_text(size = 6)) +
scale_color_brewer(palette="Set1") 
p3

# distance_t1
p4 <- full_join(estimates, ses_gllvm_model,
                by = join_by("param", "species")) %>%
  mutate(order = .data$estimate[8], .by = c("species")) %>%
  mutate(
    lcl = .data$estimate + qnorm(0.025) * .data$se,
    ucl = .data$estimate + qnorm(0.975) * .data$se,
    significant = !(0 > .data$lcl & 0 < .data$ucl),
    species = reorder(.data$species, order)
  ) |> 
  filter(param %in% c("Bare_soil", "Bare_rock")) |> 
  ggplot(
    aes(x = .data$estimate, y = .data$species, alpha =  .data$significant,
        colour = .data$param)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "darkgrey") +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(xmin = .data$lcl, xmax = .data$ucl),
                position = position_dodge(width = 0.8)) +
  labs(x = "Slope (per sd. unit)", 
       y = "Species", 
       title = "(C) % Bare rock and -soil",
       colour = "Predictor:") +
  theme_minimal() +
  scale_color_brewer(palette="Dark2") +
  guides(alpha = "none") +
  scale_alpha_manual(values = c(0.2, 1)) +
  theme(legend.position = "bottom",
        #legend.background = element_rect(fill = "white"),
        axis.text.y = element_text(size = 6))
p4






# plot together
ggpubr::ggarrange(p2,p1, 
                  common.legend = T, 
                  legend = "bottom",
                  legend.grob = ggpubr::get_legend(p1))
ggpubr::ggarrange(p4,p3, 
                  common.legend = F, 
                  legend = "bottom")


# residual corrplot
par(mfrow=c(1,1))
cr0 <- getResidualCor.gllvm(mod_cc_bet)
corrplot(cr0[order.single(cr0), order.single(cr0)], diag = F, type = "lower", 
         method = "square", tl.cex = 0.8, tl.srt = 45, tl.col = "darkgrey")

ordiplot(mod_cc_bet, biplot = T, get.coords = T)
summary(mod_cc_bet)
