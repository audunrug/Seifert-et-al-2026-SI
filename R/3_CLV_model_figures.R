# load model object from "models" directory
load("models/CLV_model.Rdata")

#### plotting coefs (adapted from Wards code)
# load coef estimates + se
estim <- coefplot.gllvm(mod_cc_bet, get.values=T)
par(mfrow=c(1,1))
estimates <- estim$Xcoef |> 
  as_tibble(rownames = "species")

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
  filter(param %in% c("dist_groupZone 1:distance_road", 
                      "dist_groupZone 2:distance_road", 
                      "dist_groupZone 3:distance_road")) |> 
  mutate(param = fct_recode(param, 
                            "Zone 1" = "dist_groupZone 1:distance_road",
                            "Zone 2" = "dist_groupZone 2:distance_road",
                            "Zone 3" = "dist_groupZone 3:distance_road")) |> 
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
       colour = "Buffer zone:") +
  theme_minimal() +
  guides(alpha = "none") +
  scale_alpha_manual(values = c(0.2, 1)) +
  theme(legend.position = "bottom",
        legend.position.inside = c(0.8, 0.15),
        #legend.background = element_rect(fill = "white"),
        axis.text.y = element_text(size = 6)) +
  scale_colour_manual(values = c("#418099", "#84D565", "#FEE949"))
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
  filter(param %in% c("dist_groupZone 1", 
                      "dist_groupZone 2")) |> 
  mutate(param = fct_recode(param, 
                            "Zone 1" = "dist_groupZone 1",
                            "Zone 2" = "dist_groupZone 2")) |>  
  ggplot(
    aes(x = .data$estimate, y = .data$species,
        colour = .data$param, alpha =  .data$significant)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "darkgrey") +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(xmin = .data$lcl, xmax = .data$ucl),
                position = position_dodge(width = 0.8)) +
  labs(x = "Intercept relative to Zone 3", 
       title = "(A) Distance to nearest road",
       y = "Species", colour = "Buffer zone:") +
  theme_minimal() +
  guides(alpha = "none") +
  scale_alpha_manual(values = c(0.2, 1)) +

  theme(legend.position = "bottom",
        #legend.background = element_rect(fill = "white"),
        axis.text.y = element_text(size = 6)) +
  scale_colour_manual(values = c("#418099", "#84D565"))
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
  filter(param %in% c("NameFrøya", "NameYtre_vikna")) |> 
  mutate(param = fct_recode(param, 
                            "F4" = "NameFrøya",
                            "Y12" = "NameYtre_vikna")) |>  
  ggplot(
    aes(x = .data$estimate, y = .data$species,
        colour = .data$param, alpha =  .data$significant)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "darkgrey") +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(xmin = .data$lcl, xmax = .data$ucl),
                position = position_dodge(width = 0.8)) +
  labs(x = "Intercept relative to S19 ", 
       title = "(D) Site",
       y = "Species", colour = "Site:") +
  theme_minimal() +
  guides(alpha = "none") +
  scale_alpha_manual(values = c(0.2, 1)) +
  theme(legend.position = "bottom",
        #legend.background = element_rect(fill = "white"),
        axis.text.y = element_text(size = 6)) +
  scale_colour_manual(values = c("black", "red"))
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
