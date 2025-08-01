# uses objects from the script "fourth corner"


fc_terms <- coefplot(fourth_corner_x, get.values = T, order=F)
fc_terms <- data.frame(coef = fc_terms$Xcoef, se = fc_terms$sdXcoef) |>
  mutate(trait=c(rep("base", 8), 
                 rep("Rud", 3),
                 rep("Com", 3),
                 rep("L", 3),
                 rep("R", 3),
                 rep("N", 3),
                 rep("Rud", 2),
                 rep("Com", 2),
                 rep("L", 2),
                 rep("R", 2),
                 rep("N", 2),
                 rep(c("Rud", "Com", "L", "R", "N"),4)),
         env= sub(":.*", "", names(fc_terms$Xcoef)),
         lcl = .data$coef + qnorm(0.025) * .data$se,
         ucl = .data$coef + qnorm(0.975) * .data$se,
         significant = !(0 > .data$lcl & 0 < .data$ucl),
         trait = fct_relevel(trait, c("Rud", "Com", "L", "R", "N")),
         env = fct_relevel(env, c("dist_group10-50", "dist_group>50", 
                                  "distance_road", "distX1050", "distX50",
                                  "dist_t1", "NameFrøya", "NameYtre_vikna"))
                 ) |>
  filter(trait != "base")

fc_terms$coef[fc_terms$env=="distX1050"] <- fc_terms$coef[fc_terms$env=="distX1050"] + fc_terms$coef[fc_terms$env=="distance_road"]
fc_terms$coef[fc_terms$env=="distX50"] <- fc_terms$coef[fc_terms$env=="distX50"] + fc_terms$coef[fc_terms$env=="distance_road"]


  # make ggplot
  ggplot(fc_terms, 
         aes(x=env,y=trait)) +
  geom_tile(aes(fill = coef)) +
  geom_text(aes(alpha = significant, label="X"), size=10, shape=8,
            color="black") +
  scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue') +
  labs(y="Trait", x="Environmental variable") +
  theme_minimal()

  # make ggplot
  ggplot(aes(x = .data$coef, y = .data$env,
             colour = .data$trait, alpha = .data$significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "darkgrey") +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = .data$lcl, xmax = .data$ucl),
                position = position_dodge(width = 0.6)) +
  labs(x = "Interaction relative to main effect", y = "", colour = "Trait:") +
  theme_minimal() +
  guides(alpha = "none") +
  scale_alpha_manual(values = c(0.3, 1)) +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "bottom",
        legend.position.inside = c(0.8, 0.15),
        #legend.background = element_rect(fill = "white"),
        axis.text.y = element_text(size = 6))
