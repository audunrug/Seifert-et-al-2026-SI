# load model object from "models" directory
load("models/fourth_corner.Rdata")

# get 4c terms
fc_terms <- coefplot(fourth_corner_x, get.values = T, order=F)

fc_terms_2 <- fc_terms$Xcoef
sim_x <- seq(0, 100, by=1)
sim_x <- sim_y <- c(1:10, 1:40, 1:50)

# base effect of road
sim_y <- sim_y*(fc_terms_2["distance_road"]/10) # distance to road main effect
sim_y[11:50] <- sim_y[11:50] + sim_x[11:50]*(fc_terms_2["distX1050"]/10)
sim_y[11:50] <- sim_y[11:50] + fc_terms_2["dist_group10-50"]
sim_y[51:100] <- sim_y[51:100] + sim_x[51:100]*(fc_terms_2["distX50"]/10)
sim_y[51:100] <- sim_y[51:100] + fc_terms_2["dist_group>50"]

# effect of Rud
sim_rud <- sim_x*(fc_terms_2["distance_road:Rud"]/10) # distance to road main effect
sim_rud[11:50] <- sim_rud[11:50] + sim_x[11:50]*(fc_terms_2["distX1050:Rud"]/10)
sim_rud[11:50] <- sim_rud[11:50] + fc_terms_2["dist_group10-50:Rud"]
sim_rud[51:100] <- sim_rud[51:100] + sim_x[51:100]*(fc_terms_2["distX50:Rud"]/10)
sim_rud[51:100] <- sim_rud[51:100] + fc_terms_2["dist_group>50:Rud"]
sim_rud <- sim_y + sim_rud

# effect of Str
sim_str <- sim_x*(fc_terms_2["distance_road:Str"]/10) # distance to road main effect
sim_str[11:50] <- sim_str[11:50] + sim_x[11:50]*(fc_terms_2["distX1050:Str"]/10)
sim_str[11:50] <- sim_str[11:50] + fc_terms_2["dist_group10-50:Str"]
sim_str[51:100] <- sim_str[51:100] + sim_x[51:100]*(fc_terms_2["distX50:Str"]/10)
sim_str[51:100] <- sim_str[51:100] + fc_terms_2["dist_group>50:Str"]
sim_str <- sim_y + sim_str


# effect of R
sim_R <- sim_x*(fc_terms_2["distance_road:R"]/10) # distance to road main effect
sim_R[11:50] <- sim_R[11:50] + sim_x[11:50]*(fc_terms_2["distX1050:R"]/10)
sim_R[11:50] <- sim_R[11:50] + fc_terms_2["dist_group10-50:R"]
sim_R[51:100] <- sim_R[51:100] + sim_x[51:100]*(fc_terms_2["distX50:R"]/10)
sim_R[51:100] <- sim_R[51:100] + fc_terms_2["dist_group>50:R"]
sim_R <- sim_y + sim_R

# Effect of N
sim_N <- sim_x*(fc_terms_2["distance_road:N"]/10) # distance to road main effect
sim_N[11:50] <- sim_N[11:50] + sim_x[11:50]*(fc_terms_2["distX1050:N"]/10)
sim_N[11:50] <- sim_N[11:50] + fc_terms_2["dist_group10-50:N"]
sim_N[51:100] <- sim_N[51:100] + sim_x[51:100]*(fc_terms_2["distX50:N"]/10)
sim_N[51:100] <- sim_N[51:100] + fc_terms_2["dist_group>50:N"]
sim_N <- sim_y + sim_N

# effect of L
sim_L <- sim_x*(fc_terms_2["distance_road:L"]/10) # distance to road main effect
sim_L[11:50] <- sim_L[11:50] + sim_x[11:50]*(fc_terms_2["distX1050:L"]/10)
sim_L[11:50] <- sim_L[11:50] + fc_terms_2["dist_group10-50:L"]
sim_L[51:100] <- sim_L[51:100] + sim_x[51:100]*(fc_terms_2["distX50:L"]/10)
sim_L[51:100] <- sim_L[51:100] + fc_terms_2["dist_group>50:L"]
sim_L <- sim_y + sim_L


# make plot df
plot_df <- data.frame("dist" = rep(seq(1, 100, by=1),6),
                      "value" = c(sim_y,sim_rud,sim_str, sim_R, sim_N, sim_L),
                      "zone" = as.factor(rep(c(rep("0", 10), rep("10", 40), rep("50", 50)), 6)),
                      "type"= rep(c("base", "Rud", "Str", "R", "N", "L"), each=100))
plot_df$zonetype <- paste0(plot_df$zone, plot_df$type)
plot_df$type <- as.factor(plot_df$type)

# plot fourth corner terms by road
ggplot(plot_df) +
  geom_line(aes(x=dist, y=value, color=type, group= c(zonetype), size=type)) +
  scale_color_manual(values=c("darkgrey", "red", "darkred", "salmon", "blue", "steelblue")) +
  scale_size_manual(values=c(3,rep(0.6, 5))) +
  theme_minimal()


plot_df_inter <- data.frame(coef = fc_terms$Xcoef, se = fc_terms$sdXcoef)
  






#plot_df_inter <- data.frame(coef = fc_terms$Xcoef, se = fc_terms$sdXcoef)
plot_df_inter <- as.data.frame(fourth_corner_x$fourth.corner)
plot_df_inter$env <- rownames(plot_df_inter)
plot_df_inter <- pivot_longer(plot_df_inter, cols = 1:5, names_to = "trait")



# get 4c terms
fc_terms <- coefplot(fourth_corner_x, get.values = T, order=F)
fc_terms <- data.frame(coef = fc_terms$Xcoef, se = fc_terms$sdXcoef) |> 
  mutate(trait=sub("^.*?:", "", names(fc_terms$Xcoef)),
         env= sub(":.*", "", names(fc_terms$Xcoef)),
         lcl = .data$coef + qnorm(0.025) * .data$se,
         ucl = .data$coef + qnorm(0.975) * .data$se,
         significant = !(0 > .data$lcl & 0 < .data$ucl),
         trait = fct_recode(trait, "CSR Ruderal"="Rud", 
                                      "CSR Stress"="Str", 
                                      "EIV L"="L", 
                                      "EIV R"="R", 
                                      "EIV N"="N"),
         env = fct_recode(env, "Bare rock"="Bare_rock",
                          "Bare soil"="Bare_soil", 
                                 "Road 10-50m"="dist_group10-50",
                                 "Road >50m"="dist_group>50",
                                 "Dist. W.T." = "dist_t1",
                                 "Rd. dist." = "distance_road",
                                 "Rd. dist. x 10-50m" = "distX1050",
                                 "Rd. dist. x >50m" = "distX50",
                                 "Site Smøla" = "NameSmøla",
                                 "Site Ytre vikna" = "NameYtre_vikna")
         )

fc_terms$env  <- factor(fc_terms$env,levels=c(
  "Rd. dist.",
  "Road 10-50m",
  "Rd. dist. x 10-50m",
  "Road >50m",
  "Rd. dist. x >50m",
  "Dist. W.T.",
  "Bare rock", 
  "Bare soil",
  "Site Smøla",
  "Site Ytre vikna"))                                 
                                 

# make ggplot
ggplot(fc_terms |>filter(trait %in% c("CSR Ruderal", 
                                        "CSR Stress", 
                                        "EIV L", 
                                        "EIV R", 
                                        "EIV N")),
       aes(x=env,y=trait)) +
  geom_tile(aes(fill = coef)) +
  geom_text(aes(alpha = significant, label="*"), size=10,
            color="black") +
  scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue') +
  labs(y="Trait", x="Environmental variable", fill="Association",
       alpha="Signifiance") +
  scale_alpha_manual(values=c(0,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust=1))                                                    
                                 
                                 
                                 
           


#### Old stuff ####

# plot WPPS
ggplot(plot_df_inter |> filter(env %in% c("NameSmøla", "NameYtre_vikna")),
       aes(x=env, y=value,color=trait, group=trait)) +
  geom_point(position = position_dodge(width = 0.5))
  geom_linerange(aes(ymin = lcl, ymax = ucl),
                 
                 position = position_dodge(width = 0.5)) +
  theme_minimal()


# plot 
ggplot(plot_df_inter |> filter(env %in% c("NameSmøla", "NameYtre_vikna"),
                               trait != "base"),
       aes(x=env, y=coef,color=trait, group=trait)) +
  geom_point(
    position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lcl, ymax = ucl),
                 
                 position = position_dodge(width = 0.5)) +
  theme_minimal()





filter(trait != "base")

fc_terms$coef[fc_terms$env=="distX1050"] <- fc_terms$coef[fc_terms$env=="distX1050"] + fc_terms$coef[fc_terms$env=="distance_road"]
fc_terms$coef[fc_terms$env=="distX50"] <- fc_terms$coef[fc_terms$env=="distX50"] + fc_terms$coef[fc_terms$env=="distance_road"]


  

  
  
  
  
  
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
