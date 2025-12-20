## ternary diagram
library(readxl)
library(dplyr)
library(ggtern)


## Load and inspect data
WF_raw <- read_excel("Vegetation_data.xlsx")
WF <- subset(WF_raw, Classification == "V") %>% na.omit()


## Convert  CSR values to proportional scale 
WF <- WF %>%
  mutate(
    Com = Com / 100,  
    Str = Str / 100,  
    Rud = Rud / 100   
  )


## Assign plots to zones
WF <- WF %>%
  mutate(Zone = case_when(
    Plot >= 1  & Plot <= 30 ~ "zone 1",
    Plot >= 31 & Plot <= 60 ~ "zone 2",
    Plot >= 61 & Plot <= 90 ~ "zone 3",
    TRUE ~ NA_character_
  ))


## Calculate community-weighted CSR per plot
CSR_weighted_plot <- WF %>%
  group_by(Site,Name,Plot,Zone) %>%
  summarise(
    C = weighted.mean(Com, Abundance, na.rm = TRUE),
    S = weighted.mean(Str, Abundance, na.rm = TRUE),
    R = weighted.mean(Rud, Abundance, na.rm = TRUE),
    .groups = "drop"
  )


# Fix factor order for Zone and site
CSR_weighted_plot <- CSR_weighted_plot %>%
  mutate(
    Zone = factor(Zone,
                         levels = c("zone 3", "zone 2", "zone 1")),
    Site = factor(Site,
                  levels = c("1", "2", "3"),
                  labels = c("F4", "Y12", "S19"))
  )


ggtern() +
  
  # Back layer zone 3
geom_point(
  data = CSR_weighted_plot %>% filter(Zone == "zone 3"),
  aes(x = R, y = C, z = S,
      color = Zone,
      shape = Site),
  size = 5, alpha = 1
) +
  
  # Middle layer zone 2
geom_point(
  data = CSR_weighted_plot %>% filter(Zone == "zone 2"),
  aes(x = R, y = C, z = S,
      color = Zone,
      shape = Site),
  size = 5, alpha = 1
) +
  
  # Front layer zone 1
geom_point(
  data = CSR_weighted_plot %>% filter(Zone == "zone 1"),
  aes(x = R, y = C, z = S,
      color = Zone,
      shape = Site),
  size = 5, alpha = 1
) +
  theme_bw() +
  labs(
    x = "R",
    y = "C",
    z = "S",
    color = "Zone",
    shape = "Site"
  ) +
  scale_shape_manual(values = c(
    "F4"  = 16,  # circle
    "Y12" = 17,  # triangle
    "S19" = 15   # square
  )) +
  scale_color_manual(values = c(
    "zone 1" = "#2c728eff",
    "zone 2" = "#75d054ff",
    "zone 3" = "#FDe735ff"
  )) +
  scale_T_continuous(limits = c(0,1), breaks = seq(0,1,0.2),
                     labels = c("", "0.8", "0.6", "0.4", "0.2", "")) +
  scale_L_continuous(limits = c(0,1), breaks = seq(0,1,0.2),
                     labels = c("", "0.8", "0.6", "0.4", "0.2", "")) +
  scale_R_continuous(limits = c(0,1), breaks = seq(0,1,0.2),
                     labels = c("", "0.8", "0.6", "0.4", "0.2", "")) +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold")
  ) +
  facet_wrap(~ Site)



