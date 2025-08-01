library(gllvm)
library(tidyverse)
library(tidyr)
library(lattice)
library(GGally)
TMB::openmp(parallel::detectCores()-1, autopar = TRUE, DLL = "gllvm")

# load data
full_data <- read.csv("data/full_dataset.csv", sep=";", dec = ",") 

# filter out duplicates
full_data <- full_data |> filter(Species != "not done yet ",  # unfinished columns
                                 Classification != "L") # remove lichens

full_data$SitePlot <- paste0(full_data$Name, full_data$Plot)

# look for duplicates
as.data.frame(table(full_data$SitePlot, full_data$Species)) # 2 duplicates 

# delete duplicates (NB wait for final confirmation from Lukas)
full_data <- full_data[-c(1401, 2003),]


# make species dataframe 
data_sp <- full_data |> # vascular + bryophytes
  select(Site, Plot, Species, Abundance) |> 
  pivot_wider(names_from = Species, 
              values_from = Abundance,
              id_cols = c(Site, Plot), 
              values_fill = 0)

data_sp_v <- full_data |> # only vascular
  filter(Classification != "B") |> 
  select(Site, Plot, Species, Abundance) |> 
  pivot_wider(names_from = Species, 
              values_from = Abundance,
              id_cols = c(Site, Plot), 
              values_fill = 0)

data_sp_f <- data_sp[,colSums(data_sp > 0) > 6] # filter for species with only 5+ obs
data_sp_v_f <- data_sp_v[,colSums(data_sp_v > 0) > 4] # filter for species with only 5+ obs

## environmental dataframe
data_env <- full_data |> 
  select(Name, Plot, distance_road, dist_t1, 
         Altitude_point, D.altitude, Operation_start,
         Open_water, Bare_rock, Bare_soil) |> 
  distinct()
data_env <- data_env[-90,]  # remove 1 typo row

data_env$dist_group <- ifelse(data_env$distance_road<=10,"<10", "10-50")
data_env$dist_group[data_env$distance_road>50] <- ">50"

# center and scale env data (NB: should I always do this?) 
data_env$distance_road_raw <- data_env$distance_road # for plotting
data_env$distance_road <- data_env$distance_road/10
data_env$distance_road[data_env$dist_group=="10-50"] <- data_env$distance_road[data_env$dist_group=="10-50"] - 1
data_env$distance_road[data_env$dist_group==">50"] <- data_env$distance_road[data_env$dist_group==">50"] - 5


data_env$dist_t1 <- scale(data_env$dist_t1)
data_env$Altitude_point <- scale(data_env$Altitude_point)
data_env$D.altitude <- scale(data_env$D.altitude)
data_env$Bare_rock <- scale(data_env$Bare_rock)


# traits
data_tr <- full_data |> 
  filter(Classification != "B", !(is.na(Rud)), !(is.na(L))) |> 
  select(Species, Rud, Com, Str, L, `F`, R, N, S) |> 
  distinct()
data_env$Bare_soil <- scale(data_env$Bare_soil)
data_tr <- data.frame(lapply(data_tr, function(x)if(is.numeric(x)){scale(x)}else{as.factor(x)}))
data_sp_4th <- data_sp[,data_tr$Species]

save(data_sp, data_sp_f, data_sp_4th, data_env, data_tr, file="windpower_data.Rdata")
