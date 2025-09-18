source("Data_setup.R")

# tweak data for gllvm
Y <- data_sp_f[,-c(1,2)]
Y <- Y[,order(colSums(Y>0),decreasing = TRUE)] # order species by most common

# concurrent ordination with 2 LVs
mod_cc_bet <- gllvm(y=Y/20,
                    X=data_env,
                    family = "orderedBeta", # ordbet
                    row.eff = "random",
                    lv.formula = ~ Altitude_point + dist_t1 + Name + 
                      Bare_rock + Bare_soil + dist_group*distance_road,
                    num.lv.c = 2,
                    disp.formula = rep(1, ncol(Y)),
                    n.init =5,
                    trace=T,
                    seed=2,
                    sd.errors = T)

## diagnostics
plot(mod_cc_bet) # ok

## summary
summary(mod_cc_bet, by="terms") # looks fine (no negative probabilities)

## save model
save(mod_cc_bet, file = "data/CLV_model.Rdata")

## standard ordination plot
par(mfrow=c(1,1))
ordiplot(mod_cc_bet, symbols=T, type="conditional", biplot=T, 
         rotate = T, predict.region = F, get.coords = T)

## coefplots
coefplot.gllvm(mod_cc_bet, which.Xcoef = 5)



## make nice ggplot
bet_coords<-ordiplot(mod_cc_bet, symbols=T, type="marginal", biplot=F, 
         rotate = F, predict.region = F, get.coords = T)
ggplot() +
  geom_point(aes(x=bet_coords$sites[,1], 
                 y=bet_coords$sites[,2], 
                 color=data_env$Name, 
                 size=(data_env$distance_road_raw)), alpha=0.4) +
  geom_segment(aes(x=0, y=0, xend=bet_coords$coefs[,1], 
                   yend=bet_coords$coefs[,2]), 
               color="red",
               linewidth=0.4, alpha=0.6,
               arrow=arrow(length = unit(0.2, "cm"))) +
  geom_text(aes(x=bet_coords$coefs[,1]*1.1, y=bet_coords$coefs[,2]*1.1, label=rownames(bet_coords$coefs)), color="red", size=2.5) +
  #stat_ellipse(aes(x=site_scaled[,1], y=site_scaled[,2],
  #               color=data_env$dist_group)) +
  scale_color_brewer(palette="Set2") +
  labs(x="Latent variable 1", y="Latent variable 2", color="Site") +
  theme_minimal()

dist_unscaled <- (data_env$distance_road*35.33401)+39.65478
raw_lvs <- getLV.gllvm(mod_cc_bet, type = "conditional")
raw_coefs <- coef(mod_cc_bet)$Canonical.coefficients[5:9,]
#raw_coefs <- raw_coefs/35.33401
#raw_coefs_int <- 0 - 39.65478*raw_coefs[1,1]
bp1 <- 10
bp2 <- 50

summary(mod_cc_bet)
ggplot() +
  geom_point(aes(x=data_env$distance_road_raw, y=raw_lvs[,1], color=data_env$Name)) +
  geom_vline(xintercept = bp1, linetype=2, color="darkgrey") +
  geom_vline(xintercept = bp2, linetype=2, color="darkgrey") +
  geom_segment(aes(x=min(data_env$distance_road),xend=bp1, 
                   y=min(data_env$distance_road)*(raw_coefs[3,1])/10, 
                   yend = (raw_coefs[3,1]/10)*bp1)) +
  geom_segment(aes(x=bp1, xend=bp2, 
                   y=(raw_coefs[2,1]), 
                   yend = (raw_coefs[2,1]) + (bp2-bp1)*(raw_coefs[3,1])/10 + (bp2-bp1)*(raw_coefs[5,1])/10)) +
  geom_segment(aes(x=bp2, xend=max(data_env$distance_road_raw), 
                   y=raw_coefs[1,1], 
                   yend = raw_coefs[1,1] + (max(data_env$distance_road_raw)-bp2)*(raw_coefs[3,1])/10 + (max(data_env$distance_road_raw)-bp2)*(raw_coefs[4,1])/10)) +
  theme_minimal()



ggplot() +
  geom_point(aes(x=data_env$distance_road_raw, y=raw_lvs[,2], color=data_env$Name)) +
  geom_vline(xintercept = bp1, linetype=2, color="darkgrey") +
  geom_vline(xintercept = bp2, linetype=2, color="darkgrey") +
  geom_segment(aes(x=min(data_env$distance_road),xend=bp1, 
                   y=min(data_env$distance_road)*(raw_coefs[3,2])/10, 
                   yend = (raw_coefs[3,2]/10)*bp1)) +
  geom_segment(aes(x=bp1, xend=bp2, 
                   y=(raw_coefs[2,2]), 
                   yend = (raw_coefs[2,2]) + (bp2-bp1)*(raw_coefs[3,2])/10 + (bp2-bp1)*(raw_coefs[5,2])/10)) +
  geom_segment(aes(x=bp2, xend=max(data_env$distance_road_raw), 
                   y=raw_coefs[1,2], 
                   yend = raw_coefs[1,2] + (max(data_env$distance_road_raw)-bp2)*(raw_coefs[3,2])/10 + (max(data_env$distance_road_raw)-bp2)*(raw_coefs[4,2])/10)) +
  theme_minimal()


  geom_segment(aes(x=bp1,xend=bp2, y=bp1*(raw_coefs[1,1]) + bp1*(raw_coefs[2,1]), yend = bp2*(raw_coefs[1,1]) + bp2*(raw_coefs[2,1]))) +
  +
  theme_minimal()

ggplot() +
  geom_point(aes(x=data_env$distance_road, y=raw_lvs[,2], color=data_env$Name)) +
  geom_vline(xintercept = bp1, linetype=2, color="darkgrey") +
  geom_vline(xintercept = bp2, linetype=2, color="darkgrey")



#raw_lvs <- getLV.gllvm(mod_cc_bet, type = "conditional")
raw_coefs <- coef(mod_cc_bet)$Canonical.coefficients[5:7,]
#raw_coefs <- raw_coefs/35.33401
#raw_coefs_int <- 0 - 39.65478*raw_coefs[1,1]
bp1 <- (10-39.65478)/35.33401
bp2 <- (50-39.65478)/35.33401




a <- as.data.frame(predict(object=mod_cc_bet, type = "response"))

plot(x=data_env$distance_road, y=a$`Andromeda polifolia`)

plot(y=bet_coords$sites[,2], x=data_env$distance_road)


# plot environmental coefs
coefplot.gllvm(mod_cc_bet, which.Xcoef = c("NameSmøla", "NameYtre_vikna"), cex.ylab = 0.5, order=T)
coefplot.gllvm(mod_cc_bet, which.Xcoef = c("distance_road", "dist_t1", "Altitude_point"), cex.ylab = 0.5, order=T)


# try to make levelplot
beta <- mod_cc_bet$params$theta[, 1:(mod_cc_bet$num.lv.c + mod_cc_bet$num.RR), 
                            drop = FALSE] %*% t(mod_cc_bet$params$LvXcoef)
betaSE <- RRse(mod_cc_bet) # needed to copy the RRse func from the repo

a <- 1
colort <- colorRampPalette(c("red", "white", "blue"))
levelplot(t(beta), xlab = "Environmental Variables", 
          ylab = "Species",col.regions = colort(100), cex.lab = 1.3,
          at = seq(-a, a, length = 100), scales = list(x = list(rot = 45)),
          height=1, width=1.4)








