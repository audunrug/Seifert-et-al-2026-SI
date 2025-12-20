library(readxl)
library(mgcv)
library(dplyr)
library(segmented)


# Load and prepare data
WF <- read_excel("Vegetation_data.xlsx")


WF$Operation_start <- as.factor(WF$Operation_start)
WFvasc <- subset(WF, Classification == "V") %>% na.omit()
WFvasc$Abundance <- as.numeric(WFvasc$Abundance)


# Calculate weighted Ruderality
WFvasc_summary <- WFvasc %>%
  group_by(Name, Plot) %>%
  summarise(weighted_ruderality = weighted.mean(Rud, Abundance), .groups = "drop")
WFvasc_final <- left_join(WFvasc, WFvasc_summary, by = c("Name", "Plot"))
WFvasc_final$weighted_ruderality100 <- WFvasc_final$weighted_ruderality / 100


# Keep one entry per plot
WFvasc_final <- WFvasc_final %>%
  distinct(Name, Plot, .keep_all = TRUE)


# Define colors per site for plot
site_names <- unique(WFvasc_final$Name)
colors <- c("black", "red", "blue")
names(colors) <- site_names


# Define shapes per site for plot
shapes <- c(16, 17, 15)  # circle, triangle, square
names(shapes) <- site_names


# Prepare  plot for CWM Ruderality vs distance to road (dist_R)
plot(1, type = "n",
     xlim = c(0,120),
     ylim = c(0,0.5),
     xlab = "Distance to road (m)",
     ylab = "CWM Ruderality"
)


# Loop through sites
for (site in site_names) {
  data_subset <- subset(WFvasc_final, Name == site)
  
  # Add points with site-specific shape
  points(data_subset$dist_R, data_subset$weighted_ruderality100,
         pch = shapes[site],
         col = adjustcolor(colors[site], alpha.f = 0.25))
  
  # Fit segmented model
  lm_mod <- lm(weighted_ruderality100 ~ dist_R, data = data_subset)
  seg_mod <- segmented(lm_mod, seg.Z = ~dist_R, psi = 50, control = seg.control(K = 1))
  
  # Prediction including breakpoint
  bp <- seg_mod$psi[1, "Est."]
  se <- seg_mod$psi[1, "St.Err"]
  ci_lower <- bp - 1.96 * se
  ci_upper <- bp + 1.96 * se
  new_dist <- sort(unique(c(min(data_subset$dist_R), data_subset$dist_R, bp, max(data_subset$dist_R))))
  pred_vals <- predict(seg_mod, newdata = data.frame(dist_R = new_dist))
  
  # Piecewise lines
  lines(new_dist[new_dist <= bp], pred_vals[new_dist <= bp], col = colors[site], lwd = 3)
  lines(new_dist[new_dist >= bp], pred_vals[new_dist >= bp], col = colors[site], lwd = 3)
  
  # Breakpoint and CI
  abline(v = bp, col = "grey", lty = 2, lwd = 2)
  rect(ci_lower, par("usr")[3], ci_upper, par("usr")[4],
       col = adjustcolor(colors[site], alpha.f = 0.1), border = NA)
  
  # Console output
  before_bp <- data_subset$weighted_ruderality100[data_subset$dist_R < bp]
  after_bp <- data_subset$weighted_ruderality100[data_subset$dist_R >= bp]
  
  if (length(before_bp) > 1 && length(after_bp) > 1) {
    t_result <- t.test(before_bp, after_bp)
    cat("Site:", site, "\n")
    cat("  Breakpoint (meters):", round(bp, 1), "m\n")
    cat("  95% CI: [", round(ci_lower, 1), "m ,", round(ci_upper, 1), "m ]\n")
    cat("  Mean before breakpoint:", round(mean(before_bp), 3), "\n")
    cat("  Mean after  breakpoint:", round(mean(after_bp), 3), "\n")
    cat("  t =", round(t_result$statistic, 3), 
        ", df =", round(t_result$parameter, 1), 
        ", p =", signif(t_result$p.value, 4), "\n\n")
  } else {
    cat("Site:", site, "- Not enough data on either side of breakpoint for t-test.\n\n")
  }
}




# Prepare  plot for CWM Ruderality vs distance to turbine (dist_T1)
plot(1, type = "n",
     xlim = c(0,600),
     ylim = c(0,0.5),
     xlab = "Distance to turbine (m)",
     ylab = "CWM Ruderality"
)

# Loop through sites
for (site in site_names) {
  data_subset <- subset(WFvasc_final, Name == site)
  
  # Add points with site-specific shape
  points(data_subset$dist_T1, data_subset$weighted_ruderality100,
         pch = shapes[site],
         col = adjustcolor(colors[site], alpha.f = 0.25))
  
  # Fit segmented model
  lm_mod <- lm(weighted_ruderality100 ~ dist_T1, data = data_subset)
  seg_mod <- segmented(lm_mod, seg.Z = ~dist_T1, psi = 50, control = seg.control(K = 1))
  
  # Prediction including breakpoint
  bp <- seg_mod$psi[1, "Est."]
  se <- seg_mod$psi[1, "St.Err"]
  ci_lower <- bp - 1.96 * se
  ci_upper <- bp + 1.96 * se
  new_dist <- sort(unique(c(min(data_subset$dist_T1), data_subset$dist_T1, bp, max(data_subset$dist_T1))))
  pred_vals <- predict(seg_mod, newdata = data.frame(dist_T1 = new_dist))
  
  # Piecewise lines
  lines(new_dist[new_dist <= bp], pred_vals[new_dist <= bp], col = colors[site], lwd = 3)
  lines(new_dist[new_dist >= bp], pred_vals[new_dist >= bp], col = colors[site], lwd = 3)
  
  # Breakpoint and CI
  abline(v = bp, col = "grey", lty = 2, lwd = 2)
  rect(ci_lower, par("usr")[3], ci_upper, par("usr")[4],
       col = adjustcolor(colors[site], alpha.f = 0.1), border = NA)
  
  # Console output
  before_bp <- data_subset$weighted_ruderality100[data_subset$dist_T1 < bp]
  after_bp <- data_subset$weighted_ruderality100[data_subset$dist_T1 >= bp]
  
  if (length(before_bp) > 1 && length(after_bp) > 1) {
    t_result <- t.test(before_bp, after_bp)
    cat("Site:", site, "\n")
    cat("  Breakpoint (meters):", round(bp, 1), "m\n")
    cat("  95% CI: [", round(ci_lower, 1), "m ,", round(ci_upper, 1), "m ]\n")
    cat("  Mean before breakpoint:", round(mean(before_bp), 3), "\n")
    cat("  Mean after  breakpoint:", round(mean(after_bp), 3), "\n")
    cat("  t =", round(t_result$statistic, 3), 
        ", df =", round(t_result$parameter, 1), 
        ", p =", signif(t_result$p.value, 4), "\n\n")
  } else {
    cat("Site:", site, "- Not enough data on either side of breakpoint for t-test.\n\n")
  }
}





