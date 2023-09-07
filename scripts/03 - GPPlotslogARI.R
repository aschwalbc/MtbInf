## Analysis code for Schwalb et al. 2023
## Distributed under CC BY 4.0
## RScript 03: GPPlots.R

# Packages ==========
library(data.table) # Faster than data.frame, allows use of j operator (:=)
library(rio) # Facilitates importing and exporting
library(here) # Building file paths
library(tidyverse) # To use tidyverse
library(ggplot2) # To build comparative plots
library(patchwork) # Plot composition

# 1. Load data ==========
ARIhist <- as.data.table(import(here("data","gp","GP_WHO_norev.Rdata"))) # ARI histories
# ARIrevhist <- as.data.table(import(here("data","gp","GP_IHME_rev.Rdata"))) # Reversion-adjusted ARI histories
ARIrevhist <- as.data.table(import(here("data","gp","GP_PMED.Rdata"))) # PMED estimates

ARI <- as.data.table(import(here("data","ari","ARI_WHO_norev.Rdata"))) # ARI estimates
#ARIrev <- as.data.table(import(here("data","ari","ARI_IHME_rev.Rdata"))) # Reversion-adjusted ARI estimates
ARIrev <- All

# 2. GP plots ==========
countries <- sort(unique(ARIrev$iso3)) # 204 countries

pdf(here("plots","Fitted trends (log ARI).pdf"), height = 6, width = 10) # Produce plot per country

for(i in 1:length(countries)){
  print(countries[i])
  ari <- ARI %>%
    filter(iso3 == countries[i])
  hist <- ARIhist %>%
    filter(iso3 == countries[i])
  p <- ggplot() + # Create plots
    geom_line(hist, mapping = aes(x=year, y=lari), colour = "red") +
    geom_ribbon(hist, mapping = aes(x=year, ymin=lower, ymax=upper), colour = "red", linetype = 2, alpha = 0) +
    geom_point(ari, mapping = aes(x=year, y=logari, colour=type), show.legend = TRUE) +
    scale_x_continuous("year", expand=c(0, 0), breaks = seq(1950, 2050, 25)) +
    scale_y_continuous("log(ARI)", expand=c(0, 0), breaks = seq(-10,0,2.5)) +
    coord_cartesian(ylim = c(-10,0), xlim = c(1950,2050)) +
  labs(title = countries[i], subtitle = "No reversion")

  arirev <- ARIrev %>%
    filter(iso3 == countries[i])
  histrev <- ARIrevhist %>%
    filter(iso3 == countries[i])
  q <- ggplot() + # Create plots
    geom_line(histrev, mapping = aes(x=year, y=lari), colour = "red") +
    geom_ribbon(histrev, mapping = aes(x=year, ymin=lower, ymax=upper), colour = "red", linetype = 2, alpha = 0) +
    geom_point(arirev, mapping = aes(x=year, y=lari, colour=type), show.legend = TRUE) +
    scale_x_continuous("year", expand=c(0, 0), breaks = seq(1950, 2050, 25)) +
    scale_y_continuous("log(ARI)", expand=c(0, 0), breaks = seq(-10,0,2.5)) +
    coord_cartesian(ylim = c(-10,0), xlim = c(1950,2050)) +
  labs(title = countries[i], subtitle = "Reversion-adjusted")

  plot <- (p + q) + plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.title = element_text("source")) 
  print(plot)
}
dev.off()
rm(list = ls())

