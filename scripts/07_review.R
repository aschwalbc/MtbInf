## Analysis code for Mtb Inf Burden
## Authors: A Schwalb 
## RScript 07: Review.R

# Packages ==========
library(rio)
library(here)
library(tidyverse)
library(glue)
library(extrafont)

# 1. SA: Varying self-clearance rates ==========
# 1.1 Self-clearance rates
sc_ref <- import(here("data", "sc", "parms_y20.Rdata")) %>%
  pivot_longer(cols = everything(), names_to = "var", values_to = "val") %>%
  group_by(var) %>%
  summarise(val = median(val, na.rm = TRUE), .groups = "drop") %>%
  mutate(type = "ref")

sc_sa <- sc_ref %>%
  select(-type) %>%
  crossing(type = c("lo25", "lo50", "lo75", "hi25", "hi50", "hi75")) %>%
  mutate(scale = as.numeric(str_extract(type, "\\d+")) / 100,
         val = if_else(str_detect(type, "lo"), val * (1 - scale), val * (1 + scale))) %>%
  select(var, val, type)

sc <- bind_rows(sc_sa, sc_ref) %>%
  mutate(type = factor(type, levels = c("lo75", "lo50", "lo25", "ref", "hi25", "hi50", "hi75"))) %>%
  arrange(type) %>%
  pivot_wider(names_from = var, values_from = val) %>%
  relocate(type)
rm(sc_ref, sc_sa)
export(sc, here("data", "sc", "varparms_y20.Rdata"))

# 1.2 Model set-up
sc_model <- function(parms, end_time = 50) {
  
  des <- function(time, state, parms) {
    S <- state["S"]
    Ia <- state["Ia"]
    Ib <- state["Ib"]
    Ic <- state["Ic"]
    Id <- state["Id"]
    
    gamma_a <- parms[['gamma_a']]
    gamma_b <- parms[['gamma_b']]
    gamma_c <- parms[['gamma_c']]
    gamma_d <- parms[['gamma_d']]
    kappa_ab <- 1 # Transition Y1 -> Y2
    kappa_bc <- 1 # Transition Y2 -> Y3-9
    kappa_cd <- 1/8 # Transition Y3-9 -> Y10+
    
    dS <- (gamma_a * Ia) + (gamma_b * Ib) + (gamma_c * Ic) + (gamma_d * Id)
    dIa <- - (gamma_a * Ia) - (kappa_ab * Ia)
    dIb <- - (gamma_b * Ib) + (kappa_ab * Ia) - (kappa_bc * Ib)
    dIc <- - (gamma_c * Ic) + (kappa_bc * Ib) - (kappa_cd * Ic)
    dId <- - (gamma_d * Id) + (kappa_cd * Ic)
    
    return(list(c(dS, dIa, dIb, dIc, dId), pSC = S/1e5))
  }
  
  state <- c(S = 0, Ia = 1e5, Ib = 0, Ic = 0, Id = 0)
  times <- seq(from = 0, to = end_time, by = 1)
  out <- deSolve::ode(y = state, times = times, func = des, parms = parms)
  
  df_out <- as.data.frame(out)
  
  if("pSC.S" %in% names(df_out)) {
    names(df_out)[names(df_out) == 'pSC.S'] <- 'pSC'
  }
  
  return(df_out)
}

# 1.3  Model loops
loop <- list()

for(i in 1:nrow(sc)) {
  parms <- sc[i,]
  
  out <- as.data.frame(sc_model(parms))
  out$type <- parms$type
  loop[[i]] <- out
}

run <- do.call("rbind", loop)
rm(sc_model, loop, out, parms, i)

# 1.4 Plot
ggplot(run) +
  geom_line(aes(x = time, y = pSC, colour = type)) +
  scale_colour_manual(values = c("lo75"  = "#08306B", "lo50"  = "#2171B5", "lo25"  = "#6BAED6",
                                 "ref"   = "#4D4D4D", "hi25"  = "#FDBBA1", "hi50"  = "#FC9272", "hi75"  = "#DE2D26"),
                      labels = c("lo75" = "-75%", "lo50" = "-50%", "lo25" = "-25%", "ref"  = "Reference",
                                 "hi25" = "+25%", "hi50" = "+50%", "hi75" = "+75%")) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.60, 1), xlim = c(0, 51)) +
  labs(x = 'Years', y = 'Percentage self-cleared or recovered', colour = 'Self-clearance rates') +
  theme_bw() +
  theme(text = element_text(family = "Open Sans"))

# 1.5 Results
files <- list.files(here("outputs", "sa_sc"), pattern = "\\.Rdata$", full.names = TRUE)
loop <- list()

for(file in files) {
  df <- import(file)
  name <- tools::file_path_sans_ext(basename(file))
  
  num <- df %>% 
    filter(year == 2022) %>% 
    filter(var %in% c('It', 'rIt')) %>% 
    mutate(across(c(val, lo, hi), ~ round(. / 1e6, 1))) %>%
    mutate(across(c(val, lo, hi), ~ sprintf("%.1f", .))) %>%
    mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
    select(var, est) %>% 
    pivot_wider(names_from = var, values_from = est) %>% 
    select(rIt, It)
  
  pct <- df %>% 
    filter(year == 2022) %>% 
    filter(var %in% c('pI', 'prI')) %>% 
    mutate(across(c(val, lo, hi), ~ round(. * 100, 1))) %>%
    mutate(across(c(val, lo, hi), ~ sprintf("%.1f", .))) %>%
    mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
    select(var, est) %>% 
    pivot_wider(names_from = var, values_from = est) %>% 
    select(prI, pI) 

  res <- cbind(num, pct) %>% 
    mutate(type = name) %>% 
    select(type, rIt, prI, It, pI) %>% 
    rename('Recent infections' = rIt, 'Recent infection prevalence' = prI,
           'All infections' = It, 'All infection prevalence' = pI)
  
  loop[[name]] <- res
}

sc_var <- bind_rows(loop) %>% 
  mutate(type = factor(type, levels = c("lo75", "lo50", "lo25", "ref", "hi25", "hi50", "hi75"))) %>% 
  arrange(type)

# 1.6 Plot
files <- list.files(here("outputs", "sa_sc"), pattern = "\\.Rdata$", full.names = TRUE)
loop <- list()

for(file in files) {
  df <- import(file)
  name <- tools::file_path_sans_ext(basename(file))
  
  num <- df %>% 
    filter(year == 2022) %>% 
    filter(var %in% c('It', 'rIt')) %>% 
    select(-year)
  
  pct <- df %>% 
    filter(year == 2022) %>% 
    filter(var %in% c('pI', 'prI')) %>% 
    select(-year)

  res <- rbind(num, pct) %>% 
    mutate(type = name) %>% 
    select(type, var, val, lo, hi)
  
  loop[[name]] <- res
}

sc_var_plot <- bind_rows(loop) %>%
  mutate(type = factor(type, levels = c("lo75", "lo50", "lo25", "ref", "hi25", "hi50", "hi75"))) %>% 
  arrange(type)

png(here("plots", paste0("07_mtbinf_sc_var.png")), width = 9, height = 5, units = 'in', res = 1000)
ggplot(filter(sc_var_plot, var %in% c("rIt", "It"))) +
  geom_col(aes(x = type, y = val, fill = var), position = "identity") +
  geom_errorbar(aes(x = type, ymin = lo, ymax = hi, group = var), 
                position = position_dodge(width = 0.9), width = 0.50, colour = "#242424") +
  scale_fill_manual(values = c("It" = "#900C3F", "rIt" = "#FF5733"),
                    labels = c("It" = "Distal infections", "rIt" = "Recent infections")) +
  scale_x_discrete(labels = c("lo75" = "-75%", "lo50" = "-50%", "lo25" = "-25%", 
                              "ref" = "Reference", "hi25" = "+25%", "hi50" = "+50%", "hi75" = "+75%")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = 'M', big.mark = ',')) +
  labs(x = 'Self-clearance rate variation', y = expression('Number of viable '*italic('Mtb')*' infections'), fill = 'Type') +
  theme_bw() +
  theme(legend.position = 'bottom',
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        text = element_text(family = "Open Sans"))
dev.off()  

rm(list = setdiff(ls(), c("sc_var", "sc_var_plot")))

# 2. SA: Exploring correlations in self-clearance ==========
# 2.1 Sampling self-clearance rates
fits <- import(here("data", "sc", "parms_y20.Rdata"))

set.seed(07032021)
corr_fits <- slice_sample(fits, n = 1000)
export(corr_fits, here("data", "sc", "corrparms_y20.Rdata"))

# 2.2 Grouping fits and samples
df <- bind_rows(fits %>% mutate(source = "full"),
                corr_fits %>% mutate(source = "sample")) %>%
  pivot_longer(cols = -source, names_to = "var", values_to = "val")

# 2.3 Plots
ggplot(df, aes(x = val, fill = source)) +
  facet_wrap(~ var, scales = "free") +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30, colour = "#000000") +
  scale_fill_manual(values = c("full" = "#D3D3D3", "sample" = "#FF5733"),
                    labels = c("full" = "Full set", "sample" = "Sampled set")) +
  labs(x = NULL, y = "Count", fill = NULL) +
  theme_bw() +
    theme(legend.position = "bottom", legend.direction = "horizontal",
      text = element_text(family = "Open Sans"))

# 2.4 Results
MTBglb <- import(here("outputs", "sa_corr", "MTBglb.Rdata"))

num <- MTBglb %>% 
  filter(year == 2022) %>% 
  filter(var %in% c('It', 'rIt')) %>% 
  mutate(across(c(val, lo, hi), ~ round(. / 1e6, 1))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.1f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
  select(var, est) %>% 
  pivot_wider(names_from = var, values_from = est) %>% 
  select(rIt, It) %>% 
  rename('Recent infections' = rIt, 'All infections' = It)

pct <- MTBglb %>% 
  filter(year == 2022) %>% 
  filter(var %in% c('pI', 'prI')) %>% 
  mutate(across(c(val, lo, hi), ~ round(. * 100, 1))) %>%
  mutate(across(c(val, lo, hi), ~ sprintf("%.1f", .))) %>%
  mutate(est = paste0(val, " (", lo, "-", hi, ")")) %>% 
  select(var, est) %>% 
  pivot_wider(names_from = var, values_from = est) %>% 
  select(prI, pI) %>% 
  rename('Recent infection prevalence' = prI, 'All infection prevalence' = pI)

# 3. Comparing ARI sources ==========
ARI <- import(here("data", "ari", "ARI_norev.Rdata")) %>% 
  mutate(sd = E/1.96) %>% 
  mutate(lower = lari - sd, upper = lari + sd) %>% 
  mutate(lo = exp(lower), hi = exp(upper)) %>% 
  select(iso3, year, ari, lo, hi, type)

ARIsurv <- ARI %>% 
  filter(type == "surv", year >= 2000)

ARIprev <- ARI %>% 
  filter(type == "prev") %>% 
  semi_join(ARIsurv, by = c("iso3", "year"))

ARIcomp <- bind_rows(ARIsurv, ARIprev) %>%
  mutate(val = glue("{sprintf('%.2f', ari * 1e2)}
                    (95%CI: {sprintf('%.2f', lo * 1e2)}–{sprintf('%.2f', hi * 1e2)})") 
         %>% as.character()) %>% 
  group_by(iso3, year, type) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  select(iso3, year, row, type, val) %>%
  pivot_wider(names_from = type, values_from = val) %>%
  group_by(iso3, year) %>%
  fill(surv, prev, .direction = "downup") %>%
  ungroup() %>%
  select(-row)

rm(list = setdiff(ls(), "ARIcomp"))

ARIchecks <- ARIcomp %>% 
  mutate(lo_surv = as.numeric(sub(".*\\(95%CI: ([0-9.]+)–[0-9.]+\\).*", "\\1", surv)),
         hi_surv = as.numeric(sub(".*–([0-9.]+)\\).*", "\\1", surv)),
         lo_prev = as.numeric(sub(".*\\(95%CI: ([0-9.]+)–[0-9.]+\\).*", "\\1", prev)),
         hi_prev = as.numeric(sub(".*–([0-9.]+)\\).*", "\\1", prev)),
         ci_overlap = lo_surv <= hi_prev & hi_surv >= lo_prev)

