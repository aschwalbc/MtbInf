# Diagnostic accuracy analysis
# Packages ==========
library(rio)
library(here)
library(tidyverse)

# 1. Constraints
ppv_target <- 0.08
npv_target <- 0.98

# PPV = TP / (TP + FP)
# PPV * (TP + FP) = TP
# PPV * TP + PPV * FP = TP
# PPV * FP = TP - PPV * TP
# PPV * FP = TP * (1 - PPV)
# FP = TP * (1 - PPV) / PPV
# TP / FP = PPV / (1 - PPV)
ppv_ratio <- ppv_target / (1 - ppv_target)  # TP/FP ratio

# NPV = TN / (TN + FN)
# NPV * (TN + FN) = TN
# NPV * TN + NPV * FN = TN
# NPV * FN = TN - NPV * TN
# NPV * FN = TN * (1 - NPV)
# NPV * FN = TN * (1 - NPV)
# TN / FN = NPV / (1 - NPV)
npv_ratio <- npv_target / (1 - npv_target)  # TN/FN ratio

# 2. Constant for constraint
const <- ppv_ratio * npv_ratio

# Diagnostic accuracy definitions:
# Sensitivity = TP / (TP + FN)
# Specificity = TN / (TN + FP)
# PPV = TP / (TP + FP)
# NPV = TN / (TN + FN)

# FP = TP / ppv_ratio  (derived from PPV constraint)
# FN = TP * (1 - sens) / sens  (derived from sensitivity definition)
# TN = FN * npv_ratio  (derived from NPV constraint)

# Substituting into specificity = TN / (TN + FP):
# spec = (FN * npv_ratio) / (FN * npv_ratio + TP / ppv_ratio)

# Divide numerator and denominator by TP:
# spec = ((FN/TP) * npv_ratio) / ((FN/TP) * npv_ratio + 1/ppv_ratio)

# Substitute FN/TP = (1 - sens) / sens:
# spec = (((1-sens)/sens) * npv_ratio) / (((1-sens)/sens) * npv_ratio + 1/ppv_ratio)

# Multiply numerator and denominator by ppv_ratio:
# spec = (const * (1 - sens) / sens) / (const * (1 - sens) / sens + 1)

# Simplify:
# spec = (const * (1 - sens)) / (const * (1 - sens) + sens)
# spec = (const * (1 - sens)) / (const - const*sens + sens)
# spec = (const * (1 - sens)) / (const - (const - 1)*sens)

# 3. Varying sensitivity
sens_range <- seq(0.00, 1.00, by = 0.001)
res <- data.frame(
  sens = numeric(),
  spec = numeric(),
  prev = numeric(),
  TP = numeric(),
  FP = numeric(),
  FN = numeric(),
  TN = numeric()
)

for (sens in sens_range) {
  # Required specificity to maintain PPV and NPV
  # Formula: spec = const * (1 - sens) / (const - (const - 1) * sens)
  denom <- const - (const - 1) * sens

  if (denom <= 0) next

  spec <- const * (1 - sens) / denom
  # Check if specificity is valid (between 0 and 1)
  if (spec >= 0 && spec <= 1) {
    # Arbitrarily set TP = 1000 to calculate the rest
    TP <- 1000
    FP <- TP / ppv_ratio
    FN <- TP * (1 - sens) / sens
    TN <- FN / npv_ratio

    total <- TP + FP + FN + TN
    prev <- (TP + FN) / total

    res <- rbind(res, data.frame(
      sens = sens,
      spec = spec,
      prev = prev,
      TP = TP,
      FP = FP,
      FN = FN,
      TN = TN
    ))
  }
}

res <- na.omit(res)

cat(sprintf("Found %d valid sensitivity/specificity combinations\n", nrow(res)))
if (nrow(res) > 0) {
  cat(sprintf("Prevalence ranges from %.2f%% to %.2f%%\n",
              min(res$prev) * 100, max(res$prev) * 100))
  cat(sprintf("Sensitivity ranges from %.1f%% to %.1f%%\n",
              min(res$sens) * 100, max(res$sens) * 100))
  cat(sprintf("Specificity ranges from %.1f%% to %.1f%%\n",
              min(res$spec) * 100, max(res$spec) * 100))
}

png(here("plots", paste0("sens_spec_roc.png")), width = 7, height = 4, units = "in", res = 1000)
ggplot(res) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#808080") +
  geom_line(aes(x = 1 - spec, y = sens, color = cut(prev, breaks = c(-Inf, 0.05, 0.1, 0.15, 0.2, 0.3, 0.5, Inf), labels = c("0-5%", "5-10%", "10-15%", "15-20%", "20-30%", "30-50%", "50%+"))), size = 2) +
  scale_color_manual(values = c("0-5%" = "#ef745c", "5-10%" = "#d06257", "10-15%" = "#b15052", "15-20%" = "#923e4d", "20-30%" = "#722b47", "30-50%" = "#531942", "50%+" = "#34073d"),
                     labels = c("0-5%" = "0-5%", "5-10%" = "5-10%", "10-15%" = "10-15%", "15-20%" = "15-20%", "20-30%" = "20-30%", "30-50%" = "30-50%", "50%+" = "50%+")) +
  labs(
    title = sprintf("ROC curve under PPV/NPV constraints\n(PPV=%.0f%%, NPV=%.0f%%)",
                    ppv_target * 100, npv_target * 100),
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)",
    color = "IGRA\nPrevalence"
  ) +
  coord_equal() +
  theme_bw() +
  theme(text = element_text(family = "Open Sans", size = 10))
dev.off()

rm(list = ls())
