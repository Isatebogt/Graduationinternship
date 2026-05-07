library(dplyr)
library(ggplot2)

# IL22 data
il22 <- data.frame(
  Microbe = c("Pseudomonas", "Rheinheimera", "Cetobacterium", "Bacteroides",
              "uncultured_f_Barnesiellaceae", "Plesiomonas", "other",
              "Shewanella", "Chitinilyticum", "Aeromonas"),
  RDA1_il22 = c(-1.13688060, -0.46648031, 0.32205922, -0.18929866,
                -0.12681089, -0.09717261, -0.19510452,
                -0.19606554, -0.12553286, -0.01760783)
)

# CXCL8A data (corrected to match what you wrote)
cxcl8a <- data.frame(
  Microbe = c("Cetobacterium", "Pseudomonas", "Escherichia-Shigella",
              "Aeromonas", "Vibrio", "other",
              "uncultured_f_Barnesiellaceae", "Plesiomonas",
              "Bacteroides", "Streptococcus"),
  RDA1_cxcl8a = c(1.12453485, -0.32214649, -0.01480092,
                  0.26276786, 0.57141586, 0.15132776,
                  0.90413167, 0.51122108,
                  0.61279244, -0.15119718)
)

# Merge
merged_df <- full_join(il22, cxcl8a, by = "Microbe")

# Plot
ggplot(merged_df, aes(x = RDA1_cxcl8a, y = RDA1_il22)) +
  geom_point(size = 3) +
  geom_text(aes(label = Microbe), vjust = -1, , size = 3, position = "nudge") +
  theme_minimal() + geom_smooth(method = "lm", se=FALSE, col='blue', size=1)
