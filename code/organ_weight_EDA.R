library(psych)
library(tidyverse)

load("data/lemon.RData")
load("data/makos.RData")


# Lemon ----
# Body
ggplot(lemon, aes(length, body)) +
  geom_point() + 
  labs(title = "Lemon shark", x = "TL (cm)", y = "Body Weight (kg)") +
  theme_bw()
ggsave("figures/EDA/lemon_TL_BW.png")
# Heart
ggplot(lemon, aes(length, heart)) +
  geom_point() + 
  labs(title = "Lemon shark", x = "TL (cm)", y = "Heart Weight (kg)") +
  theme_bw()
ggsave("figures/EDA/lemon_TL_heart.png")
# Liver
ggplot(lemon, aes(length, liver)) +
  geom_point() + 
  labs(title = "Lemon shark", x = "TL (cm)", y = "Liver Weight (kg)") +
  theme_bw()
ggsave("figures/EDA/lemon_TL_liver.png")
# Length Hist
ggplot(lemon, aes(length)) +
  geom_histogram() + 
  labs(title = "Lemon shark", x = "TL", y = "Count") +
  theme_bw()
ggsave("figures/EDA/lemon_TL_hist.png")


# Makos ----
# Body
ggplot(makos, aes(length, body)) +
  geom_point() + 
  labs(title = "Mako shark", x = "TL (cm)", y = "Body Weight (kg)") +
  theme_bw()
ggsave("figures/EDA/makos_TL_BW.png")
# Heart
ggplot(makos, aes(length, heart)) +
  geom_point() + 
  labs(title = "Mako shark", x = "TL (cm)", y = "Heart Weight (kg)") +
  theme_bw()
ggsave("figures/EDA/makos_TL_heart.png")
# Liver
ggplot(makos, aes(length, liver)) +
  geom_point() + 
  labs(title = "Mako shark", x = "TL (cm)", y = "Liver Weight (kg)") +
  theme_bw()
ggsave("figures/EDA/makos_TL_liver.png")
# Length Hist
ggplot(makos, aes(length)) +
  geom_histogram() + 
  labs(title = "Mako shark", x = "TL", y = "Count") +
  theme_bw()
ggsave("figures/EDA/makos_TL_hist.png")


# Combined Histogram
combo <- data.frame(tl = c(lemon$length, makos$length),
                    Species = c(rep("Lemon",nrow(lemon)),
                                rep("Mako", nrow(makos))))
ggplot(combo, aes(tl, fill = Species)) +
  geom_histogram() +
  coord_cartesian(xlim = c(25,300), ylim = c(0, 40)) +
  labs(x = "Total Length (cm)", y = "Count") +
  theme_bw()
ggsave("figures/EDA/combo_hist.png")

# Pairs.Panels
pp.lemon <- lemon %>%
  select(length, body, heart, liver, stomach, spleen, intestine)
pp.makos <- makos %>%
  select(length, body, heart, liver, stomach, spleen, intestine, pancreas, rectal, epi)

pairs.panels(pp.lemon, cex.cor = 1, cex.labels = 1.5)
pairs.panels(pp.makos, cex.cor = 1, cex.labels = 1.5)