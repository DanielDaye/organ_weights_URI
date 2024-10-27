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
# Heart
ggplot(lemon, aes(length, heart)) +
  geom_point() + 
  labs(title = "Lemon shark", x = "TL (cm)", y = "Heart Weight (kg)") +
  theme_bw()
# Liver
ggplot(lemon, aes(length, liver)) +
  geom_point() + 
  labs(title = "Lemon shark", x = "TL (cm)", y = "Liver Weight (kg)") +
  theme_bw()
# Length Hist
ggplot(lemon, aes(length)) +
  geom_histogram() + 
  labs(title = "Lemon shark", x = "TL")


# Makos ----
# Body
ggplot(makos, aes(length, body)) +
  geom_point() + 
  labs(title = "Mako shark", x = "TL (cm)", y = "Body Weight (kg)") +
  theme_bw()
# Heart
ggplot(makos, aes(length, heart)) +
  geom_point() + 
  labs(title = "Mako shark", x = "TL (cm)", y = "Heart Weight (kg)") +
  theme_bw()
# Liver
ggplot(makos, aes(length, liver)) +
  geom_point() + 
  labs(title = "Mako shark", x = "TL (cm)", y = "Liver Weight (kg)") +
  theme_bw()
# Length Hist
ggplot(makos, aes(length)) +
  geom_histogram() + 
  labs(title = "Mako shark", x = "TL")


# Pairs.Panels
pp.lemon <- lemon %>%
  select(length, body, heart, liver, stomach, spleen, intestine)
pp.makos <- makos %>%
  select(length, body, heart, liver, stomach, spleen, intestine, pancreas, rectal, epi)

pairs.panels(pp.lemon, cex.cor = 1, cex.labels = 1.5)
pairs.panels(pp.makos, cex.cor = 1, cex.labels = 1.5)