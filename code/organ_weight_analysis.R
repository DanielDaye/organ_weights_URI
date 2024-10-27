# Organ Weights Analysis for Lemon & Mako Sharks
# Daniel Daye, Colby Kresge, Bradley Wetherbee

# Sourced from: final_script_ddaye_ckresge.R

# Updated 2024-10-27
set.seed(50)

# Setup
library(bbmle)
library(tidyverse)

# Default par()
par_default <- par()


# 1. Load data ----
lemon <- read.csv("data/weight_lemon.csv", header = TRUE) %>%
    transmute(species = "Lemon_shark",
              sex = toupper(sex),
              type = toupper(type),
              length = total_length,
              body = weight_g/1000, 
              heart = heart_g/1000, 
              liver = liver_g/1000, 
              brain = brain_g/1000,
              reproductive = reproductive/1000, 
              stomach = stomach/1000, 
              spleen = spleen_g/1000,
              intestine = intestine/1000,
              status, stat_2)

makos <- read.csv("data/weight_makos.csv", header = TRUE) %>%
  transmute(species = "Mako_shark", sex, length, 
            body = body_weight, 
            heart, liver, stomach, intestine, pancreas, spleen,
            rectal = rectal_gland, 
            ovary, testes,
            epi = epi_organ)

save(lemon, file = "data/lemon.RData")
save(makos, file = "data/makos.RData")


# 2. EDA ----
# See organ_weight_EDA.R
# file.edit("code/organ_weight_EDA.R")


# 3. Dataframe Transformations ----
# Long
long_lemon <- lemon %>%
  gather(org, weight, c(body:intestine))%>%
  drop_na()
long_makos <- makos %>%
  gather(org, weight, c(body:epi)) %>%
  drop_na()

# Normalized
norm_lemon <- lemon %>%
  filter(length>50)%>%
  transmute(length,
            body = 100*body/max(body, na.rm = T),
            liver = 100*liver/max(liver, na.rm = T),
            heart = 100*heart/max(heart, na.rm = T),
            brain = 100*brain/max(brain, na.rm = T),
            stomach = 100*stomach/max(stomach, na.rm = T),
            intestine = 100*intestine/max(intestine, na.rm = T),
            spleen = 100*spleen/max(spleen, na.rm = T))

norm_makos <- makos %>%
  filter(length < 280)%>%
  transmute(length,
            body = 100*body/max(body, na.rm = T),
            liver = 100*liver/max(liver, na.rm = T),
            heart = 100*heart/max(heart, na.rm = T),
            stomach = 100*stomach/max(stomach, na.rm = T),
            intestine = 100*intestine/max(intestine, na.rm = T),
            pancreas = 100*pancreas/max(pancreas, na.rm = T),
            spleen = 100*spleen/max(spleen, na.rm = T),
            rectal = 100*rectal/max(rectal, na.rm = T),
            epi = 100*epi/max(epi, na.rm = T))

norm_long_lemon <- norm_lemon %>%
  gather(org, weight, c(body:intestine))%>%
  drop_na()
norm_long_makos <- norm_makos %>%
  gather(org, weight, c(body:epi)) %>%
  drop_na()


# 4. Bolker Gamma Distribution Parameter Estimation ----
lemon2 <- lemon[,4:5]
ggplot(lemon2, aes(length, body)) +
  geom_point() + 
  coord_cartesian(xlim = c(50, 100), ylim = c(0, 10)) +
  theme_bw()

# Body Weight
gammaNLL2 <- function(shape, scale) {
  val = -1*sum(dgamma(lemon2$body, shape = shape, scale = scale,
              log = T))
  val
}
gm2 = mean(lemon2$body, na.rm = T)
cv2 = var(lemon2$body, na.rm = T)/mean(lemon2$body, na.rm = T)

mle2(gammaNLL2, start= list(shape = gm2/cv2, scale = cv2))

x2 <- rgamma(n = 125, shape = .5901818, scale = 21.9326541)

par(mfrow = c(1,2))
hist(x2, breaks = 30, xlim = c(0,150), main = 'Estimated Gamma Distribution', xlab = 'length')
hist(lemon2$body, breaks = 30, xlim = c(0,150), main = 'Actual Weight Distribution', xlab = 'length')


#5.  FINAL MAKO MODELS ----
table(norm_long_makos$org)

# Power function model
mako_nrm_pow <- mle2(weight ~ dgamma(shape = (a*(length/100)^b)/theta, scale = theta),
                     start = list(a = 10, b = 2.5, theta = 5),
                     data = norm_long_makos,
                     parameters = list(a~org, b~org, theta~org),
                     control = list(maxit = 50000),
                     
)
summary(mako_nrm_pow)
par(mfrow = c(1,2))
plot(residuals(mako_nrm_pow), main = NULL)
hist(residuals(mako_nrm_pow), main = NULL, xlab = 'Residual')


# Linear log-log model
mako_nrm_lin <- mle2(log(weight) ~ dnorm(mean = (a+b*log(length)), sd = k),
                     start = list(a = 10, b = 2.5, k = 2),
                     data = norm_long_makos,
                     parameters = list(a~org, b~org, k~org),
                     control = list(maxit = 50000))
summary(mako_nrm_lin)


## Linear & Power Model Plots ----
# ylim had to be normalized to a 0-100 scale
par(mfrow = c(1,2))
plot(NULL, xlim = c(0,280), ylim = c(0,100),
     xlab = 'TL (cm)', ylab = 'Normalized Weight',
     main = 'Mako Shark Growth Curves')
rain <- rainbow(8)
lines(seq(0,300), 7.82*(seq(0,300)/100)^2.60, col = 'black', lwd = 2) # body
lines(seq(0,300), (7.82+0.70)*(seq(0,300)/100)^(2.60+0.03), col = rain[1], lwd = 2) # heart
lines(seq(0,300), (7.82-2.93)*(seq(0,300)/100)^(2.60+0.23), col = rain[3], lwd = 2) # stomach
lines(seq(0,300), (7.82-0.93)*(seq(0,300)/100)^(2.60-0.24), col = rain[4], lwd = 2) # spleen
lines(seq(0,300), (7.82-2.97)*(seq(0,300)/100)^(2.60+0.24), col = rain[5], lwd = 2) # intestine
lines(seq(0,300), (7.82+2.39)*(seq(0,300)/100)^(2.60-0.36), col = rain[6], lwd = 2) # pancreas
lines(seq(0,300), (7.82+7.14)*(seq(0,300)/100)^(2.60-0.70), col = rain[7], lwd = 2) # rectal
lines(seq(0,300), (7.82+0.52)*(seq(0,300)/100)^(2.60-0.18), col = rain[8], lwd = 2) # epi
lines(seq(0,300), (7.82-3.23)*(seq(0,300)/100)^(2.60+0.55), col = rain[2], lwd = 3) # liver
legend(0, 100,
       legend = c('body','heart','liver','stomach','spleen','intestine','pancreas','rectal','epigonal'),
       lty = 1, lwd = 2, col = c('black',rain[1],rain[2],rain[3],rain[4],rain[5],rain[6],rain[7],rain[8])
)
box(which = 'plot')

plot(NULL, xlim = c(0,1), ylim = c(0,3),
     xlab = 'log(TL)', ylab = 'log(wt)')
abline(coef = c(0,3), lty = 'dotted')
abline(coef = c(0,2.73), lwd = 2, col = 'black') # body
abline(coef = c(0,2.73-.04), lwd = 2, col = rain[1]) # heart
abline(coef = c(0,2.73+.87), lwd = 2, col = rain[2]) # liver
abline(coef = c(0,2.73+.10), lwd = 2, col = rain[3]) # stomach
abline(coef = c(0,2.73-.01), lwd = 2, col = rain[4]) # spleen
abline(coef = c(0,2.73+.10), lwd = 2, col = rain[5]) # intestine
abline(coef = c(0,2.73-.21), lwd = 2, col = rain[6]) # pancreas
abline(coef = c(0,2.73-.63), lwd = 2, col = rain[7]) # rectal
abline(coef = c(0,2.73-.16), lwd = 2, col = rain[8]) # epi
box(which = 'plot')


# 6. FINAL LEMON MODELS ----
table(norm_long_lemon$org)
b <- names(norm_long_lemon)

lemon_nrm_pow <- mle2(weight ~ dgamma(shape = (a*(length/100)^b)/theta, scale = theta),
                      start = list(a = 10, b = 2.5, theta = 5),
                      data = norm_long_lemon,
                      parameters = list(a~org, b~org, theta~org),
                      control = list(maxit = 50000),
                      
)
summary(lemon_nrm_pow)
par(mfrow = c(1,2))
plot(residuals(lemon_nrm_pow), main = NULL)
hist(residuals(lemon_nrm_pow), main = NULL, xlab = 'Residual')


lemon_nrm_lin <- mle2(log(weight) ~ dnorm(mean = (a+b*log(length)), sd = k),
                      start = list(a = 10, b = 2.5, k = 2),
                      data = norm_long_lemon,
                      parameters = list(a~org, b~org, k~org),
                      control = list(maxit = 50000))
summary(lemon_nrm_lin)


# lemon_nrm_all plot
# ylim had to be normalized to a 0-100 scale
plot(NULL, xlim = c(0,280), ylim = c(0,100),
     xlab = 'TL (cm)', ylab = 'Normalized Weight',
     main = 'Lemon Shark Growth Curves')
rain2 <- rainbow(7)
lines(seq(0,300), 3.61*(seq(0,300)/100)^3.07, col = rain2[1], lwd = 2) # body
lines(seq(0,300), (3.61+24.63)*(seq(0,300)/100)^(3.07-1.86), col = rain2[2], lwd = 2) # brain - not an appropriate growth relation
lines(seq(0,300), (3.61+1.41)*(seq(0,300)/100)^(3.07-0.30), col = rain2[3], lwd = 2) # heart
lines(seq(0,300), (3.61+3.43)*(seq(0,300)/100)^(3.07-0.51), col = rain2[4], lwd = 2) # intestine
lines(seq(0,300), (3.61-1.43)*(seq(0,300)/100)^(3.07+0.13), col = rain2[5], lwd = 2) # liver
lines(seq(0,300), (3.61+3.25)*(seq(0,300)/100)^(3.07-0.65), col = rain2[6], lwd = 2) # spleen
lines(seq(0,300), (3.61+2.12)*(seq(0,300)/100)^(3.07-0.34), col = rain2[7], lwd = 2) # stomach
legend(0, 100,
       legend = c('body','brain','heart','intestine','liver','spleen','stomach'),
       lty = 1, col = c(rain2[1],rain2[2],rain2[3],rain2[4],rain2[5],rain2[6],rain2[7])
)
box(which = 'plot')


## Final Lemon Plot ----
par(mfrow = c(1,2))
plot(NULL, xlim = c(0,280), ylim = c(0,100),
     xlab = 'TL (cm)', ylab = 'Normalized Weight',
     main = 'Lemon Shark Growth Curves')
rain <- rainbow(8)
lines(seq(0,300), 3.61*(seq(0,300)/100)^3.07, col = 'black', lwd = 2) # body
lines(seq(0,300), (3.61+1.41)*(seq(0,300)/100)^(3.07-.30), col = rain[1], lwd = 2) # heart
lines(seq(0,300), (3.61+2.12)*(seq(0,300)/100)^(3.07-.34), col = rain[3], lwd = 2) # stomach
lines(seq(0,300), (3.61+3.26)*(seq(0,300)/100)^(3.07-.65), col = rain[4], lwd = 2) # spleen
lines(seq(0,300), (3.61+3.43)*(seq(0,300)/100)^(3.07-.51), col = rain[5], lwd = 2) # intestine
lines(seq(0,300), (3.61-1.43)*(seq(0,300)/100)^(3.07+0.13), col = rain[2], lwd = 3) # liver
legend(0, 100,
       legend = c('body','heart','liver','stomach','spleen','intestine'),
       lty = 1, lwd = 2, col = c('black',rain[1],rain[2],rain[3],rain[4],rain[5])
)
box(which = 'plot')

plot(NULL, xlim = c(0,1), ylim = c(0,3),
     xlab = 'log(TL)', ylab = 'log(wt)')
abline(coef = c(0,3), lty = 'dotted')
abline(coef = c(0,3.01), lwd = 2, col = 'black') # body
abline(coef = c(0,3.01-.20), lwd = 2, col = rain[1]) # heart
abline(coef = c(0,3.01-.02), lwd = 2, col = rain[2]) # liver
abline(coef = c(0,3.01+.07), lwd = 2, col = rain[3]) # stomach
abline(coef = c(0,3.01-.39), lwd = 2, col = rain[4]) # spleen
abline(coef = c(0,3.01+.30), lwd = 2, col = rain[5]) # intestine
box(which = 'plot')


