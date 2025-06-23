# Load packages -----------
library(readxl)
library(nlme)
library(ggplot2)
library(tidyverse)
library(emmeans)
library(ggpubr)


options(scipen = 99)
set.seed(999)

# Load data ---------------------------------

dataf <- read_excel("data/base_vocabulario.xlsx", sheet = "datos")

# Descriptive statistics --------------------
dataf |> 
  pivot_longer(cols = c(edad,VE:V),
               names_to = "Vocabulary",
               values_to = "Score") |> 
  group_by(Vocabulary) |> 
  summarise(M = mean(Score),
            sd = sd(Score),
            min = min(Score),
            max = max(Score)) 

dataf |> 
  pivot_longer(cols = c(edad, VE:V),
               names_to = "Vocabulary",
               values_to = "Score") |> 
  group_by(grupo_edad, Vocabulary) |> 
  summarise(M = mean(Score),
            sd = sd(Score),
            min = min(Score),
            max = max(Score)) 

dataf |> 
  janitor::tabyl(genero)

# EV and GV items with same level of difficulty in participants from 30 to 35 years of age------ 

# Pivot table from wide to long format
dataf_long <- dataf |>
  pivot_longer(
    cols = starts_with("v_") | starts_with("ve_"),
    names_to = "item",
    values_to = "score"
  ) 



faciles <- dataf_long |> 
  filter(between(edad, 30, 35)) |> 
  filter(score == 1) |>
  count(item, score) |> 
  filter(n >= 24) |> # 75% respuestas correctas
  pull(item)


vg <- str_detect(faciles, pattern = "v_")
vg <- faciles[vg]
vg

ve <- str_detect(faciles, pattern = "ve_")
ve <- faciles[ve]
ve

faciles_vg <- dataf_long |> 
  filter(between(edad, 30, 35)) |> 
  filter(item %in% vg) |> 
  filter(score == 1) |> 
  count(item) 

faciles_vg|>arrange(desc(n))

dataf_long |> 
  filter(between(edad, 30, 35)) |> 
  filter(item %in% ve) |> 
  filter(score == 1) |> 
  count(item) 


faciles_ve <- vector(mode = "character", length = nrow(faciles_vg))

for (i in seq_along(faciles_vg$n)) {

  ve_item <- dataf_long |> 
    filter(between(edad, 30, 35)) |> 
    filter(item %in% ve) |> 
    filter(score == 1) |> 
    count(item) |>
    filter(n == faciles_vg$n[i])
  
  faciles_ve[i] <- sample(ve_item$item, size = 1)
  
    
}
  

faciles_ve

# Calculate mean and SEM
summary_stats <- dataf_long |>
  group_by(item) |>
  summarise(
    mean = mean(score, na.rm = TRUE),
    sem = sd(score, na.rm = TRUE) / sqrt(sum(!is.na(score))),
    .groups = "drop"
  ) |>
  mutate(type = if_else(startsWith(item, "v_"), "General", "Emotional"))

summary_stats


# Analize differences in selected items

dataf_paired <- dataf |>
  filter(between(edad, 30, 35))  |>
  select(all_of(faciles_vg$item), all_of(faciles_ve)) |>
  rowwise() |>
  mutate(
    V = sum(c_across(starts_with("v_")), na.rm = TRUE),
    Ve = sum(c_across(starts_with("ve_")), na.rm = TRUE)
  ) |>
  ungroup()


t.test(dataf_paired$Ve, dataf_paired$V)


# Retrain selected items ------------
# Define the specific items to sum

# Compute the row-wise sums, keeping all columns
dataf <- dataf |>
  rowwise() |>
  mutate(
    V = sum(c_across(all_of(faciles_vg$item)), na.rm = TRUE),
    VE = sum(c_across(all_of(faciles_ve)), na.rm = TRUE)
  ) |>
  ungroup()

dataf |> 
  pivot_longer(cols = c(V, VE, edad),
               names_to = "variable",
               values_to = "valores") |> 
  group_by(variable) |> 
  summarise(M = mean(valores, na.rm = TRUE),
          sd = sd(valores, na.rm = TRUE),
          Min = min(valores, na.rm = TRUE),
          Max = max(valores, na.rm = TRUE)) |> view()


# Define Gompertz functions -----------------
gompertz <- function(t, A, B, C) {
  A * exp(-exp(-B * (t - C)))
}

gompertz2 <- function(t, cat, B, C, coef_B, coef_C) {
   exp(-exp(-(B + cat * coef_B) * (t - (C + cat * coef_C))))
 }

Gomp_second_der <- function(x, a, k, t) {
  (k * exp(-k * (x - t)) - k) * a * k * exp(-exp(-k * (x - t)) - k * (x - t))
}

# Fit non linear Least Squares Gompertz models separately ------------

# Model fit
fit_v <- nls(V ~ gompertz(edad, A, B, C), data = dataf, start = list(A = 14, B = 0.09, C = 10))
fit_ve <- nls(VE ~ gompertz(edad, A, B, C), data = dataf, start = list(A = 14, B = 0.09, C = 10))

summary(fit_v)
summary(fit_ve)


# Analize residuals

res_V <- resid(fit_v)
res_Ve <- resid(fit_ve)

plot(res_V)
plot(res_Ve)

pred_V <- fitted(fit_v)
pred_Ve <- fitted(fit_ve)

plot(pred_V, res_V)
plot(pred_Ve, res_Ve)

hist(res_V)
hist(res_Ve)


# Compute Pseudo-R2 
SS_residual_v <- sum(residuals(fit_v)^2)
SS_total_v <- sum((dataf$V - mean(dataf$V))^2)

pseudo_R2_v <- 1 - (SS_residual_v / SS_total_v)
print(pseudo_R2_v)


SS_residual_ve <- sum(residuals(fit_ve)^2)
SS_total_ve <- sum((dataf$VE - mean(dataf$VE))^2)

pseudo_R2_ve <- 1 - (SS_residual_ve / SS_total_ve)
print(pseudo_R2_ve)


# Plot vocabulary scores distribution against fitted growth curve

extrapolation_range <- seq(from = 6, to = 90, by = 1)

p1 <- ggplot() +
  geom_point(data = dataf, aes(x = edad, y = V), 
             color = "blue", size = .8) +
  geom_line(aes(x = extrapolation_range, 
                y = gompertz(extrapolation_range, 
                             coef(fit_v)[1], 
                             coef(fit_v)[2], 
                             coef(fit_v)[3])), 
            color = "blue", linewidth = 1) +
  labs(x = "Age (years)", y = "General Vocabulary") +
  theme_light()

p2 <- ggplot() +
  geom_point(data = dataf, aes(x = edad, y = VE), 
             color = "red", size = .8) +
  geom_line(aes(x = extrapolation_range, 
                y = gompertz(extrapolation_range, 
                             coef(fit_ve)[1], 
                             coef(fit_ve)[2], 
                             coef(fit_ve)[3])), 
            color = "red", linewidth = 1) +
  labs(x = "Age (years)", y = "Emotional vocabulary") +
  theme_light()

figure1 <- gridExtra::grid.arrange(p1, p2, ncol = 1)

ggsave("plots/Figure1.png", figure1, width = 5, height = 5, dpi = 1200)

# plot second derivative
p3 <- ggplot() +
  geom_line(aes(x = extrapolation_range, 
                y = Gomp_second_der(extrapolation_range, 
                                    coef(fit_v)[1], 
                                    coef(fit_v)[2], 
                                    coef(fit_v)[3])), 
            color = "blue", linewidth = 1) +
  labs(x = "Age (years)", y = "") +
  theme_light()


p4 <- ggplot() +
  geom_line(aes(x = extrapolation_range, 
                y = Gomp_second_der(extrapolation_range, 
                                    coef(fit_ve)[1], 
                                    coef(fit_ve)[2], 
                                    coef(fit_ve)[3])), 
            color = "red", linewidth = 1) +
  labs(x = "Age (years)", y = "") +
  theme_light()

figure2 <- gridExtra::grid.arrange(p3, p4, ncol = 1)

ggsave("plots/Figure2.png", figure2, width = 5, height = 5, dpi = 1200)

# Plot normalized curves for GV and EV
ggplot() +
  geom_line(aes(x = extrapolation_range, 
                y = (gompertz(extrapolation_range, 
                              coef(fit_v)[1], 
                              coef(fit_v)[2], 
                              coef(fit_v)[3])) / coef(fit_v)[1]), 
            color = "blue", linewidth = 1) +
  geom_line(aes(x = extrapolation_range, 
                y = (gompertz(extrapolation_range, 
                              coef(fit_ve)[1], 
                              coef(fit_ve)[2], 
                              coef(fit_ve)[3])) / coef(fit_ve)[1]), 
            color = "red", linewidth = 1) +
  labs(x = "Age (years)", y = "Normalized Value") +
  ggtitle("Normalized Curve Fit for GV and VE") +
  theme_light() +
  annotate("text", x = 34, 
           y = 0.75, 
           label = "GV", color = "blue", hjust = 1) +
  annotate("text", x = 10, 
           y = 0.75, 
           label = "VE", color = "red", hjust = 1)

ggsave("plots/Figure3.png", width = 5, height = 2.5, dpi = 1200)

# Find min time for second derivative for GV
posicion_min_segunda_der_v <- which.min(Gomp_second_der(extrapolation_range, coef(fit_v)[1], coef(fit_v)[2], coef(fit_v)[3]))
tiempo_min_segunda_der_v <- extrapolation_range[posicion_min_segunda_der_v]
print(tiempo_min_segunda_der_v)

# Find min time for second derivative for EV
posicion_min_segunda_der_ve <- which.min(Gomp_second_der(extrapolation_range, coef(fit_ve)[1], coef(fit_ve)[2], coef(fit_ve)[3]))
tiempo_min_segunda_der_ve <- extrapolation_range[posicion_min_segunda_der_ve]
print(tiempo_min_segunda_der_ve)



# Prepare data for group comparisons --------------

# Compute normalized variables 
V_norm <- dataf$V / coef(fit_v)[1]
VE_norm <- dataf$VE / coef(fit_ve)[1]

# Create the combined dataset
nuevo_df <- tibble(
  voc_norm = c(V_norm, VE_norm),
  dummy = rep(c(1, 0), each = nrow(dataf)),
  age = rep(dataf$edad, 2),
  ID = rep(dataf$ID, 2),
  age_group = rep(dataf$grupo_edad, 2)
)

# Calculate difference between normalized VE and GV
nuevo_df_dif <- tibble(
  voc_dif = VE_norm - V_norm,
  age = dataf$edad
)

# Plot difference between normalized VE and GV ---------

nuevo_df_dif |> 
  ggplot(aes(x = age, y = voc_dif)) +
  geom_point(color = "blue", alpha = 0.5) +  
  geom_smooth(method = "loess", color = "red") +  
  labs(title = "",
       x = "Age (years)",
       y = "EV(norm)-GV(norm)") +
  theme_light()



# Fit group comparison gompertz model ---------

# Fit model
group_dif <- nls(voc_norm ~ gompertz2(age, dummy, B, C, coef_B, coef_C), 
                 data = nuevo_df, 
                 start = list(B = 0.16, C = 6.25, coef_B = 0, coef_C = 0),
                 control = nls.control(maxiter = 1000, tol = 1e-05, minFactor = 1e-06))

summary(group_dif)

# Analize residuals

res_dif <- residuals(group_dif)
pred_dif <- fitted(group_dif)

plot(pred_dif, res_dif)
hist(res_dif)

# Compute Pseudo-R2
SS_residual_dif <- sum(residuals(group_dif)^2)
SS_total_dif <- sum((nuevo_df$voc_norm - mean(nuevo_df$voc_norm))^2)

pseudo_R2_dif <- 1 - (SS_residual_dif/SS_total_dif)
print(pseudo_R2_dif)


#  Fit nonlinear mixed effects model ---------- 
group_dif_nlme<- nlme(voc_norm ~ gompertz2(age, dummy, B, C,  coef_B, coef_C),
                      fixed =  B + C  + coef_B + coef_C ~ 1,
                      random = + C   ~ 1 | ID,  
                      groups = ~ ID,
                      data = nuevo_df,
                      start = c( B = 0.16, C = 6.25, coef_B = 1, coef_C = 1),
                      control = nlmeControl(msMaxIter = 10000))  

summary(group_dif_nlme)


#  Fit nonlinear mixed effects model with age group as predictor ---------- 
nuevo_df$age_group <- cut(nuevo_df$age, breaks = seq(from = 12, to = 86, by = 2),  right = FALSE)
nuevo_df |> 
  count(age_group)

# Fit model
group_dif_lme <- lme(voc_norm ~ dummy * age_group, random = ~1 | ID, data = nuevo_df)
anova(group_dif_lme)


# Pairwise comparison of EV and GV within each age group
emmeans_model <- emmeans(group_dif_lme, pairwise  ~ dummy | age_group, 
                         adjust = "holm")
summary(emmeans_model)

emm_df <- as.data.frame(emmeans_model$emmeans)

emm_df |> 
  ggplot(aes(x = age_group, y = emmean, color = as.factor(dummy), group = dummy)) +
  geom_point(position = position_dodge(0.3), size = 1) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                width = 0.2, position = position_dodge(0.3)) +
  labs(x = "Age (years)", y = "Normalized Vocabulary Score", color = "Vocabulary Type") +
  scale_color_manual(values = c("red", "blue"), labels = c("Emotional Vocabulary", "General Vocabulary")) +
  scale_x_discrete(breaks = as.character(seq(12, 84, by = 7))) +   # key line
  theme_light()
  
ggsave("plots/Figure4.png", width = 6, height = 3, dpi = 1200)

