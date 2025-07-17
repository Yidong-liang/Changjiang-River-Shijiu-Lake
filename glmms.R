


data$habitat <- factor(data$habitat, levels = c("SL", "RC","YR"))


str(data)


desc_stats <- data %>%
  group_by(habitat) %>%
  summarise(
    Richness_mean = mean(Richness, na.rm = TRUE),
    Richness_sd = sd(Richness, na.rm = TRUE),
    Shannon_mean = mean(Shannon, na.rm = TRUE),
    Shannon_sd = sd(Shannon, na.rm = TRUE)
  )
print(desc_stats)

model_shannon <- lmer(Shannon ~ waterlevel + habitat + (1|season), data = data)


model_richness <- glmer(Richness ~ waterlevel + habitat + (1|season), 
                        data = data, family = poisson(link = "log"))

summary(model_shannon)
summary(model_richness)

）
vif(model_shannon)

shannon_coef <- summary(model_shannon)$coefficients
shannon_ci <- confint(model_shannon, parm = "beta_", method = "Wald")


results_table <- data.frame(
  Variable = rownames(shannon_coef),
  Estimate = round(shannon_coef[, "Estimate"], 3),
  SE = round(shannon_coef[, "Std. Error"], 3),
  CI_lower = round(shannon_ci[, 1], 3),
  CI_upper = round(shannon_ci[, 2], 3),
  P_value = round(shannon_coef[, "Pr(>|t|)"], 3)
)
print(results_table)


candidate_models <- dredge(model_shannon)

avg_model <- model.avg(candidate_models, subset = delta < 2)
summary(avg_model)

importance <- data.frame(
  Variable = names(avg_model$importance),
  Importance = round(avg_model$importance, 2)
)
print(importance)

ggplot(data, aes(x = waterlevel, y = Shannon, color = habitat)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Water Level", y = "Shannon Diversity", color = "Habitat") +
  theme_classic()

ggplot(data, aes(x = habitat, y = Shannon, fill = habitat)) +
  geom_boxplot() +
  labs(x = "Habitat", y = "Shannon Diversity") +
  theme_minimal()



model_shannon <- lmer(Shannon ~ waterlevel + habitat + (1|season), data = data)

model_richness <- glmer(Richness ~ waterlevel + habitat + (1|season), 
                        data = data, family = poisson)

candidate_models <- dredge(model_shannon)
print(candidate_models)

