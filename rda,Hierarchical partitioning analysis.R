
sampledata <- read.csv(file.choose(), head = TRUE, row.names=1)
env <- read.csv(file.choose(), header=TRUE, row.names=1)
sampledata <- t(sampledata)

sampledata <- decostand(sampledata,method = "hellinger")

mite.rda <- rda(sampledata~., env, scale = FALSE)
summary(mite.rda)
plot(mite.rda)


mite.rda.hp <- rdacca.hp(sampledata, env, method = 'RDA', type = 'R2', scale = FALSE)
mite.rda.hp

set.seed(123)
permu_hp <- permu.hp(dv = sampledata, iv = env, method = 'RDA', type = 'adjR2', permutations = 999)
permu_hp


envfit <- envfit(mite.rda, env, permutations  = 999)
r <- as.matrix(envfit$vectors$r)
p <- as.matrix(envfit$vectors$pvals)
env.p <- cbind(r,p)
colnames(env.p) <- c("r2","p-value")
KK <- as.data.frame(env.p)
KK


library(ggplot2)
library(dplyr)



mite_rda_hp_data <- data.frame(
  variable = rownames(mite.rda.hp$Hier.part),
  percentage = mite.rda.hp$Hier.part[, "I.perc(%)"]
)


KK_data <- data.frame(
  variable = rownames(KK),
  p_value = KK[, "p-value"]
)

data_plot <- merge(mite_rda_hp_data, KK_data, by = "variable")


data_plot$significance <- ifelse(data_plot$p_value < 0.05, "Significant", "Not Significant")
data_plot$color <- ifelse(data_plot$p_value < 0.05, "#F39B7FFF", "#8491B4FF")

data_plot$variable <- factor(data_plot$variable, levels = unique(data_plot$variable))


ggplot(data_plot, aes(x = variable, y = percentage, fill = significance)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(percentage, 2)), vjust = -0.5, size = 3.5) + 
  coord_polar(start = 1.5) + 
  ylim(-3, max(data_plot$percentage) + 3) +  
  theme_minimal() +
  labs(y = "% Explained Variation", x = "", fill = "Significance") +
  scale_fill_manual(values = c("Significant" = "#F39B7FFF", "Not Significant" = "#8491B4FF")) +
  theme(
    axis.text.x = element_text(size = 10, hjust = 1), 
    panel.background = element_rect(fill = "white")
  )

