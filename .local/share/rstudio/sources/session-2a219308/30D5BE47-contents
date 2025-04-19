# import library
if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE)
}
if (!require("vcd")) {
  install.packages("vcd", dependencies = TRUE)
}
if (!require("ca")) {
  install.packages("ca", dependencies = TRUE)
}
if (!require("caret")) {
  install.packages("caret", dependencies = TRUE)
}
if (!require("smotefamily")) {
  install.packages("smotefamily", dependencies = TRUE)
}
if (!require("corrplot")) {
  install.packages("corrplot", dependencies = TRUE)
}
if (!require("txtplot")) {
  install.packages("txtplot", dependencies = TRUE)
}
if (!require("GGally")) {
  install.packages("GGally", dependencies = TRUE)
}
if (!require("kableExtra")) {
  install.packages("kableExtra", dependencies = TRUE)
}
if (!require("htmltools")) {
  install.packages("htmltools", dependencies = TRUE)
}
if (!require("webshot2")) {
  install.packages("webshot2", dependencies = TRUE)
}
if (!require("ggsci")) {
  install.packages("ggsci", dependencies = TRUE)
}
if (!require("ggthemes")) {
  install.packages("ggthemes", dependencies = TRUE)
}
library(ggplot2)
library(vcd)
library(RColorBrewer)
library(scales)
library(dplyr)
library(ca)
library(gridExtra)
library(caret)
library(smotefamily)
library(corrplot)
library(txtplot)
library(GGally)
library(tidyr)
library(patchwork)
library(knitr)
library(kableExtra)
library(htmltools)
library(webshot2)
library(ggsci)
library(ggthemes)

# load data
col_names <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "class")
car_data <- read.csv("car.data", header = FALSE, col.names = col_names, stringsAsFactors = TRUE)

# set factor level
car_data$buying <- factor(car_data$buying, 
                          levels = c("low", "med", "high", "vhigh"),
                          ordered = TRUE)
car_data$maint <- factor(car_data$maint, 
                         levels = c("low", "med", "high", "vhigh"),
                         ordered = TRUE)
car_data$lug_boot <- factor(car_data$lug_boot,
                            levels = c("small", "med", "big"),
                            ordered = TRUE)
car_data$safety <- factor(car_data$safety,
                          levels = c("low", "med", "high"),
                          ordered = TRUE)
car_data$class <- factor(car_data$class,
                         levels = c("unacc", "acc", "good", "vgood"),
                         ordered = TRUE)

# === EDA ===

# general explore
str(car_data)
summary(car_data)

# class distribution analysis
png("result_1_distribution.png", width = 800, height = 600, res = 100)
class_dist <- table(car_data$class)
prop_dist <- prop.table(class_dist) * 100
barplot(prop_dist, 
        main = "Car Evaluation Class Distribution",
        xlab = "Evaluation Category",
        ylab = "Percentage (%)",
        col = palette("Paired")[1:4],
        ylim = c(0, 75),
        cex.names = 1.2,
        cex.lab = 1.2)
text(x = seq_along(prop_dist), 
     y = prop_dist + 2,
     labels = paste0(round(prop_dist, 1), "%"),
     col = "black",
     cex = 1.0)
dev.off()

# chi-square (association between feature & target)
features <- setdiff(names(car_data), "class")
chi_results <- lapply(features, function(feat) {
  tbl <- table(car_data[[feat]], car_data$class)
  chi_test <- chisq.test(tbl)
  data.frame(Feature = feat, 
             P_Value = chi_test$p.value, 
             Chi_Squared = chi_test$statistic)
})
do.call(rbind, chi_results)

# visualize distribution
plot_data <- car_data %>%
  mutate(class = factor(class, levels = c("unacc", "acc", "good", "vgood")))
features <- setdiff(names(plot_data), "class")

plots <- lapply(features, function(feature) {
  ggplot(plot_data, aes_string(x = feature, fill = "class")) +
    geom_bar(position = "fill") +
    labs(title = paste("Distribution by", feature), y = "Proportion") +
    scale_fill_brewer(palette = "Paired") +
    theme(
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
      axis.title.x = element_text(size = 14),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      panel.background = element_blank(),  
      panel.grid.major = element_line(color = "gray90") 
    )
})

png("result_2_distribution.png", width = 1600, height = 1200, res = 100)
do.call(grid.arrange, c(plots, ncol = 2))
dev.off()

# === PRE-PROCESSING ===

# ordinal encoding
car_data <- car_data %>%
  mutate(
    buying = factor(buying, levels = c("low", "med", "high", "vhigh"), ordered = TRUE),
    maint = factor(maint, levels = c("low", "med", "high", "vhigh"), ordered = TRUE),
    safety = factor(safety, levels = c("low", "med", "high"), ordered = TRUE),
    class = factor(class, levels = c("unacc", "acc", "good", "vgood"), ordered = TRUE),
    doors = factor(doors, levels = c("2", "3", "4", "5more"), ordered = TRUE),
    persons = factor(persons, levels = c("2", "4", "more"), ordered = TRUE),
    class = factor(class, levels = c("unacc", "acc", "good", "vgood"), ordered = TRUE)
  )

ordinal_features <- setdiff(names(car_data), "class")
target <- "class"
car_encoded <- data.frame(
  lapply(car_data[, ordinal_features], as.integer),
  class = car_data[[target]]
)

# visualize
str(car_data)
summary(car_encoded)
features <- setdiff(names(car_encoded), "class")
for (feat in features) {
  cat("\n---", feat, "vs. class ---\n")
  print(table(car_encoded[[feat]], car_encoded$class))
}

# standardization scaling
set.seed(123)
train_index <- sample(1:nrow(car_encoded), 0.8 * nrow(car_encoded))
train_data <- car_encoded[train_index, ]
test_data <- car_encoded[-train_index, ]

preproc <- preProcess(train_data[, -ncol(train_data)], method = c("center", "scale"))
train_original <- train_data
train_scaled <- predict(preproc, train_data)
test_scaled <- predict(preproc, test_data)

# visualize
scaling_comparison <- bind_rows(
  list(Original = train_original %>% select(-class), Scaled = train_scaled %>% select(-class) ),
  .id = "Group"
) %>%
  pivot_longer(
    cols = -Group,
    names_to = "Feature",
    values_to = "Value"
  )
p <- ggplot(scaling_comparison, aes(x = Value, fill = Group)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ Feature, scales = "free") +
  scale_fill_manual(values = c("Original" = "#29ff3b","Scaled" = "#2b92ff")) 
  labs(
    title = "Feature Distributions: Before vs. After Scaling",
    x = "Value",
    y = "Density",
    fill = "Data State"
  ) +
  theme_minimal()
ggsave(filename = "result_3_scaling.png", plot = p)

# over-sampling
cat("Class distribution BEFORE balancing:\n")
print(table(train_scaled$class))
train_balanced <- upSample(x = train_scaled[, -ncol(train_scaled)],
                           y = train_scaled[, ncol(train_scaled)],
                           yname = "class")
cat("\nClass distribution AFTER SMOTE:\n")
print(table(train_balanced$class))

# visualize
class_counts <- data.frame(
  Stage = c(rep("Before", 4), rep("After", 4)),
  Class = factor(
    rep(c("unacc", "acc", "good", "vgood"), 2),
    levels = c("unacc", "acc", "good", "vgood")  # Preserve order
  ),
  Count = c(
    table(train_scaled$class),
    table(train_balanced$class) 
  )
)

p <- ggplot(class_counts, aes(x = Count, y = Class, fill = Stage)) +
  geom_col(position = position_dodge(width = 0.8)) +
  labs(
    title = "Class Distribution Before vs. After Over-Sampling",
    x = "Count",
    y = "Class",
    fill = "Stage"
  ) +
  scale_fill_manual(values = c("#2b92ff", "#29ff3b")) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"), 
    axis.text.y = element_text(size = 12), 
    axis.title = element_text(size = 14),
    legend.position = "bottom"
  )
ggsave(filename = "result_4_over-sampling.png", plot = p)

# === MODELING ===

# random forest
rf_ctrl <- trainControl(method = "repeatedcv",
                        number = 3,
                        classProbs = TRUE)
rf_model <- train(
  class ~ .,
  data    = train_balanced,
  method  = "rf",
  trControl = rf_ctrl,
  tuneLength = 5
)

rf_pred <- predict(rf_model, newdata = test_scaled)
rf_cm   <- confusionMatrix(rf_pred, test_scaled$class)
print(rf_model)
print(rf_cm)  

# SVM
svm_ctrl <- trainControl(method = "repeatedcv",
                         number = 3,
                         classProbs = TRUE)
svm_model <- train(
  class ~ .,
  data    = train_balanced,
  method  = "svmRadial",
  trControl = svm_ctrl,
  tuneLength = 5
)

svm_pred <- predict(svm_model, newdata = test_scaled)
svm_cm   <- confusionMatrix(svm_pred, test_scaled$class)
print(svm_model)
print(svm_cm) 


