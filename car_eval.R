# import library
if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE)
}
if (!require("vcd")) {
  install.packages("vcd", dependencies = TRUE)
}
library(ggplot2)
library(vcd)

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
        col = c("#E69F00", "#56B4E9", "#009E73", "#F0E442"),
        ylim = c(0, 75),
        cex.names = 1.2,
        cex.lab = 1.2)
text(x = seq_along(prop_dist), 
     y = prop_dist + 2,
     labels = paste0(round(prop_dist, 1), "%"),
     col = "black",
     cex = 1.0)
dev.off()

# chi-square analysis
features <- setdiff(names(car_data), "class")
chi_results <- lapply(features, function(feat) {
  tbl <- table(car_data[[feat]], car_data$class)
  chi_test <- chisq.test(tbl)
  data.frame(Feature = feat, 
             P_Value = chi_test$p.value, 
             Chi_Squared = chi_test$statistic)
})
do.call(rbind, chi_results)




