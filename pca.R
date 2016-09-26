# Read in the data
spine_dat <- read.csv("Dataset_spine.csv", header = TRUE, stringsAsFactors = FALSE)

#Re-label the class column
spine_dat$Class_att[spine_dat$Class_att == "Abnormal"] <- 1
spine_dat$Class_att[spine_dat$Class_att == "Normal"] <- 2

# Conduct the pca using the prcom() function. Center will center all variables, and scaling 
# will standardize all the variables.
spine_PCA <- prcomp(spine_dat, center = TRUE, scale. = TRUE)

# Retrieve the first 5 prinicipal component loadings.
head(spine_PCA$rotation, 5)

# Generate the PCA standard deviations
spinePCA_SD<-spine_PCA$sdev

# Generate the PCA variances
spinePCA_var <- round(spinePCA_SD^2, digits = 3)

# Generate the proportion of variance for each component
propvar_spinePCA <- spinePCA_var / sum(spinePCA_var)


# Produce a screeplot and a barplot of the amount of variance each component contributes.
plot(spine_PCA_summary$importance[2, 1:12], xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "h")

barplot(spine_PCA_summary$importance[2, 1:12], xlab = "Principal Component", ylab = "Proportion of Variance Explained")

# Conduct Horn's Parallel Analysis
horn<-paran(spine_dat, iterations = 5000, centile = 0, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = FALSE, seed = 0)
