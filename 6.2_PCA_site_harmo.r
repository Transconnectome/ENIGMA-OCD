##################################################################
## ENIGMA-OCD PCA by site ##
##################################################################

library(ggbiplot)
library(dplyr)
library(ggplot2)
library(naniar)
library(ggfortify)
library(ggthemes)

###################################
########## 1. load data ########## 
##################################
setwd("../ENIGMA-OCD/final/3.NeuroCombat/1.adult")
adult_train <- read.csv("Adult_dx_train_harmo.csv")
adult_test <- read.csv("Adult_dx_test_harmo.csv")
adult_total <- rbind(adult_train, adult_test)

setwd("../ENIGMA-OCD/final/3.NeuroCombat/2.pediatric")
pedi_train <- read.csv("Pedi_dx_train_harmo.csv")
pedi_test <- read.csv("Pedi_dx_test_harmo.csv")
pedi_total <- rbind(pedi_train, pedi_test)

###################################
###### 2. data preprocessing ######
##################################
# PCA - numeric only & noNA
#sum(is.na(adult_total$Site))
#sum(is.na(pedi_total$Site))

### check the index of brain feature
# adult
locs.brain.init_train_a <- which(names(adult_train) == 'ACR.FA') #21 #_a: adult / _p: pediatric
locs.brain.end_train_a <- which(names(adult_train)== 'UNC.R.AD') #272 #_a: adult / _p: pediatric
adult_train_b <- adult_train[,c(locs.brain.init_train_a:locs.brain.end_train_a)] #252 vars  #_b: brain

locs.brain.init_test_a <- which(names(adult_test) == 'ACR.FA') #21 #_a: adult / _p: pediatric
locs.brain.end_test_a <- which(names(adult_test)== 'UNC.R.AD') #272 #_a: adult / _p: pediatric
adult_test_b <- adult_test[,c(locs.brain.init_test_a:locs.brain.end_test_a)] #252 vars  #_b: brain

locs.brain.init_total_a <- which(names(adult_total) == 'ACR.FA') #21 #_a: adult / _p: pediatric
locs.brain.end_total_a <- which(names(adult_total)== 'UNC.R.AD') #272 #_a: adult / _p: pediatric
adult_total_b <- adult_total[,c(locs.brain.init_total_a:locs.brain.end_total_a)] #252 vars  #_b: brain

# pediatric
locs.brain.init_train_p <- which(names(pedi_train) == 'ACR.FA') #21 #_a: adult / _p: pediatric
locs.brain.end_train_p <- which(names(pedi_train)== 'UNC.R.AD') #272 #_a: adult / _p: pediatric
pedi_train_b <- pedi_train[,c(locs.brain.init_train_p:locs.brain.end_train_p)] #252 vars #_b: brain

locs.brain.init_test_p <- which(names(pedi_test) == 'ACR.FA') #21 #_a: adult / _p: pediatric
locs.brain.end_test_p <- which(names(pedi_test)== 'UNC.R.AD') #272 #_a: adult / _p: pediatric
pedi_test_b <- pedi_test[,c(locs.brain.init_test_p:locs.brain.end_test_p)] #252 vars  #_b: brain

locs.brain.init_total_p <- which(names(pedi_total) == 'ACR.FA') #21 #_a: adult / _p: pediatric
locs.brain.end_total_p <- which(names(pedi_total)== 'UNC.R.AD') #272 #_a: adult / _p: pediatric
pedi_total_b <- pedi_total[,c(locs.brain.init_total_p:locs.brain.end_total_p)] #252 vars  #_b: brain

### Check missing values in PCA input
# adult
sum(is.na(adult_train_b)) #3204
sum(is.na(adult_test_b)) #0
sum(is.na(adult_total_b)) #3204

library(naniar)
View(miss_case_summary(adult_train_b)) #CST.RD, CST.L.RD, CST.R.RD
locs.A_train_na <- c(which(names(adult_train_b) == 'CST.RD'), which(names(adult_train_b) == 'CST.L.RD'),
                     which(names(adult_train_b) == 'CST.R.RD'))
adult_train_b <- adult_train_b[,-locs.A_train_na]

locs.A_total_na <- c(which(names(adult_total_b) == 'CST.RD'), which(names(adult_total_b) == 'CST.L.RD'),
                     which(names(adult_total_b) == 'CST.R.RD'))
adult_total_b <- adult_total_b[,-locs.A_total_na]

# pediatric
sum(is.na(pedi_train_b)) #0
sum(is.na(pedi_test_b)) #0
sum(is.na(pedi_total_b)) #0

###################################
############ 3. PCA ##############
##################################
# adult
pca_adult_train_b <- prcomp(adult_train_b, center = T, scale. = T)
pca_adult_test_b <- prcomp(adult_test_b, center = T, scale. = T)
pca_adult_total_b <- prcomp(adult_total_b, center = T, scale. = T)

# pediattric
pca_pedi_train_b <- prcomp(pedi_train_b, center = T, scale. = T)
pca_pedi_test_b <- prcomp(pedi_test_b, center = T, scale. = T)
pca_pedi_total_b <- prcomp(pedi_total_b, center = T, scale. = T)

###################################
###### 4. PCA visualization #######
##################################
##################################################################################################
library(factoextra)
### adult
# train
fviz_pca_ind(pca_adult_train_b, geom.ind = "point", pointshape = 21, 
             pointsize = 2, #alpha.ind = 4,
             fill.ind = adult_train$Site, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Site") +
  ggtitle("2D PCA-plot of Train Set in Adult") +
  theme(plot.title = element_text(hjust = 0.5))

# test
fviz_pca_ind(pca_adult_test_b, geom.ind = "point", pointshape = 21, 
             pointsize = 2, #alpha.ind = 4,
             fill.ind = adult_test$Site, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Site") +
  ggtitle("2D PCA-plot of Test Set in Adult") +
  theme(plot.title = element_text(hjust = 0.5))

# total
fviz_pca_ind(pca_adult_total_b, geom.ind = "point", pointshape = 21, 
             pointsize = 2, #alpha.ind = 4,
             fill.ind = adult_total$Site, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Site") +
  ggtitle("2D PCA-plot of Total Set in Adult") +
  theme(plot.title = element_text(hjust = 0.5))

### pediatric
# train
fviz_pca_ind(pca_pedi_train_b, geom.ind = "point", pointshape = 21, 
             pointsize = 2, #alpha.ind = 4,
             fill.ind = pedi_train$Site, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Site") +
  ggtitle("2D PCA-plot of Train Set in Pediatric") +
  theme(plot.title = element_text(hjust = 0.5))

# test
fviz_pca_ind(pca_pedi_test_b, geom.ind = "point", pointshape = 21, 
             pointsize = 2, #alpha.ind = 4,
             fill.ind = pedi_test$Site, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Site") +
  ggtitle("2D PCA-plot of Test Set in Pediatric") +
  theme(plot.title = element_text(hjust = 0.5))

# total
fviz_pca_ind(pca_pedi_total_b, geom.ind = "point", pointshape = 21, 
             pointsize = 2, #alpha.ind = 4,
             fill.ind = pedi_total$Site, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Site") +
  ggtitle("2D PCA-plot of Total Set in Pediatric") +
  theme(plot.title = element_text(hjust = 0.5))

