###Machine Learning in R
###Chapter 3 - Classification - Using Nearest Neighbours



# Housekeeping ------------------------------------------------------------

rm(list=ls())

# Loading Data and Exploration---------------------------------------------

wbcd<-read.csv("./Datasets/wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
wbcd<-wbcd[-1]
table(wbcd$diagnosis)
wbcd$diagnosis<-factor(wbcd$diagnosis, levels = c("B","M"), labels = c("Benign","Malignant"))

round(prop.table(table(wbcd$diagnosis))*100, digits = 1)
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])

# Normalising Data --------------------------------------------------------

normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

wbcd_n<-as.data.frame(lapply(wbcd[2:31],normalize))
summary(wbcd_n$area_mean)
summary(wbcd_n$smoothness_mean)


# Training and Test Datasets ----------------------------------------------

wbcd_train<-wbcd_n[1:469,]
wbcd_test<-wbcd_n[470:569,]
wbcd_train_labels<-wbcd[1:469, 1]
wbcd_test_labels<-wbcd[470:569, 1]


# Training ----------------------------------------------------------------

install.packages("class")
library(class)

wbcd_test_pred<-knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k =21)


# Evaluating the Model ----------------------------------------------------

install.packages("gmodels")
library(gmodels)

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)



# Improving Model Performance ---------------------------------------------

wbcd_z<-as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

wbcd_train<-wbcd_z[1:469,]
wbcd_test<-wbcd_z[470:569,]
wbcd_train_labels<-wbcd[1:469, 1]
wbcd_test_labels<-wbcd[470:569, 1]

wbcd_test_pred<-knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k =21)

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)





