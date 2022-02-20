data <-read.csv("wine_data.csv")
dim(data)
summary(data)
library(plyr)
attach("data")
hist(Alcohol)

#check whether rows have NA values
row.has.na <- apply(data, 1, function(x){any(is.na(x))})
sum(row.has.na) #since this = 0 no NA values

#remove duplicate rows
unique(data)

#no column naming changes


#plot varibales
library(purrr)
library(tidyr)
library(ggplot2)

data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#finding outliers
#find absolute value of z-score for each value in each column
z_scores <- as.data.frame(sapply(data, function(data) (abs(data-mean(data))/sd(data))))
head(z_scores)

#only keep rows in dataframe with all z-scores less than absolute value of 3 
no_outliers <- data[!rowSums(z_scores>3), ]
dim(no_outliers)

#drop class column
library(dplyr)
new_data<-select(no_outliers,-c(Class))
dim(new_data)

# Scaling dataset
df <- scale(no_outliers)



install.packages("factoextra")
library(factoextra)

#K-means clustering
km <- kmeans(df, centers = 4, nstart = 25)

# Visualize the clusters
fviz_cluster(km, data = df)

#DB Scan

install.packages("dbscan")
library(dbscan)

d <- dbscan::dbscan(df, eps = 0.45, MinPts =  2)
d
