library(readxl)
library(NbClust)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(cluster)
library(knitr)
library(ggfortify)
library(tidymodels)
library(factoextra)
library(flexclust)

library(funtimes)

#reading the data
vehicles <- read_excel("vehicles.xlsx")

#viewing the data
view(vehicles)
summary(vehicles)


#pca_result <- prcomp(vehicles_scaled, scale = TRUE)
#names(pca_result)

#pca_result$rotation




#firs twe have to factor the data since we have the numerical and nominal data 
#then used the  clean names method from the janitor package  to clean the dirty data from the dataset
vehicles_fact <- mutate(vehicles,Class=as_factor(vehicles$Class))
 


summary(vehicles_fact)

vehicles_clean <-janitor::clean_names(vehicles_fact)
summary(vehicles_clean)

#van outlier

vehicles_clean %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  dplyr::filter(class == "van") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Outlier Detection for class: 'van'")

#bus outlier

vehicles_clean %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "bus") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Outlier Detection for class: 'bus'")

#saab outlier
vehicles_clean %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "saab") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Outlier Detection for class: saab")

#opel outlier

vehicles_clean %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Outlier Detection for class: opel")


## outlier removal
vehicles_bus = vehicles_clean %>%
  
  filter(class == "bus") %>%
  
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

vehicles_van = vehicles_clean %>%
  
  filter(class == "van") %>%
  
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

vehicles_opel = vehicles_clean %>%
  
  filter(class == "opel") %>%
  
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

vehicles_saab = vehicles_clean %>%
  
  filter(class == "saab") %>%
  
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

combined = bind_rows(list(vehicles_bus,vehicles_opel,vehicles_saab,vehicles_van)) %>%
  
  arrange(samples)



print(combined)

combined %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "bus") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Transformed Outliers class: 'bus'")

combined %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "saab") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Transformed Outliers for class: saab")



combined %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "opel") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Transformed Outliers for class: opel")

combined %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "van") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Transformed Outliers for class: opel")

# Remove the sample name and the class name. Both of these will be remove so that only 

#numerical data is left for the algorithm.

vehicles_data_points = combined %>%
  
  select(-samples, -class )

# Now that we have the "vehicles_data_points" dataset, scaling is performed

vehicles_scaled = vehicles_data_points %>%
  
  mutate(across(everything(), scale))




# Using a seed because the points taken for the cluster always changes when u run the code
#set.seed(123)





# Perform the kmeans using the NbClust function




# Use Euclidean
#for distance

cluster_euclidean = NbClust(vehicles_scaled,distance="euclidean",
                            min.nc=2,max.nc=10,method="kmeans",index="all")



table(vehicles$Class,cluster_euclidean$Best.partition)



#clustering using manhattan distance

cluster_manhattan = NbClust(vehicles_scaled,distance="manhattan",
                            min.nc=2,max.nc=15,method="kmeans",index="all")

# Comparing the predicted clusters with the original data
table(vehicles$Class,cluster_manhattan$Best.partition)

 
 
 #finding the optimal number of cluster using elbow methods
  #way 1
 k = 1:10
 #set.seed(42)	
 WSS = sapply(k, function(k) {kmeans(vehicles_scaled, centers=k)$tot.withinss})
 plot(k, WSS, type="l", xlab= "Number of k", ylab="Within sum of squares")
 

 

clusplot(vehicles,cluster_euclidean$Best.partition, color=T, shade=T, labels=0, lines=0)
# Use manhattan for distance

#way 2
tot.withinss <- vector(mode="character", length=10)
for (i in 1:10){
  kc <- kmeans(vehicles_scaled, center=i, nstart=20)
  tot.withinss[i] <- kc$tot.withinss
}

plot(1:10, tot.withinss, type="b", pch=19)
#as we can see the optimal number of cluster is 2

kc =kmeans(vehicles_scaled,2,nstart=20 )
kc
kc$tot.withinss

#table(vehicles$Class,kc$cluster)

autoplot(kc,vehicles_scaled,frame=TRUE)

kc$centers

k2 <- kmeans(vehicles_scaled, centers = 2, nstart = 25)
k3 <- kmeans(vehicles_scaled, centers = 3, nstart = 25)
k4 <- kmeans(vehicles_scaled, centers = 4, nstart = 25)
k5 <- kmeans(vehicles_scaled, centers = 5, nstart = 25)


k4$tot.withinss
# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = vehicles_scaled) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data =vehicles_scaled) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data =vehicles_scaled) + ggtitle("k = 4")


library(gridExtra)
grid.arrange(p1, p2, p3,  nrow = 3)

fviz_cluster(k2, data = vehicles_scaled)




