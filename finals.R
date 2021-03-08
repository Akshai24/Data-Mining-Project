#Q2c
#I have labeled them as 'a' to 'g' in my papers

library ( arules )
data <- paste ("a,b", "a,c", "d,e", "a,c,f", "d,e" , "a,e" , "a,f" , "a,b,g", "b,f" , "d,e,g" , "b,d,e" ,
                 "b,c,g", sep = "\n")
cat( data)
write(data, file = "demo_basket_data")
tr3 <- read.transactions ("demo_basket_data", format = "basket", sep = ",")
tr3
inspect(tr3)


its3 <- apriori(tr3, parameter=list(target ="frequent", support =0.3))
inspect(its3)
summary(its3)

its_max3 <- its3[is.maximal(its3)]
its_max3
inspect (its_max3, by ="support")

its_closed3 <- its3[is.closed(its3)]
its_closed3
inspect (its_closed3)

#Q2d
result <- apriori(tr3, parameter=list(support =0.3, confidence =0.9))
summary(result)
inspect(result)

rules <- sort(result, by ="confidence")
inspect(rules)

rules_lift <- sort ( result, by = "lift")
inspect(rules_lift)


#Q3 Perform a k-means clustering on this data set using the Euclidean distance as the distance function. Here k is chosen as 2, and the centers of 2 clusters were initialized as  c1 = (2,4), c2 = (9,7). What is the center of the first cluster after one iteration? What's the center of the second cluster after the second iteration? (You might use R to help your calculation.) 
library ( tidyverse)
library ( ggplot2)
x <- c(7,1,2,0,9,6)
y <- c(6,5,4,3,7,8)
plot (x,y)

data <- data.frame(cbind(x,y))
fun.scale<-function(x) (x - mean (x, na.rm = TRUE )) / sd(x, na.rm = TRUE )
data_scaled <- mutate_if(data , is.numeric , fun.scale )
  ggplot ( data_scaled , aes (x = x, y = y)) + geom_point ()

  set.seed (2020)
  ks <- 1:5
  
   WSS<-sapply(ks,FUN = function(k) {
    kmeans (data_scaled , centers=k, nstart=5)$tot.withinss   
     })
   WSS <- sapply(ks, FUN = function (k) {
     kmeans(data_scaled , centers = k, nstart = 5)$tot.withinss
   })
  
   
   
km <- kmeans(data_clustered, centers = 2)
km

data_clustered <- add_column(data_clustered , cluster = factor(km$cluster ))
data_clustered


ggplot(data_clustered , aes (x = x, y = y, color = cluster )) + geom_point ()


centroids <- as_tibble (km$centers , rownames = "cluster")
centroids


#------------------------------------------------------

Ex <- data.frame (x <- c(7,1,2,0,9,6),
                    y <- c(6,5,4,3,7,8))

d<- dist(Ex)
install.packages("ggdendro")
library ("ggdendro")

## Clusterine with complete link
hc.complete <- hclust(d, method = "complete")
hc.complete$height
ggdendrogram (hc.complete , labels = T, theme_dendro = T)





