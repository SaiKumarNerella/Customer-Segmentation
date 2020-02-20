#install.packages('ggplot2')
#install.packages('DMwR')
#install.packages('gpairs')
#install.packages('vioplot')

library(ggplot2) #Ploting library
library(GGally) #
library(DMwR) #Data mining functionality

#Setting a random seed to fix the stream through which random data being generated
set.seed(5580)

#PRODUCT BASED#
#read the csv into a data frame
prod= read.csv("/Users/saranjyotsingh/Downloads/DM/assignment1/product_cluster.csv")
View(prod)

#Visualize data as wrt all the features
ggpairs(prod[, which(names(prod) != "ITEM_SK")], upper = list(continuous = ggally_points),
        lower = list(continuous = "points"), title = "Products before outlier removal")

#View the head(features) of data
head(prod, 5) 

#Visualize data as a particular feature
boxplot(prod$TOTAL_REVENUE, main="TOTAL_REVENUE") 
boxplot(prod$BASKETS, main="BASKETS")
boxplot(prod$DISTINCT_CUSTOMERS, main="DISTINCT_CUSTOMERS")
boxplot(prod$AVERAGE_PRICE, main="AVERAGE_PRICE")
boxplot(prod$AVERAGE_QUANTITY_BOUGHT, main="AVERAGE_QUANTITY_BOUGHT")

#Outlier Detection
outlier_row_TOTAL_REVENUE<-prod$ITEM_SK[prod$TOTAL_REVENUE>100000]
outlier_row_TOTAL_REVENUE

outlier_row_BASKETS<-prod$ITEM_SK[prod$BASKETS>80000]
outlier_row_BASKETS

outlier_row_DISTINCT_CUSTOMERS<-prod$ITEM_SK[prod$DISTINCT_CUSTOMERS>15000]
outlier_row_DISTINCT_CUSTOMERS

#outlier_row_AVERAGE_PRICE<-prod$ITEM_SK[prod$AVERAGE_PRICE>100000]
#outlier_row_AVERAGE_PRICE

outlier_row_QUANTITY_BOUGHT<-prod$ITEM_SK[prod$AVERAGE_QUANTITY_BOUGHT>4]
outlier_row_QUANTITY_BOUGHT
#Outlier: "ITEM_SK" 11740941

#read the csv into a data frame
item= read.csv("/Users/saranjyotsingh/Downloads/DM/assignment1/item.csv")
#item=item[item$ITEM_SK %in% prod$ITEM_SK,]
item= item[is.element(item$ITEM_SK, prod$ITEM_SK), ]
head(item$ITEM_SK, 5) 
View(item)

#Outlier Description
View(item[is.element(item$ITEM_SK, outlier_row_BASKETS), ]) #BANANA
outlier_row_QUANTITY_BOUGHT
View(item[is.element(item$ITEM_SK, outlier_row_QUANTITY_BOUGHT), ]) #Outliers rejected

#Outlier Removal
prod.clean = prod[prod$ITEM_SK != outlier_row_BASKETS, ]
length(prod.clean$ITEM_SK)
View(prod.clean)

boxplot(prod.clean$TOTAL_REVENUE, main="TOTAL_REVENUE") 

#Scale values
prod.scale = scale(prod.clean[-1])
View(prod.scale)

#Performing k-means on range from low to high and visualize it as a Elbow plot
withinSSrange <- function(data,low,high,maxIter)
{
  withinss = array(0, dim=c(high-low+1));
  for(i in low:high)
  {
    withinss[i-low+1] <- kmeans(data, i, maxIter)$tot.withinss
  }
  withinss
}

#Elbow plot to determine the optimal number of clusters between 1 and 50.
plot(withinSSrange(prod.scale, 1, 25,150),
     type = "l",
     main = "Elbow Curve",
     xlab="No. of clusters",
     ylab="Sum of squared errors",
     col.main="red", 
     col.lab="blue")
#i.e. cluster selected= 8

p_km = kmeans(prod.scale, 8, 150)

#Denormalize data by reversing scale function
prod.realCenters = unscale(p_km$centers, prod.scale)

## Combine values and their cluster
clustered_prod = cbind(prod.clean, p_km$cluster)

#Visualize the cluster
plot(clustered_prod[,2:5], col=p_km$cluster)

#Customer BASED#
#read the csv into a data frame
cust= read.csv("/Users/saranjyotsingh/Downloads/DM/assignment1/customer_cluster.csv")
View(cust)

#Visualize data as wrt all the features
ggpairs(cust[, which(names(prod) != "CUSTOMER_SK")], upper = list(continuous = ggally_points),
        lower = list(continuous = "points"), title = "Customer before outlier removal")
#View the head(features) of data
head(cust, 5) 

#Visualize data as a particular feature
boxplot(cust$PRODUCT_BOUGHT, main="PRODUCT_BOUGHT") 
boxplot(cust$DISTINCT_PRODUCT_BOUGHT, main="DISTINCT_PRODUCT_BOUGHT")
boxplot(cust$TOTAL_REVENUE, main="TOTAL_REVENUE")
boxplot(cust$VISITS, main="VISITS")
boxplot(cust$AVERAGE_AMOUNT_SPENT, main="AVERAGE_AMOUNT_SPENT")

#Outlier Detection
outlier_cust_row_TOTAL_REVENUE<-cust$CUSTOMER_SK[cust$TOTAL_REVENUE>1500000]
outlier_cust_row_TOTAL_REVENUE

outlier_cust_row_PRODUCT_BOUGHT<-cust$CUSTOMER_SK[cust$PRODUCT_BOUGHT>15000]
outlier_cust_row_PRODUCT_BOUGHT

outlier_cust_row_DISTINCT_PRODUCT_BOUGHT<-cust$CUSTOMER_SK[cust$DISTINCT_PRODUCT_BOUGHT>15000]
outlier_cust_row_DISTINCT_PRODUCT_BOUGHT

#outlier_cust_row_AVERAGE_AMOUNT_SPENT<-cust$CUSTOMER_SK[cust$AVERAGE_AMOUNT_SPENT>500]
#outlier_cust_row_AVERAGE_AMOUNT_SPENT

outlier_cust_row_VISITS<-cust$CUSTOMER_SK[cust$VISITS>15000]
outlier_cust_row_VISITS
#Outlier: "CUSTOMER_SK" 1


#Outlier Removal
cust.clean = cust[cust$CUSTOMER_SK != 1, ]
length(cust.clean$CUSTOMER_SK)
View(cust.clean)

boxplot(cust.clean$VISITS, main="VISITS")

#Scale values
cust.scale = scale(cust.clean[-1])
View(cust.scale)

#Elbow plot to determine the optimal number of clusters between 1 and 50.
plot(withinSSrange(cust.scale, 1, 15, 150),
     type = "l",
     main = "Elbow Curve",
     xlab="No. of clusters",
     ylab="Sum of squared errors",
     col.main="red", 
     col.lab="blue")
#i.e. cluster selected= 7

c_km = kmeans(cust.scale, 7, 150)

#Denormalize data by reversing scale function
cust.realCenters = unscale(c_km$centers, cust.scale)

## Combine values and their cluster
clustered_cust = cbind(cust.clean, c_km$cluster)

#Visualize the cluster
plot(clustered_cust[,2:5], col=c_km$cluster)
plot(clustered_cust[,2:5], col=c_km$cluster)

#Extracting data for profiling
View(clustered_cust)
write.csv(clustered_cust,"/Users/saranjyotsingh/Downloads/DM/assignment1/final_cluster_cust.csv", row.names = FALSE)
write.csv(clustered_prod,"/Users/saranjyotsingh/Downloads/DM/assignment1/final_cluster_prod.csv", row.names = FALSE)

View(cust.clean)
ggpairs(cust.clean[, which(names(cust.clean) != "CUSTOMER_SK")], upper = list(continuous = ggally_points),
        lower = list(continuous = "points"), title = "Customer before outlier removal")

#Top 5 porducts bought




