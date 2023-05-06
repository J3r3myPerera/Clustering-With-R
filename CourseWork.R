
#importing the data
vehicles <- read.csv("vehicles.csv", header = TRUE, sep = ",")
vehicles
head(vehicles)

#Create the function to standardize the variables
standardize = function(x) {
  return((x - mean(x, na.rm = TRUE))/ sd(x, na.rm = TRUE))
}

#using Z-Score Standardization 
vehiclesNormZ <- as.data.frame(apply(vehicles[2:18],2,standardize))
vehiclesNormZ

#Removing NA records
vehiclesNormZ <- vehiclesNormZ[complete.cases(vehiclesNormZ),]

#check if there are any na values
sum(is.na(vehiclesNormZ))

#to check the outliers
boxplot(vehiclesNormZ)

#To get the Quartile ranges for removing the outliers
summary(vehiclesNormZ$Rad.Ra)

IQR_Rad.Ra = 0.77853 - (-0.83475)
Upfen_Rad.Ra = 0.77853 + 1.5 * IQR_Rad.Ra
Upfen_Rad.Ra

summary(vehiclesNormZ$Pr.Axis.Ra)

IQR_Pr.Axis.Ra = 0.41912 - (-0.59504)
Upfen_Pr.Axis.Ra = 0.41912 + 1.5 * IQR_Pr.Axis.Ra
Upfen_Pr.Axis.Ra

summary(vehiclesNormZ$Max.L.Ra)

IQR_Max.L.Ra = 0.3114 - (- 0.3406)
Upfen_Max.L.Ra = 0.3114 + 1.5 * IQR_Max.L.Ra
Upfen_Max.L.Ra
Downfen_Max.L.Ra = - 0.3406 - 1.5 * IQR_Max.L.Ra
Downfen_Max.L.Ra

summary(vehiclesNormZ$Sc.Var.Maxis)

IQR_Sc.Var.Maxis = 0.9038 - (-0.6888 )
Upfen_Sc.Var.Maxis = 0.9038 + 1.5 * IQR_Sc.Var.Maxis
Upfen_Sc.Var.Maxis

summary(vehiclesNormZ$Sc.Var.maxis)

IQR_Sc.Var.maxis = 0.8325 - (- 0.6885)
Upfen_Sc.Var.maxis = 0.8325 + 1.5 * IQR_Sc.Var.maxis
Upfen_Sc.Var.maxis

summary(vehiclesNormZ$Skew.Maxis)

IQR_Skew.Maxis = 0.3390 - (-0.7296)
Upfen_Skew.Maxis =  0.3390 + 1.5 * IQR_Skew.Maxis
Upfen_Skew.Maxis


summary(vehiclesNormZ$Skew.maxis)

IQR_Skew.maxis = 0.53329 - (- 0.88995)
Upfen_Skew.maxis = 0.53329 + 1.5 * IQR_Skew.maxis
Upfen_Skew.maxis

summary(vehiclesNormZ$Kurt.maxis)

IQR_Kurt.maxis = 0.7167 - (-0.8509)
Upfen_Kurt.maxix = 0.7167 + 1.5 * IQR_Kurt.maxis
Upfen_Kurt.maxix

summary(vehiclesNormZ)
vehiclesClean = subset(vehiclesNormZ, vehiclesNormZ$Comp<= 3.0751 & vehiclesNormZ$Circ<= 2.2915 & vehiclesNormZ$D.Circ<=1.8965 & vehiclesNormZ$Rad.Ra <= Upfen_Rad.Ra & vehiclesNormZ$Pr.Axis.Ra<=Upfen_Pr.Axis.Ra & vehiclesNormZ$Max.L.Ra<=Upfen_Max.L.Ra & vehiclesNormZ$Max.L.Ra>=Downfen_Max.L.Ra & vehiclesNormZ$Scat.Ra<=2.8925 & vehiclesNormZ$Elong<=2.5688 & vehiclesNormZ$Pr.Axis.Rect<=3.2472 & vehiclesNormZ$Max.L.Rect<=2.7557 & vehiclesNormZ$Sc.Var.Maxis<=Upfen_Sc.Var.Maxis & vehiclesNormZ$Sc.Var.maxis<=Upfen_Sc.Var.maxis & vehiclesNormZ$Ra.Gyr<= 2.86657 & vehiclesNormZ$Skew.Maxis<=Upfen_Skew.Maxis & vehiclesNormZ$Skew.maxis<=Upfen_Skew.maxis & vehiclesNormZ$Kurt.maxis<=Upfen_Kurt.maxix & vehiclesNormZ$Kurt.Maxis<=2.7689)
boxplot(vehiclesClean)

#loading the NbClust library
library(NbClust)

#Information on the dataset
str(vehiclesClean)

#Setting the data columns to the variable
x = vehiclesClean[]

#Setting the seed value so that the result is reproducable
set.seed(26)

#Setting the NBClust method
#Using the Euclidean method
clusterNo=NbClust(x,distance="euclidean", min.nc=2,max.nc=15,method="kmeans",index="all")

#using the manhattan method
clusterNo=NbClust(x,distance="manhattan", min.nc=2,max.nc=15,method="kmeans",index="all")

#Using the maximum distancee
clusterNo=NbClust(x,distance="maximum", min.nc=2,max.nc=15,method="kmeans",index="all")


#setting the Elbow method
k=2:15
set.seed(40)
WSS = sapply(k, function(k) {kmeans(x, centers=k)$tot.withinss})
plot(k, WSS, type="l", xlab= "Number of k", ylab="Within sum of squares")

#Calling the libaries for fviz_clust function
library(cluster)
library(factoextra)

#Using Gap Statistic
fviz_nbclust(vehiclesClean, kmeans, method = 'gap_stat')

#Using the average silhoute method
fviz_nbclust(vehiclesClean, kmeans, method = 'silhouette')

#create the kmeans model with 2 clusters
kc <- kmeans(x,2)
kc

#Checking the "Between Sum of Squares" and the "total within-cluster sum of square"
wss = kc$tot.withinss
bss = kc$betweenss
wss
bss

#illustration of the clusters
fviz_cluster(kc,data=vehiclesClean)
fviz_cluster(kc, data = vehiclesClean, ellipse.type = "euclid",star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())

#getting the silhouette coefficient
sil <- silhouette(kc$cluster, dist(vehiclesClean))
fviz_silhouette(sil)


#2nd Subtask
#Appling the PCA 
df = cbind(vehicles$Comp , vehicles$Circ, vehicles$D.Circ, vehicles$Rad.Ra, vehicles$Pr.Axis.Ra, vehicles$Max.L.Ra, vehicles$Scat.Ra, vehicles$Elong, vehicles$Pr.Axis.Rect, vehicles$Max.L.Rect, vehicles$Sc.Var.Maxis, vehicles$Sc.Var.maxis, vehicles$Ra.Gyr, vehicles$Skew.Maxis, vehicles$Skew.maxis, vehicles$Kurt.maxis, vehicles$Kurt.Maxis, vehicles$Holl.Ra)
df1 = cbind(vehiclesClean$Comp , vehiclesClean$Circ, vehiclesClean$D.Circ, vehiclesClean$Rad.Ra, vehiclesClean$Pr.Axis.Ra, vehiclesClean$Max.L.Ra, vehiclesClean$Scat.Ra, vehiclesClean$Elong, vehiclesClean$Pr.Axis.Rect, vehiclesClean$Max.L.Rect, vehiclesClean$Sc.Var.Maxis, vehiclesClean$Sc.Var.maxis, vehiclesClean$Ra.Gyr, vehiclesClean$Skew.Maxis, vehiclesClean$Skew.maxis, vehiclesClean$Kurt.maxis, vehiclesClean$Kurt.Maxis, vehiclesClean$Holl.Ra)

pca_vehicels = prcomp(df, center = TRUE, scale = TRUE)
pca_vehicels1 = prcomp(df1, center = TRUE, scale = TRUE)

summary(pca_vehicels)
summary(pca_vehicels1)

#Creating the new datset
vehiclesNew = as.data.frame(-pca_vehicels1$x[,1:8])
head(vehiclesNew)

#The new k according to the Elbow method
fviz_nbclust(vehiclesNew, kmeans, method='wss') 

#Using the avarage silhouette method
fviz_nbclust(vehiclesNew, kmeans, method = 'silhouette')

#Using the gap-star method
fviz_nbclust(vehiclesNew, kmeans, method = 'gap_stat')

#Using the  NBclust method
xNew = vehiclesNew[]
clusterNoNew=NbClust(xNew,distance="euclidean", min.nc=2,max.nc=15,method="kmeans",index="all")
clusterNoNew=NbClust(xNew,distance = "manhattan", min.nc=2,max.nc=15,method="kmeans",index="all")
clusterNoNew=NbClust(xNew,distance="maximum", min.nc=2,max.nc=15,method="kmeans",index="all")

#using number of clusters as 2
k =2
kmeans_vehicles = kmeans(vehiclesNew, centers = k, nstart = 10)
kmeans_vehicles

#Visualizing the variables
fviz_cluster(kmeans_vehicles, data = vehiclesClean)
fviz_cluster(kmeans_vehicles, data = vehiclesClean, ellipse.type = "euclid",star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())

#getting the WSS and the BSS
wssNew = kmeans_vehicles$tot.withinss
bssNew = kmeans_vehicles$betweenss
wssNew
bssNew

#Visualizing the silhouette graph
silNew <- silhouette(kmeans_vehicles$cluster, dist(vehiclesNew))
fviz_silhouette(silNew)

#for the calinski-harabasz index
clusplot(vehiclesNew, kmeans_vehicles$cluster, color = TRUE, shade=TRUE, labels = 2, lines = 0)

#For kmeans clustering on pca dataset, compute Calinski-Harabasz Index 
set.seed(123)
kmeans_analyse <- kmeans(vehiclesNew , centers = 2)
Calinski_Harabasz_index <- clusGap(vehiclesNew, kmeans, K.max = 10, nstart = 25)

#Plot Calinski-Harabasz 
plot(Calinski_Harabasz_index, main = "Calinski-Harabasz Index Plot for K-Means Clustering")
