library(e1071)
library(mlbench)
library(ggplot2)
library(caret)
library(cluster)
library(fpc)
library(ade4)
library(NbClust)
library(factoextra)
library(flexclust)


data("HouseVotes84")
USCD = HouseVotes84

str(USCD)

summary(USCD$Class)
summary(USCD)

AUSCD = USCD
USCD = USCD[-1]
USCD[is.na(USCD)] <- as.factor("n")
USCD = as.data.frame(ifelse(USCD == "y", 1, 0))

summary(USCD)

par(mfrow=c(1,1))

Sim = dist.binary(USCD, method = 2)

nb <- NbClust(USCD, diss = Sim, min.nc = 2, distance = NULL, max.nc = 10, method = "centroid", index ="all")
summary(nb)
fviz_nbclust(nb) + theme_minimal()


Model = vector()
RandIndex = double()
InitialClusters = AUSCD[,1]
InitialClusters = ifelse(InitialClusters == "republican", 1, 2)
names(InitialClusters) = c(1:nrow(USCD))

findBestDistanceMeasure = function(){
  for(i in 1:10){
    if( i == 7 || i == 8 || i == 9){
      RandIndex[i] = 0
    }
    else{
      Sim = dist.binary(USCD, method = i)
      mod = pam(Sim, 2, diss = "TRUE")
      names(mod$clustering) = c(1:nrow(USCD))
      tab = table(mod$clustering, InitialClusters)
      tab.ca = classAgreement(tab,match.names = TRUE)
      RandIndex[i] = tab.ca$rand
    }
  }
  return(which.max(RandIndex))
}

tellMe = findBestDistanceMeasure()

Sim = dist.binary(USCD, method = 1)
Model_1 = pam(Sim, 2, diss = "TRUE")
plot(Model_1)
clusplot(USCD, Model_1$clustering, color=TRUE, shade=TRUE,
         labels=2, lines=0, main = "Clusters identified by Jaccard")


par(mfrow=c(1,1))

Sim = dist.binary(USCD, method = 2)
Model_2 = pam(Sim, 2, diss = "TRUE")
plot(Model_2, main = "Silhouette for K = 2 with Simple Matching Coefficient")
clusplot(USCD, Model_2$clustering, color=TRUE, shade=TRUE,
         labels=2, lines=0, main = "Clusters identified by K-Medoids & Simple Match Making", border = NA)

Model_2$medoids
Model_2$clusinfo
Model_2$objective
print(Model_2$silinfo$widths <0)
which(Model_2$silinfo$widths[,3] <= 0)[2]


Model_2 = pam(Sim, 4, diss = "TRUE")
plot(Model_2, main = "Silhouette for K = 4 with Simple Matching Coefficient",border = NA)
clusplot(USCD, Model_2$clustering, color=TRUE, shade=TRUE,
         labels=2, lines=0, main = "Clusters identified by K-Medoids & Simple Match Making")

Model_2$medoids
Model_2$clusinfo
Model_2$objective
print(Model_2$silinfo$widths <0)
sum(Model_2$silinfo$widths[,3] <= 0)





Sim = dist.binary(USCD, method = 3)
Model_3 = pam(Sim, 2, diss = "TRUE")
plot(Model_3)
clusplot(USCD, Model_3$clustering, color=TRUE, shade=TRUE,
         labels=2, lines=0, main = "Clusters identified by Sokal & Sneath")


Sim = dist.binary(USCD, method = 5)
Model_5 = pam(Sim, 2, diss = "TRUE")
plot(Model_5)
clusplot(USCD, Model_5$clustering, color=TRUE, shade=TRUE,
         labels=2, lines=0, main = "Clusters identified by Dice")

table(Model_3$clustering)


tab1 = table(Model_1$clustering, InitialClusters)
tab2 = table(Model_2$clustering, InitialClusters)
tab3 = table(Model_3$clustering, InitialClusters)
tab5 = table(Model_5$clustering, InitialClusters)

tab1.ca = classAgreement(tab1)
tab2.ca = classAgreement(tab2)
tab3.ca = classAgreement(tab3)
tab5.ca = classAgreement(tab5)


tab1.ca$rand
tab2.ca$rand
tab3.ca$rand
typeof(tab5.ca$rand)
classAgreement(tab3_5)
