
library(tidyverse)
library(dplyr)
library(readxl)
library(gplots)
bsData <- read_excel("C:\\Users\\rahul\\Documents\\572\\assignment3\\Assgt3_BathSoap_Data.xls", sheet = "DM_Sheet")

################################################################################################################
#-------------------------------------Purchase Behavior--------------------------------------------------------#
colnames(bsData)

#better to change the colNames which contain punctuation, space to _ (like affluence index,etc)
names(bsData) <- gsub("[[:punct:]]|\\s", "_", names(bsData))
colnames(bsData)
str(bsData)

#rename the data
bsd<- bsData

#for brLoyalty, calculate maxBr as max of purchase by different major brand (excl others)
bsd <- bsd %>% rowwise() %>%  mutate(maxBr=max(Br__Cd__57__144, Br__Cd__55, Br__Cd__272, Br__Cd__286, Br__Cd__24, Br__Cd__481, Br__Cd__352, Br__Cd__5))

######remove these variables as they are of no use now
bsd <- bsd %>% select(-c(Br__Cd__57__144, Br__Cd__55, Br__Cd__272, Br__Cd__286, Br__Cd__24, Br__Cd__481, Br__Cd__352, Br__Cd__5))

#some attributes cannot be considered as 'numeric'
bsd$Member_id <- as.factor(bsd$Member_id)
bsd$FEH <- as.factor(bsd$FEH)
bsd$SEX <- as.factor(bsd$SEX)
bsd$CHILD <- as.factor(bsd$CHILD)
bsd$CS <- as.factor(bsd$CS)


#convert this to dummies, since the values are not ordinal, and remove the '0' level dummy
bsd <-bsd %>% mutate(fehDummy=1) %>% pivot_wider(names_from = FEH, values_from = fehDummy, names_prefix = "FEH_", values_fill = list(fehDummy=0)) %>% dplyr::select(-FEH_0)
#bsd<- bsd %>% select(-FEH_0)  # can append this to the last line too

#explore MT
summary(bsd$MT)
#keep levels 0, 4, 5, 10, 17 as dummies, with 0 in the dummies indicating 'other'
bsd <- bsd %>% mutate(MT=if_else(MT %in% c(0, 4, 5, 10, 17),MT,-1))
bsd$MT <- as.factor(bsd$MT)
bsd <-bsd %>% mutate(mtDummy=1) %>% pivot_wider(names_from = MT, values_from = mtDummy, names_prefix = "MT_", values_fill = list(mtDummy=0)) 
bsd<- bsd %>% dplyr::select(- `MT_-1`)

#similarly for CHILD, leave out the level '5' for unknown
bsd <- bsd %>% mutate(mtChild=1) %>% pivot_wider(names_from = CHILD, values_from = mtChild, names_prefix = "CHILD_", values_fill = list(mtChild=0)) %>% dplyr::select(- CHILD_5) 

#Other variables ?
### SEX
bsd <- bsd %>% mutate(sexdummy=1) %>% pivot_wider(names_from = SEX, values_from = sexdummy, names_prefix = "SEX_", values_fill = list(sexdummy=0)) %>% dplyr::select(- SEX_0) 

###CS
bsd <- bsd %>% mutate(csdummy=1) %>% pivot_wider(names_from = CS, values_from = csdummy, names_prefix = "CS_", values_fill = list(csdummy=0)) %>% dplyr::select(- CS_2)

########kMeans clustering
library(factoextra)

#clustering on  purchase behavior varables
PURCHASE_BEHAVIOR <- c('No__of_Brands', 'Brand_Runs', 'Total_Volume', 'No__of__Trans','Vol_Tran','Value', 'Trans___Brand_Runs', 'Avg__Price', 'maxBr', 'Others_999')

x_purchase_behavior <- bsd
#Create a scaled dataset for clustering, and use this
xpb <- x_purchase_behavior %>% dplyr::select(PURCHASE_BEHAVIOR) %>% scale() 

######## Making a variable brand loyalty and excluding some variables after checking correlation between them.
### maxBr should be higher
### Others_999 should be lower
### Transaction per brand should be higher
### Number of brands should be lower
xpb <- as.data.frame(xpb)
xpb$brnd_lylty <- xpb$maxBr-xpb$Others_999+xpb$Trans___Brand_Runs-xpb$No__of_Brands
xpb <- xpb %>% select(-c(maxBr,Others_999,Trans___Brand_Runs,No__of_Brands))

### We have also removed Volume per transaction as it is not giving much information
## about high volume and high transaction customer
##removing value as it is directly correlated with volume and transactions
### removing brand_runs as it is flawed according to business logic
xpb <- xpb %>% select(-c(Vol_Tran,Value))
xpb <- xpb %>% select(-c(Brand_Runs))
  

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x_purchase_behavior <- x_purchase_behavior %>% select(-c(maxBr,Others_999,Trans___Brand_Runs,No__of_Brands,Vol_Tran,Value,Brand_Runs))
x_purchase_behavior$brnd_lylty <- xpb$brnd_lylty

############# k means
kmClus_pb<- xpb%>%kmeans(centers=3, nstart=20)
kmClus_pb$tot.withinss

###Davies-Bouldien index
kmClus_pb$tot.withinss/kmClus_pb$betweenss


#how many clusters is best
fviz_nbclust(xpb, kmeans, method = "wss")
#2  or 3
fviz_nbclust(xpb, kmeans, method = "silhouette")
#2

library(cluster)
dissimilar <- daisy(xpb)         
sil <- silhouette(kmClus_pb$cluster, dissimilar)
fviz_silhouette(sil)
###### 3 clusters reason
### size similar
#### affluence index-edu 
#### middle class and rich same behaviour in brand loyalty, however,
## different in purchase behaviour 

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pb, data=xpb)


#plotting for business interpretation... 
centres_km <- t(kmClus_pb$centers)
centres_km <- (centres_km - min(centres_km))/(max(centres_km)-min(centres_km))
matplot(y = centres_km, type = 'l', lty = 1, col = 1:5, cex = 1, xlab = "Purchase behavior", ylab = "mean", xaxt="n")
legend(legend = colnames(centres_km), x = "topright", y = "topright", lty = 1, lwd = 2, col = 1:5)
angleAxis(1, labels = c('Total_Volume', 'No__of__Trans', 'Avg__Price', 'brnd_lylty'), at = 1:9, srt = 90, xpd = TRUE, cex = 0.7)

##### analyzing with demographics
x_purchase_behavior <- x_purchase_behavior %>% mutate(clusKM=kmClus_pb$cluster)
x_purchase_behavior %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX_1','SEX_2', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', 'Total_Volume', 'No__of__Trans', 'Avg__Price','brnd_lylty'), mean) %>% view()

#####################Hierarchical clustering
##Dissimilarity measure - correlation or euclidean ## correlation better
###linkage - complete/single/average/ward.d ## ward.d
### optimal cut - 3

#just using the agnes
xdist <- dist(xpb ,method = "euclidean")

#check the agglomerative coeff given by agnes
# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(xdist, method = x)$ac
}
map_dbl(m, ac)

#### checking with correlation distance
xdist <- get_dist(xpb ,method = "pearson")
map_dbl(m, ac)

##### pearson correlation (dissimilarity measure) is better
##### linkage method - ward is better
###proceeding with them
hierC_pb_ag_w <- agnes(xdist, method = "ward" )


#Evaluating the agnes model.
fviz_nbclust(xpb, FUN = hcut, method = "wss", 
             k.max = 10) +
  ggtitle("Elbow method")
fviz_nbclust(xpb, FUN = hcut, method = "silhouette", 
             k.max = 10) +
  ggtitle("Silhouette method")

#evaluating agnes using silhouette plot.
library(cluster)
dissimilar <- daisy(xpb)         
sil <- silhouette(cut_hierC_pb_ag_w, dissimilar)
fviz_silhouette(sil)


#####going with 3 clusters as not much difference in silhoutte
### whereas a lot of difference in within variation
#use cuttree to assign different clusters to examples
cut_hierC_pb_ag_w <- cutree(hierC_pb_ag_w, k = 3)#3
table(cut_hierC_pb_ag_w)

#DB Index
library(clusterSim)
print(index.DB(xpb,cut_hierC_pb_ag_w, centrotypes="centroids"))


#### getting even - sized clusters
#### Important
x_purchase_behavior <- x_purchase_behavior %>% mutate(clus_aeg=cut_hierC_pb_ag_w)
x_purchase_behavior %>% group_by(clus_aeg) %>% summarise_at(c('EDU', 'Affluence_Index', 'Total_Volume', 'No__of__Trans', 'Avg__Price', 'brnd_lylty'), mean) %>% view()

#plotting for business interpretation... 
temp <- x_purchase_behavior  %>% group_by(clus_aeg) %>% summarise_at(c('Total_Volume', 'No__of__Trans', 'Avg__Price', 'brnd_lylty'), mean)
temp_ag <- temp[,-1] %>% scale()
centres_ag <- t(temp_ag)
colnames(centres_ag) <- t(temp[,1])
centres_ag <- (centres_ag - min(centres_ag))/(max(centres_ag)-min(centres_ag)) #scaling the centers to make them between 0 and 1
matplot(y = centres_ag, type = 'l', lty = 1, col = 1:4, cex = 1, xlab = "Purchase behavior", ylab = "mean", xaxt="n")
legend(legend = colnames(centres_ag), x = "topright", y = "topright", lty = 1, lwd = 2, col = 1:5)
angleAxis(1, labels = c('Total_Volume', 'No__of__Trans', 'Avg__Price', 'brnd_lylty'), at = 1:9, srt = 90, xpd = TRUE, cex = 0.7)

####visualization
fviz_cluster(list(data=xpb,cluster=cut_hierC_pb_ag_w), main="agnes-wards")
#dendograms using fviz_dend
fviz_dend(hierC_pb_ag_w, k=3, color_labels_by_k = FALSE, rect=TRUE, main="agnes - Wards")


###################################DBScan
library(dbscan)

#eps mentions the neighbourhood size.
msDbscan<- dbscan(xpb, eps = 0.75, MinPts = 6) #optimum k=3  eps= 0.75, minpts = 6
msDbscan

#optimal eps value
dbscan::kNNdistplot(xpb, k =  4)
abline(h = 0.75, lty = 2)


msDbscan$cluster <- msDbscan$cluster + 1   #changed cluster 0,1,2 to 1,2,3.

#important for interpretation
x_purchase_behavior <- x_purchase_behavior %>% mutate(clus_dbs=msDbscan$cluster)
x_purchase_behavior %>% group_by(clus_dbs) %>% summarise_at(c('EDU', 'Affluence_Index', 'Total_Volume', 'No__of__Trans', 'Avg__Price', 'brnd_lylty'), mean) %>% view()

#optimal eps value for a given value of k.
temp <- x_purchase_behavior %>% group_by(clus_dbs) %>% summarise_at(c('Total_Volume', 'No__of__Trans', 'Avg__Price', 'brnd_lylty'), mean)
temp_dbs <- temp[,-1] %>% scale()
centres_dbs <- t(temp_dbs)
colnames(centres_dbs) <- t(temp[,1])
centres_dbs <- (centres_dbs - min(centres_dbs))/(max(centres_dbs)-min(centres_dbs)) #scaling the centers to make them between 0 and 1
matplot(y = centres_dbs, type = 'l', lty = 1, col = 1:4, cex = 1, xlab = "Purchase behavior", ylab = " ", xaxt="n")
legend(legend = colnames(centres_dbs), x = "topright", y = "topright", lty = 1, lwd = 2, col = 1:5)
angleAxis(1, labels = c('Total_Volume', 'No__of__Trans', 'Avg__Price', 'brnd_lylty'), at = 1:9, srt = 90, xpd = TRUE, cex = 0.7)

#silhouette plot
library(cluster)
dissimilar <- daisy(xpb)         
sil <- silhouette(msDbscan$cluster, dissimilar)
fviz_silhouette(sil)

#-----------------------------------------------------------------------------------------------#
######################################## Basis for purchase######################################

#clustering on basis of purchase variables
###purchase by promotion, price categories, selling propositions
x_basis_pur <- bsd

#Categorizing the prop_cat5,8,9,10,11,13 under beauty
x_basis_pur <- x_basis_pur %>% mutate(PropCat_beauty = PropCat_5+PropCat_9+PropCat_10+PropCat_11+PropCat_13+PropCat_8) %>% select(-c('PropCat_5','PropCat_9','PropCat_10','PropCat_11','PropCat_13','PropCat_8'))
#Categorizing the prop_cat6,7,12,14 under health
x_basis_pur <- x_basis_pur %>% mutate(PropCat_health = PropCat_6+PropCat_7+PropCat_12+PropCat_14) %>% select(-c('PropCat_6','PropCat_7','PropCat_12','PropCat_14'))
#Categorizing the prop_cat15 under others
x_basis_pur <- x_basis_pur %>% mutate(PropCat_other = PropCat_15) %>% select(-c('PropCat_15'))

######on the basis of analyzing, we can remove following variables with reasoning
##given below:
##PropCat_health - Pr_cat_3 has same definition
##PropCat_other - Pr_cat_other gives no information as such
##No Promo is 90% of data so is useless
## Other promotions are 3% and giving no information

##basis of purchase
BASIS_OF_PURCHASE <- c('Pur_Vol_Promo_6__','Pr_Cat_1', 'Pr_Cat_2',
                       'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty')

#Create a scaled dataset for clustering, and use this
xbop <- x_basis_pur %>% dplyr::select(BASIS_OF_PURCHASE) %>% scale() 

###########business logic
### basically price cat
#ANY PREMIUM SOAPS			3	rich	128 - promotions only 3%
#ANY POPULAR SOAP			2	middle	332 - anyways (ignore)
#ANY ECONOMY/CARBOLIC			1	poor	83 - image change
#ANY SUB-POPULAR			4	lower middle	57 - keep on selling banded

kmClus_bop<- xbop%>%kmeans(centers=4, nstart=30)
kmClus_bop$tot.withinss

###Davies-Bouldien index
kmClus_pb$tot.withinss/kmClus_pb$betweenss

#how many clusters is best
fviz_nbclust(xbop, kmeans, method = "wss")
#4
fviz_nbclust(xbop, kmeans, method = "silhouette")
#4
#####but merging them into 4 as two had similar traits
#library(cluster)
dissimilar <- daisy(xbop)         
sil <- silhouette(kmClus_bop$cluster, dissimilar)
fviz_silhouette(sil)

#clusters
#fviz_cluster(kmClus_bop, data=xbop)

#plotting for business interpretation...
centre_km1 <- t(kmClus_bop$centers)
centre_km1 <- (centre_km1 - min(centre_km1))/(max(centre_km1)-min(centre_km1)) #normalized the centers to make them between 0 and 1
row_names_cluster <- rownames(centre_km1)
matplot(y = centre_km1, type = 'l',cex=0.3,xlab = "basis of purchase", ylab = "mean" , lty = 1, col = 1:6, xaxt="n")
legend(legend = colnames(centre_km1), x = "topright", y = "topright", lty = 1, lwd = 1, col = 1:6)
angleAxis(1, labels = c('Pur_Vol_Promo_6__','Pr_Cat_1', 'Pr_Cat_2',
                        'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty'), at = 1:10, srt = 90, xpd = TRUE, cex = 0.7)


#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x_basis_pur <- x_basis_pur %>% mutate(clusKM=kmClus_bop$cluster)


#### Important analysis
x_basis_pur %>% group_by(clusKM_bop) %>% summarise_at(c('SEC','HS', 'SEX_1','SEX_2', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 
                                                        'CHILD_4', 'Pur_Vol_No_Promo____', 'Pur_Vol_Promo_6__',
                                                        'Pur_Vol_Other_Promo__', 'Pr_Cat_1', 'Pr_Cat_2',
                                                        'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty', 'PropCat_health', 'PropCat_other'), mean) %>% view()

##########################Hierarchical clustering#########################################

#just using the agnes
xdist_bop <- dist(xbop ,method = "euclidean")

#check the agglomerative coeff given by agnes
# function to compute coefficient
ac <- function(x) {
  agnes(xdist_bop, method = x)$ac
}
map_dbl(m, ac)
#### ward is better
#### checking with correlation distance
xdist_bop <- get_dist(xbop ,method = "pearson")
map_dbl(m, ac)
##### pearson correlation (dissimilarity measure) is better
##### linkage method - ward is better
###proceeding with them
hierC_bop_ag_w <- agnes(xdist_bop, method = "ward" )

#evaluating agnes
fviz_nbclust(xbop, FUN = hcut, method = "wss", 
             k.max = 10) +
  ggtitle("Elbow method")
##4
fviz_nbclust(xbop, FUN = hcut, method = "silhouette", 
             k.max = 10) +
  ggtitle("Silhouette method")
##3



### 4 makes sense as sub-popular segment is also getting segregated
#use cuttree to assign different clusters to examples
cut_hierC_bop_ag_w <- cutree(hierC_bop_ag_w, k = 4)
table(cut_hierC_bop_ag_w)
library(clusterSim)
print(index.DB(xbop,cut_hierC_bop_ag_w, centrotypes="centroids"))


library(cluster)
dissimilar <- daisy(xbop)         
sil <- silhouette(cut_hierC_bop_ag_w, dissimilar)
fviz_silhouette(sil)

#### Important
x_basis_pur <- x_basis_pur %>% mutate(clus_aeg=cut_hierC_bop_ag_w)
x_basis_pur %>% group_by(clus_aeg) %>% summarise_at(c('EDU', 'Affluence_Index','AGE','Pur_Vol_Promo_6__',
                                                      'Pr_Cat_1', 'Pr_Cat_2',
                                                      'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty'), mean) %>% view()

#### getting even - sized clusters
#plotting for business interpretation...
temp_2 <- x_basis_pur  %>% group_by(clus_aeg) %>% summarise_at(c('Pur_Vol_Promo_6__',
                                                                 'Pr_Cat_1', 'Pr_Cat_2','Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty'), mean)
temp_ag_2 <- temp_2[,-1] %>%scale()
centres_ag_2 <- t(temp_ag_2)
colnames(centres_ag_2) <- t(temp_2[,1])
centres_ag_2 <- (centres_ag_2 - min(centres_ag_2))/(max(centres_ag_2)-min(centres_ag_2)) #scaling the centers to make them between 0 and 1
matplot(y = centres_ag_2, type = 'l', lty = 1, col = 1:4, cex = 1, xlab = "Basis of Purchase", ylab = "mean", xaxt="n")
legend(legend = colnames(centres_ag_2), x = "topright", y = "topright", lty = 1, lwd = 2, col = 1:5)
angleAxis(1, labels = c('Pur_Vol_Promo_6__','Pr_Cat_1', 'Pr_Cat_2',
                        'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty'), at = 1:9, srt = 90, xpd = TRUE, cex = 0.7)


####visualization
fviz_cluster(list(data=xbop,cluster=cut_hierC_bop_ag_w), main="agnes-wards")
#dendograms using fviz_dend
fviz_dend(hierC_bop_ag_w, k=4, color_labels_by_k = FALSE, rect=TRUE, main="agnes - Wards")

###################Kernel k-means
library(dbscan)

#eps mentions the neighbourhood size.
msDbscan<- dbscan(xbop, eps = 0.69, MinPts = 8) #optimum k=4  eps= 0.69, minpts = 8
msDbscan

#optimal eps value
dbscan::kNNdistplot(xbop, k =  4)
abline(h = 0.69, lty = 2)


msDbscan$cluster <- msDbscan$cluster + 1   #changed cluster 0,1,2,3 to 1,2,3,4

x_basis_pur <- x_basis_pur %>% mutate(clus_dbs=msDbscan$cluster)
x_basis_pur %>% group_by(clus_dbs) %>% summarise_at(c('EDU', 'Affluence_Index','AGE','Pur_Vol_Promo_6__', 'Pr_Cat_1', 'Pr_Cat_2', 'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty'), mean) %>% view()

#optimal eps value for a given value of k.
temp <- x_basis_pur %>% group_by(clus_dbs) %>% summarise_at(c('Pur_Vol_Promo_6__',
                                                              'Pr_Cat_1', 'Pr_Cat_2',
                                                              'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty'), mean)
temp_dbs1 <- temp[,-1] %>% scale()
centres_dbs1 <- t(temp_dbs1)
colnames(centres_dbs1) <- t(temp[,1])
centres_dbs1 <- (centres_dbs1 - min(centres_dbs1))/(max(centres_dbs1)-min(centres_dbs1)) #scaling the centers to make them between 0 and 1
matplot(y = centres_dbs1, type = 'l', lty = 1, col = 1:4, cex = 1, xlab = "Basis of purchase", ylab = "mean ", xaxt="n")
legend(legend = colnames(centres_dbs1), x = "topright", y = "topright", lty = 1, lwd = 2, col = 1:5)
angleAxis(1, labels = c('Pur_Vol_Promo_6__',
                        'Pr_Cat_1', 'Pr_Cat_2',
                        'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty'), at = 1:9, srt = 90, xpd = TRUE, cex = 0.7)

#silhouette plot
library(cluster)
dissimilar <- daisy(xpb)         
sil <- silhouette(msDbscan$cluster, dissimilar)
fviz_silhouette(sil)
fviz_nbclust(xpb, dbscan, method = "wss")
fviz_nbclust(xbop, dbscan, method = "wss")
#############################################################################################################
#-----------------------------------Both purchase behavior and basis of purchase----------------------------#

x_both_segment <- x_purchase_behavior %>% select(-c(clusKM,clus_aeg))

#Categorizing the prop_cat5,8,9,10,11,13 under beauty
x_both_segment <- x_both_segment %>% mutate(PropCat_beauty = PropCat_5+PropCat_9+PropCat_10+PropCat_11+PropCat_13+PropCat_8) %>% select(-c('PropCat_5','PropCat_9','PropCat_10','PropCat_11','PropCat_13','PropCat_8'))
#Categorizing the prop_cat6,7,12,14 under health
x_both_segment <- x_both_segment %>% mutate(PropCat_health = PropCat_6+PropCat_7+PropCat_12+PropCat_14) %>% select(-c('PropCat_6','PropCat_7','PropCat_12','PropCat_14'))
#Categorizing the prop_cat15 under others
x_both_segment <- x_both_segment %>% mutate(PropCat_other = PropCat_15) %>% select(-c('PropCat_15'))


#clustering on  purchase behavior varables
PURCHASE_BEHAVIOR_AND_BASIS <- c('Total_Volume', 'No__of__Trans','Avg__Price','brnd_lylty',
                                 'Pur_Vol_Promo_6__','Pr_Cat_1', 'Pr_Cat_2',
                                 'Pr_Cat_3', 'Pr_Cat_4', 'PropCat_beauty')


#Create a scaled dataset for clustering, and use this
xboth <- x_both_segment %>% dplyr::select(PURCHASE_BEHAVIOR_AND_BASIS) %>% scale() 

#how many clusters is best
fviz_nbclust(xboth, kmeans, method = "wss")
#4
fviz_nbclust(xboth, kmeans, method = "silhouette")
#4

########kMeans clustering
kmClus_both<- xboth%>%kmeans(centers=4, nstart=20)
kmClus_both$tot.withinss

library(cluster)
dissimilar <- daisy(xboth)         
sil <- silhouette(kmClus_both$cluster, dissimilar)
fviz_silhouette(sil)


#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_both, data=xboth)

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
x_both_segment <- x_both_segment %>% mutate(clusKM=kmClus_both$cluster)

x_both_segment %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX_1','SEX_2', 'EDU', 'Affluence_Index','AGE', 'CHILD_1', 'CHILD_2', 'CHILD_3', 'CHILD_4', PURCHASE_BEHAVIOR_AND_BASIS), mean) %>% view()

#plotting for business interpretation... 
centres_km3<- t(kmClus_both$centers)
centres_km3 <- (centres_km3 - min(centres_km3))/(max(centres_km3)-min(centres_km3))
matplot(y = centres_km3, type = 'l', lty = 1, col = 1:6, cex = 1, xlab = "Purchase behavior and basis", ylab = " ", xaxt="n")
legend(legend = colnames(centres_km3), x = "topright", y = "topright", lty = 1, lwd = 2, col = 1:5)
angleAxis(1, labels = c(colnames(xboth)), at = 1:10, srt = 90, xpd = TRUE, cex = 0.7)



#####################Hierarchical clustering
##Dissimilarity measure - correlation or euclidean ## correlation better
###linkage - complete/single/average/ward.d ## ward.d
### optimal cut - 3

#just using the agnes
xdist_b <- get_dist(xboth ,method = "euclidean")
hierC_both_ag_w <- agnes(xdist_b, method = "ward" )

#check the agglomerative coeff given by agnes
# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(xdist_b, method = x)$ac
}
map_dbl(m, ac)


#Evaluating the agnes model.
fviz_nbclust(xboth, FUN = hcut, method = "wss", 
             k.max = 10) +
  ggtitle("Elbow method")
##4
fviz_nbclust(xboth, FUN = hcut, method = "silhouette", 
             k.max = 10) +
  ggtitle("Silhouette method")
##2 and 4
### 2 is too less
#use cuttree to assign different clusters to examples
cut_hierC_both_ag_w <- cutree(hierC_both_ag_w, k = 5)
table(cut_hierC_both_ag_w)

#DB index
library(clusterSim)
print(index.DB(xboth,cut_hierC_both_ag_w, centrotypes="centroids"))

#### Important analysis
x_both_segment <- x_both_segment %>% mutate(clus_aeg=cut_hierC_both_ag_w)
x_both_segment %>% group_by(clus_aeg) %>% summarise_at(c('EDU', 'Affluence_Index', PURCHASE_BEHAVIOR_AND_BASIS), mean) %>% view()

#plotting for business interpretation... #need to work on how to plot this
temp_3 <- x_both_segment  %>% group_by(clus_aeg) %>% summarise_at(c(PURCHASE_BEHAVIOR_AND_BASIS), mean)
temp_ag_3 <- temp_3[,-1] %>% scale()
centres_ag_3 <- t(temp_ag_3)
colnames(centres_ag_3) <- t(temp_3[,1])
centres_ag_3 <- (centres_ag_3 - min(centres_ag_3))/(max(centres_ag_3)-min(centres_ag_3)) #scaling the centers to make them between 0 and 1
matplot(y = centres_ag_3, type = 'l', lty = 1, col = 1:5, cex = 1, xlab = "Purchase behavior and basis", ylab = "mean", xaxt="n")
legend(legend = colnames(centres_ag_3), x = "topright", y = "topright", lty = 1, lwd = 1, col = 1:5)
angleAxis(1, labels = c(PURCHASE_BEHAVIOR_AND_BASIS), at = 1:10, srt = 90, xpd = TRUE, cex = 0.7)

####visualization
fviz_cluster(list(data=xboth,cluster=cut_hierC_both_ag_w), main="agnes-wards")
#dendograms using fviz_dend
fviz_dend(hierC_both_ag_w, k=3, color_labels_by_k = FALSE, rect=TRUE, main="agnes - Wards")


##############Kernel k-means
library(dbscan)

#eps mentions the neighbourhood size.
msDbscan<- dbscan(xboth, eps = 2, MinPts = 11) #optimum k=3  eps= 1.5, minpts = 15
msDbscan

#optimal eps value
dbscan::kNNdistplot(xboth, k =  3)
abline(h = 1.5, lty = 2)


msDbscan$cluster <- msDbscan$cluster + 1   #changed cluster 0,1,2 to 1,2,3.

x_both_segment <- x_both_segment %>% mutate(clus_dbs=msDbscan$cluster)
x_both_segment %>% group_by(clus_dbs) %>% summarise_at(c('EDU', 'Affluence_Index', PURCHASE_BEHAVIOR_AND_BASIS), mean) %>% view()

#optimal eps value for a given value of k.
temp <- x_both_segment %>% group_by(clus_dbs) %>% summarise_at(c(PURCHASE_BEHAVIOR_AND_BASIS), mean)
temp_dbs2 <- temp[,-1] %>% scale()
centres_dbs2 <- t(temp_dbs2)
colnames(centres_dbs2) <- t(temp[,1])
centres_dbs2 <- (centres_dbs2 - min(centres_dbs2))/(max(centres_dbs2)-min(centres_dbs2)) #scaling the centers to make them between 0 and 1
matplot(y = centres_dbs2, type = 'l', lty = 1, col = 1:4, cex = 1, xlab = "Purchase behavior and basis", ylab = "mean", xaxt="n")
legend(legend = colnames(centres_dbs2), x = "topright", y = "topright", lty = 1, lwd = 2, col = 1:5)
angleAxis(1, labels = c(PURCHASE_BEHAVIOR_AND_BASIS), at = 1:10, srt = 90, xpd = TRUE, cex = 0.7)

#silhouette plot
library(cluster)
dissimilar <- daisy(xpb)         
sil <- silhouette(msDbscan$cluster, dissimilar)
fviz_silhouette(sil)
####################################################################################################################
#Mutating the x3 clusters to x1 and x2
x <- x3 %>% mutate(clusKM_both = x3$clusKM_both,clus_aeg_b = x3$clus_aeg_b,clus_kkm_b=x3$clus_kkm_b)
x1 <- x3 %>% mutate(clusKM_both = x3$clusKM_both,clus_aeg_b = x3$clus_aeg_b,clus_kkm_b=x3$clus_kkm_b)
####################################################################################################################

################## Decision Trees #################################
library(rpart)
nr<-nrow(x_basis_pur)

set.seed(22)
trnIndex<- sample(1:nr, size = round(0.70*nr), replace=FALSE)
x_trn <- x_basis_pur[trnIndex, ]
x_tst <- x_basis_pur[-trnIndex, ]
dim(x_trn)
dim(x_tst)

km_dt <- rpart(clusKM ~., data=x_trn %>% select(-c(Member_id,clus_aeg)), method="class", parms = list(split = "gini"), control = rpart.control(cp = 0.0, minsplit = 50))


#check for performance with dfferent cp levels
printcp(km_dt)
plotcp(km_dt)
### pick cp on the basis of plot
kmp_r<- prune.rpart(km_dt, cp=0.0004)

#variable importance
kmp_r$variable.importance

#Performance evaluation
#Evaluate performance
predTrn=predict(kmp_r,x_trn, type='class')
table(pred = predTrn, true=x_trn$clusKM)
mean(predTrn == x_trn$clusKM)

table(pred = predict(kmp_r,x_tst, type='class'), true=x_tst$clusKM)
mean(predict(kmp_r,x_tst, type='class') ==x_tst$clusKM)

#### hierarchical
aeg_dt <- rpart(clus_aeg ~., data=x_trn %>% select(-c(Member_id,clusKM)), method="class", parms = list(split = "gini"), control = rpart.control(cp = 0.0, minsplit = 50))


#check for performance with dfferent cp levels
printcp(aeg_dt)
plotcp(aeg_dt)
### pick cp on the basis of plot
aegp_r<- prune.rpart(aeg_dt, cp=0.14)

#variable importance
aegp_r$variable.importance

#Performance evaluation
#Evaluate performance
predTrn=predict(aegp_r,x_trn, type='class')
table(pred = predTrn, true=x_trn$clus_aeg)
mean(predTrn == x_trn$clus_aeg)

table(pred = predict(aegp_r,x_tst, type='class'), true=x_tst$clus_aeg)
mean(predict(aegp_r,x_tst, type='class') ==x_tst$clus_aeg)

