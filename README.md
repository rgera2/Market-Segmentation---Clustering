# Market-Segmentation---Clustering
# Introduction
CRISA wanted to segment the market based on two key sets of variables more directly related to the purchase process and to brand loyalty:
1. Purchase behavior (volume, frequency, susceptibility to discounts, and brand loyalty)
2. Basis of purchase (price, selling proposition)
3. Both

Doing so would allow CRISA to gain information about what demographic attributes are associated with different purchase behaviors and degrees of brand loyalty, and more effectively deploy promotion budgets.

The goal is to do an effective market segmentation that would enable CRISA’s clients to design more cost-effective promotions targeted at appropriate segments.

# Data
Data file – Assgt3_BathSoap_Data.xls | Each row contains the data for one household. Two additional datasets were used in the derivation of the summary data.
The Durables sheet in the data file contains information used to calculate the affluence index. Each row corresponds to a household, and each column represents a durable consumer good. A 1 in a column indicates that the durable is possessed by the household; a 0 indicates that it is not possessed. This value is multiplied by the weight assigned to the durable item. The sum of all the weighted values of the durables possessed gives the affluence index.

# Data Preprocessing
1. Appropriate data type conversion according to business sense
2. Removal of correlated variables
3. Feature Engineering

# Models
1. K-means - Parameters used: k and nstart
2. Agglomerate Hierarchical Algorithm(agnes) - Dissimilarity parameters used: Euclidean and Pearson | Linkage parameters used: Average, Single, Complete and Ward
3. DBScan algorithm - Parameters used: eps(size of neighbourhood) and minpts(core points)

# Evaluation metrics
1. For K-means, Accuracy was compared and optimum number of clusters were calculated by - elbow method, silhouette method, DB index and cluster size trade-off
2. For agnes, same as above 
3. For DbScan - Accuracy was compared and KNN distplot was used to calculate optimum number of clusters.

# Conclusion
1. Clusters obtained from K-means and Aegnes were similar. However, the clusters obtained from DBScan were not reliable as it was sensitive to clustering parameters.
2. Accuracy was highest for K-means when both the sets of variables (Purchase behaviour and Basis of purchase) were used.
3. We also confirmed the interpretations of our clusters by building a decision tree on our best segmentation and k-means algorithm. This was because the important variables which came out of decision trees were either the same or having similar correlation as what we observed in the line graphs in k-means sections. However, decision trees cannot distinguish between correlated variables and they can have equally higher importance which is not the case usually when we make the clusters using business acumen.
