#REQUIRED PACKAGES
library(readxl)
library(dplyr)
library(writexl)
library(corrplot)
library(factoextra)
library(REdaS)
library(plotly)
library(scales)
library(fastDummies)
library(factoextra)
library(amap)
library(pheatmap)
library(tree)
library(randomForest)
library(ggplot2)
library(dplyr)
library(RANN)
library(caret)
library(ranger)
library(rpart.plot)
library(rpart, lib.loc = "C:/Program Files/R/R-4.0.5/library")
library(rattle)


#IMPORTING DATASETS
economicfreedom <- read_excel("C:/Users/lucad/OneDrive/Desktop/Stat Learn Project/Datasets/economicfreedom.xlsx")
freedomhouse <- read_excel("C:/Users/lucad/OneDrive/Desktop/Stat Learn Project/Datasets/freedomhouse.xlsx")
climate <- read_excel("C:/Users/lucad/OneDrive/Desktop/Stat Learn Project/Datasets/climate.xlsx")
coastline <- read_excel("C:/Users/lucad/OneDrive/Desktop/Stat Learn Project/Datasets/coastline.xlsx")
colonies <- read_excel("C:/Users/lucad/OneDrive/Desktop/Stat Learn Project/Datasets/colonies.xlsx")
etnic_fractionalization <- read_excel("C:/Users/lucad/OneDrive/Desktop/Stat Learn Project/Datasets/etnic_fractionalization.xlsx")
manzi <- read_excel("C:/Users/lucad/OneDrive/Desktop/Stat Learn Project/Datasets/region_legal.xlsx")
natural_resources_revenue <- read_excel("C:/Users/lucad/OneDrive/Desktop/Stat Learn Project/Datasets/natural_resources_revenue.xls")
religious_diversity_index <- read_excel("C:/Users/lucad/OneDrive/Desktop/Stat Learn Project/Datasets/religious_diversity_index.xlsx")
communism <- read_excel("C:/Users/lucad/OneDrive/Desktop/Stat Learn Project/Datasets/communism.xlsx")

#CREATING THE COMPLETE FREEDOM DATASET: merging, missing values, normalization
freedom <- merge(freedomhouse, economicfreedom, by="country") 

dataset_imp_free<-freedom[,2:38]
preProcValues_free <- preProcess(dataset_imp_free,method = c("knnImpute"),
                                 k = 5,
                                 knnSummary = mean)
impute_dataset_free <- predict(preProcValues_free, dataset_imp_free,na.action = na.pass)

procNames_free <- data.frame(col = names(preProcValues_free$mean), mean = preProcValues_free$mean, sd = preProcValues_free$std)
for(i in procNames_free$col){
  impute_dataset_free[i] <- impute_dataset_free[i]*preProcValues_free$std[i]+preProcValues_free$mean[i] 
}

freedom_completo<-data.frame(freedom[,1],impute_dataset_free)
colnames(freedom_completo)[1]<-"country"

freedom_completo[,2:38] <- scale(freedom_completo[,2:38])
freedom_norm<- freedom_completo[,1:38]
summary(freedom_norm)

#CREATING THE COMPLETE DATASET: merging "freedom" with all the other datasets
dataset2 <- merge(freedom_norm, climate, by="country", all.x = TRUE)
dataset3 <- merge(dataset2, coastline, by="country", all.x = TRUE)
dataset4 <- merge(dataset3, colonies, by="country", all.x = TRUE)
dataset5 <- merge(dataset4, etnic_fractionalization, by="country", all.x = TRUE)
dataset6 <- merge(dataset5, manzi, by="country", all.x = TRUE)
dataset7 <- merge(dataset6, natural_resources_revenue, by="country", all.x = TRUE)
dataset8 <- merge(dataset7, religious_diversity_index, by="country", all.x = TRUE)
dataset9 <- merge(dataset8, communism, by="country", all.x = TRUE)

alldata<-dataset9

#UNSUPERVISED LEARNING

#CORRELOGRAM: short names, correlation matrix, correlogram plot
short_names<-c("country","p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12","c13","c14","c15","e1","e2","e3","e4","e5","e6","e7","e8","e9","e10","e11","e12")
freedom_norm_short<-freedom_norm
colnames(freedom_norm_short)<-short_names
dataset_corr = cor(freedom_norm_short[,2:38])
corrplot(dataset_corr)

#HIERARCHICAL CLUSTERING
freedom_norm_clust <-freedom_norm[,2:38]
rownames(freedom_norm_clust)<-freedom_norm[,1]

#EUCLIDEAN DISTANCE, COMPLETE LINKAGE
euc_distances <- Dist(freedom_norm_clust, method="euclidean")

hc_output_euc<- hclust(euc_distances, method="complete")
plot(hc_output_euc)
rect.hclust(hc_output_euc, k=6)

freedom_clusters <- cutree(hc_output_euc, k=6)
fviz_cluster(list(data=freedom_norm_clust,cluster=freedom_clusters),repel = TRUE)

pheatmap(freedom_norm_clust, scale = "row",clustering_distance_rows="euclidean")

#MANHATTAN DISTANCE, COMPLETE LINKAGE
man_distances <- Dist(freedom_norm_clust, method="manhattan")

hc_output_man<- hclust(man_distances, method="complete")
plot(hc_output_man)
rect.hclust(hc_output_man, k=6)

freedom_clusters_man <- cutree(hc_output_man, k=6)
fviz_cluster(list(data=freedom_norm_clust,cluster=freedom_clusters_man),repel = TRUE)

pheatmap(freedom_norm_clust, scale = "row",clustering_distance_rows="manhattan")

#CORRELATION DISTANCE, COMPLETE LINKAGE
cor_distances <- Dist(freedom_norm_clust, method="correlation")

hc_output_cor<- hclust(cor_distances, method="complete")
plot(hc_output_cor)
rect.hclust(hc_output_cor, k=3)

freedom_clusters_cor <- cutree(hc_output_cor, k=3)
fviz_cluster(list(data=freedom_norm_clust,cluster=freedom_clusters_cor),repel = TRUE)

pheatmap(freedom_norm_clust, scale = "row",clustering_distance_rows="correlation")


#PRINCIPAL COMPONENT ANALYSIS
freedom_norm_pca<-freedom_norm[,2:38]
rownames(freedom_norm_pca)<-freedom_norm[,1]

#CHECK DATASET SUITABILITY FOR PCA
#Kaiser-Meyer-Olkin (KMO) criterium
KMOS(freedom_norm_pca)
#Bartlett's test of sphericity
bart_spher(freedom_norm_pca)

#PCA IMPLEMENTATION WITHOUT PREDEFINED FUNCTIONS
n<-nrow(freedom_norm_pca)
p<-ncol(freedom_norm_pca)
cor_PCA<-cor(freedom_norm_pca)
eigenval<-eigen(cor_PCA)$values
eigenvec<-eigen(cor_PCA)$vectors

#SCREEDIAGRAM 
eigen_dataframe<-data.frame(c(1:p),eigenval)
eigen_plot <- ggplot(data = eigen_dataframe, aes(x=eigen_dataframe[,1], y = eigen_dataframe[,2]))+
  geom_point(size=2)+
  geom_hline(yintercept = 1, color = "green", size= 1)+
  labs(x="Number of Components",y="Eigenvalue")+ ggtitle("SCREE PLOT")
eigen_plot

#VARIANCE EXPLAINED
eigen_power<- eigenval/p
eigenpower_dataframe <- data.frame(c(1:p),cumsum(eigen_power))
power_plot <- ggplot(data = eigenpower_dataframe, aes(x=eigenpower_dataframe[,1], y = eigenpower_dataframe[,2]))+
  geom_point(size=2)+
  geom_hline(yintercept = 0.8, color = "green", size= 1)+
  labs(x="Number of Components",y="% of Variance Explained")+ ggtitle("VARIANCE EXPLAINED")
power_plot

#LOADINGS
comp<-round(cbind(eigenvec[,1]*sqrt(eigenval[1]),eigenvec[,2]*sqrt(eigenval[2]),eigenvec[,3]*sqrt(eigenval[3])),3)
rownames(comp)<-colnames(freedom_norm[,2:38])
colnames(comp)<-c("Comp1","Comp2","Comp3")
comp_var<-rbind(comp,eigen_power[1:3])
rownames(comp_var)[38]<-"% of VAR explained"
comp_var<-data.frame(rownames(comp_var),comp_var)
comp_var

#COMMUNALITIES for the three principal components
communality<-comp[1:37,1]^2+comp[1:37,2]^2+comp[1:37,3]^2
comp_comu<-cbind(comp[1:37,],communality)
comp_comu<- data.frame(rownames(comp_comu),comp_comu[,4])
comp_comu

#COMMUNALITIES for the first principal component
communality_first<-comp[1:37,1]^2
communality_first<-data.frame(communality_first)
communality_first

#PCA REPRESENTATIONS
scores_groups <- as.matrix(freedom_norm_pca)%*%as.matrix(eigenvec[,1:3])
scores2_groups<-cbind(scores_groups[,1]/sqrt(eigenval[1]),scores_groups[,2]/sqrt(eigenval[2]),scores_groups[,3]/sqrt(eigenval[3]))



a<- list(
  family = "sans serif",
  size = 14,
  color = "black")

#2-DIMENSIONAL
PCAplot_2d<-plot_ly(x=scores2_groups[,1], y=scores2_groups[,2], mode="markers", text = rownames(scores2_groups))
PCAplot_2d <- PCAplot_2d %>% add_markers(size=1)
PCAplot_2d <- PCAplot_2d %>% add_text(textfont = a, textposition = "top right")
PCAplot_2d

#3-DIMENASIONAL
PCAplot_3d<-plot_ly(x=scores2_groups[,1], y=scores2_groups[,2], z=scores2_groups[,3], type="scatter3d", mode="markers", text = rownames(scores2_groups))
PCAplot_3d <- PCAplot_3d %>% add_markers(size=1)
PCAplot_3d <- PCAplot_3d %>% add_text(textfont = a, textposition = "top right")
PCAplot_3d

#NEW VARIABLE CREATION: "overall_freedom" 

scalednewvar <- rescale(scores2_groups[,1], to = c(0, 100))
overall_freedom <- (100-scalednewvar)


#1-DIMENSIONAL PLOT: country ranking
PCAplot_1d<-plot_ly(x=0, y=(overall_freedom), mode="markers", text = rownames(scores2_groups))
PCAplot_1d <- PCAplot_1d %>% add_markers(size=1)
PCAplot_1d <- PCAplot_1d %>% add_text(textfont = a, textposition = "top right")
PCAplot_1d

#DATASET FOR SUPERVISED LEARNING CREATION
dataset_supervised <- data.frame(freedom_norm[,1],overall_freedom,alldata[,39:71])
dataset_supervised <- dummy_cols(dataset_supervised, select_columns = 'region')
dataset_supervised <- dummy_cols(dataset_supervised, select_columns = 'legal')
dataset_supervised<-data.frame(dataset_supervised[,1:14],dataset_supervised[,19:28],dataset_supervised[,30:47])

#LOADINGS PLOT

b<- list(
  family = "sans serif",
  size = 30,
  color = toRGB("grey50"))

#2-DIMENSIONAL
loadings2d<-plot_ly(x=comp[,1], y=(comp[,2]), mode="markers", text = colnames(freedom_norm_short[2:38]))
loadings2d <- loadings2d %>% add_markers(size=4)
loadings2d <- loadings2d %>% add_text(textfont = b, textposition = "top right")
loadings2d <- loadings2d %>% layout(title = 'Loadings',
                                    xaxis = list(title = 'First Component'
                                    ),
                                    yaxis = list(title = 'Second Component'
                                    ))
loadings2d

#3-DIMENSIONAL
loadings3d <-plot_ly(x=comp[,1], y=(comp[,2]), z=(comp[,3]), mode="markers", text = colnames(freedom_norm_short[2:38]))
loadings3d <- loadings3d %>% add_markers()
loadings3d <- loadings3d %>% add_text(textfont = b, textposition = "top right")
loadings3d



#PRINCIPAL COMPONENT ANALYSIS WITH PREDEFINED FUNCTIONS: representations
#2-DIMENSIONAL: COUNTRIES
res.pca <- prcomp(freedom_norm_pca, scale = TRUE)
third_comp<-res.pca$x
fviz_eig(res.pca)
fviz_pca_ind(res.pca,
             col.ind = third_comp[,3], # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#2-DIMENSIONAL: LOADINGS
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#BIPLOT
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

#SUPERVISED LEARNING
#CREATING THE DATASET: handling missing data
dataset_imp<-dataset_supervised[,3:42]
preProcValues <- preProcess(dataset_imp,method = c("knnImpute"),
                            k = 3,
                            knnSummary = mean)
impute_dataset <- predict(preProcValues, dataset_imp,na.action = na.pass)

procNames <- data.frame(col = names(preProcValues$mean), mean = preProcValues$mean, sd = preProcValues$std)
for(i in procNames$col){
  impute_dataset[i] <- impute_dataset[i]*preProcValues$std[i]+preProcValues$mean[i] 
}

dataset_imputed<-data.frame(dataset_supervised[,1:2],impute_dataset)

#TRAIN AND TEST SETS FOR FORECASTING
set.seed(131)
intrain_tree<-createDataPartition(dataset_imputed$overall_freedom,p=0.75,list=FALSE)
train_set<-dataset_imputed[intrain_tree,]
test_set<-dataset_imputed[-intrain_tree,]
testing_newvar_names<-test_set[,1:2]

#DECISION TREE
#DECISION TREE INTERPRETATION ON THE WHOLE DATASET
tree <- rpart(overall_freedom~precip+temp+coast+belgium+britain+france+germany+italy+netherlands+portugal+spain+
                fract+resources+RDI+christian+muslim+unaffiliated+jewish+region_Americas+
                region_Asia.Pacific+region_Europe+region_Middle.East.and.North.Africa+
                region_Sub.Saharan.Africa+legal_Civil.and.Common.Law+legal_Civil.and.Sharia.Law+
                legal_Civil.Law+legal_Common.and.Sharia.Law+
                legal_Common.Law+legal_Religious.Law+current_comm+
                previous_comm+current_constitut_ref_socialism+
                previous_constitut_ref_socialism+governing_social_commun+communism, data = dataset_imputed,
              method  = "anova")
rpart.plot(tree,box.palette = "OrBu",type =1,tweak=1.3)


#TRAINING THE DECISION TREE
tree_train <- rpart(overall_freedom~precip+temp+coast+belgium+britain+france+germany+italy+netherlands+portugal+spain+
                      fract+resources+RDI+christian+muslim+unaffiliated+jewish+region_Americas+
                      region_Asia.Pacific+region_Europe+region_Middle.East.and.North.Africa+
                      region_Sub.Saharan.Africa+legal_Civil.and.Common.Law+legal_Civil.and.Sharia.Law+
                      legal_Civil.Law+legal_Common.and.Sharia.Law+
                      legal_Common.Law+legal_Religious.Law+current_comm+
                      previous_comm+current_constitut_ref_socialism+
                      previous_constitut_ref_socialism+governing_social_commun+communism, data = train_set,
                    method  = "anova")
rpart.plot(tree_train,box.palette = "OrBu",type =1,tweak=1.3)

importance_tree<- data.frame(tree_train$variable.importance)
importance_tree<- data.frame(rownames(importance_tree),importance_tree)
importance_tree

#PREDICTIONS AND EVALUATION
tree_pred<- predict(tree_train,test_set,type="vector")

compare_tree<-data.frame(testing_newvar_names,tree_pred)
tree_RMSE <- sqrt(mean((tree_pred-testing_newvar_names[,2])^2))
tree_RMSE

#PRUNINING THE TREE

(b <- tree_train$cptable[which.min(tree_train$cptable[, "xerror"]), "CP"])

pruned_model <- prune(tree_train, cp = b)

rpart.plot(pruned_model,box.palette = "OrBu",type =1,tweak=1.3)

#PREDICTIONS AND EVALUATION OF THE PRUNED MODEL
tree_pred_pruned<- predict(pruned_model,test_set,type="vector")

compare_pruned<-data.frame(testing_newvar_names,tree_pred_pruned)
tree_RMSE_pruned <- sqrt(mean((tree_pred_pruned-testing_newvar_names[,2])^2))
tree_RMSE_pruned



#RANDOM FOREST ON THE WHOLE DATASET FOR VARIABLE IMPORTANCE INTERPRETATION
set.seed(4543)
rf <- randomForest(overall_freedom~ precip+temp+coast+belgium+britain+france+germany+italy+netherlands+portugal+spain+
                     fract+resources+RDI+christian+muslim+unaffiliated+jewish+region_Americas+
                     region_Asia.Pacific+region_Europe+region_Middle.East.and.North.Africa+
                     region_Sub.Saharan.Africa+legal_Civil.and.Common.Law+legal_Civil.and.Sharia.Law+
                     legal_Civil.Law+legal_Common.and.Sharia.Law+
                     legal_Common.Law+legal_Religious.Law+current_comm+
                     previous_comm+current_constitut_ref_socialism+
                     previous_constitut_ref_socialism+governing_social_commun+communism, data=dataset_imputed, ntree=1000,
                   keep.forest=FALSE, importance=TRUE)
varImpPlot(rf)

ImpData <- data.frame(rf$importance)
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`X.IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`X.IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

#"randomForest" package
#TRAINING THE RANDOM FOREST WITH 10-FOLD CROSS VALIDATION 
#FOR HYPERPARAMETER TUNING: "mtry"
rf_fit<-train(overall_freedom~ precip+temp+coast+belgium+britain+france+germany+italy+netherlands+portugal+spain+
                fract+resources+RDI+christian+muslim+unaffiliated+jewish+region_Americas+
                region_Asia.Pacific+region_Europe+region_Middle.East.and.North.Africa+
                region_Sub.Saharan.Africa+legal_Civil.and.Common.Law+legal_Civil.and.Sharia.Law+
                legal_Civil.Law+legal_Common.and.Sharia.Law+
                legal_Common.Law+legal_Religious.Law+current_comm+
                previous_comm+current_constitut_ref_socialism+
                previous_constitut_ref_socialism+governing_social_commun+communism,
              data=train_set, method="rf",n_estimators=1000, 
              importance=TRUE, trControl=trainControl(method = "cv", number=10))
rf_fit

varImp(rf_fit)
varImpPlot(rf_fit$finalModel)

#PREDICTIONS AND EVALUATION OF THE randomForest MODEL
rf_pred<-predict(rf_fit$finalModel, test_set)
compare_rf<-data.frame(testing_newvar_names,rf_pred)
rf_RMSE <- sqrt(mean((test_set$overall_freedom - rf_pred)^2))
rf_RMSE

#"ranger" package
#TRAINING THE RANDOM FOREST WITH 10-FOLD CROSS VALIDATION 
#FOR HYPERPARAMETERS TUNING: "mtry", "splitrule", "min.node.size"

set.seed(54)
ranger_fit<-train(overall_freedom~ precip+temp+coast+belgium+britain+france+germany+italy+netherlands+portugal+spain+
                    fract+resources+RDI+christian+muslim+unaffiliated+jewish+region_Americas+
                    region_Asia.Pacific+region_Europe+region_Middle.East.and.North.Africa+
                    region_Sub.Saharan.Africa+legal_Civil.and.Common.Law+legal_Civil.and.Sharia.Law+
                    legal_Civil.Law+legal_Common.and.Sharia.Law+
                    legal_Common.Law+legal_Religious.Law+current_comm+
                    previous_comm+current_constitut_ref_socialism+
                    previous_constitut_ref_socialism+governing_social_commun+communism,
                  data=train_set, method="ranger", num.trees = 1000,
                  importance="impurity", trControl=trainControl(method = "cv", number=10))
ranger_fit

varImp(ranger_fit)

#PREDICTIONS AND EVALUATION OF THE ranger MODEL
ranger_pred<-predict(ranger_fit, test_set)
compare_ranger<-data.frame(testing_newvar_names,ranger_pred)
ranger_RMSE <- sqrt(mean((test_set$overall_freedom - ranger_pred)^2))
ranger_RMSE







