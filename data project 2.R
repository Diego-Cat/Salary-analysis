rm(list=ls())
library(plotly)
library(dummy)
library(cluster)
library(factoextra)

Salary_Data = read.csv(file.choose())
Salary_Data = na.omit(Salary_Data)
summary(Salary_Data)


summary(Salary_Data)

# build an ordinal dummy variable for "Education Level"
Education_Level = as.numeric(factor(Salary_Data$Education.Level, levels = c("Bachelor's","Master's","PhD")))
Salary_Data$Education.Level.num = Education_Level

#remove the old categorical variable "Education Level" and the "Job Title" variable
Salary_Data1 = Salary_Data[,-3]
Salary_Data1 = Salary_Data1[,-3]
View(Salary_Data1)

#separate the gender column in order to normalize all the other variables
Salary_Data_2 = Salary_Data1[,-2]
View(Salary_Data_2)
summary(Salary_Data_2)

#scale the data without the "Education Level" variable, which does not need normalization
Salary_Data_scaled= scale(Salary_Data_2[,-4])
View(Salary_Data_scaled)


#create a dummy variable for gender
Gender = dummy::dummy(Salary_Data1,int = TRUE)
View(Gender)
Gender_Male = Gender$Gender_Male

#bind it to the rest of the data
Salary_Data_scaled = cbind.data.frame(Salary_Data_scaled,Education_Level, Gender_Male)
View(Salary_Data_scaled)

#compute the COVARIANCE MATRIX
cov_matrix = cov(Salary_Data_scaled)
cov_matrix
corr_matrix = cor(Salary_Data_scaled)
corr_matrix

#perform the pca on "Age" and "Yrs of Exp" to avoid multicolinearity
pca = prcomp(Salary_Data_scaled[,-(3:5)], center = TRUE, scale. = TRUE)
print(pca$rotation)

#translate the results into % of explained variance per principal component
pca_variance = (pca$sdev^2)/sum(pca$sdev^2)*100
pc_labels = paste0("PC", 1:length(pca_variance))
pca_variance
barchart_pc = barplot(pca_variance, main = "Principal Components' Explained Variance: Scree Plot", 
        xlab = "Principal Components", ylab = "Percent of Variance Explained", 
        names.arg =pc_labels, ylim =c(0,100), col = "skyblue")
lines(x = barchart_pc, y = pca_variance, lwd = 2, type = "b",pch = 1, cex = 1.15)

#plot the first principal component into a scatterplot
data_scatter = data.frame(pca$x)
plot(data_scatter$PC1, ylab = "Observation Score", xlab = "Observation", main = "Principal Component 1")

#make our new dataframe made up by the three dimensions of explanatory variables (PC1, "Gender", "Years of Experience")
explanatory_variables = cbind.data.frame(data_scatter$PC1,Salary_Data_scaled$Education_Level,Salary_Data_scaled$Gender_Male)

#plot them
plot_ly(explanatory_variables, x = explanatory_variables$`data_scatter$PC1`,
        y = explanatory_variables$`Salary_Data_scaled$Education_Level`,
        z = explanatory_variables$`Salary_Data_scaled$Gender_Male`, 
        color = ~factor(explanatory_variables$`Salary_Data_scaled$Gender_Male`),
        colors = c("pink","blue"),
        type = "scatter3d", mode = "markers") %>%
        layout( scene = list(
          xaxis = list(title = "PC1 - Years"), 
          yaxis = list(title = "Education Lvl"), 
          zaxis = list(title = "Gender Male")))

#make a multiple linear regression
regression = lm(Salary_Data_scaled$Salary ~ 
                  explanatory_variables$`data_scatter$PC1` +
                  explanatory_variables$`Salary_Data_scaled$Education_Level` +
                  explanatory_variables$`Salary_Data_scaled$Gender_Male`)
summary(regression)

#now we find the % of variance explained by each variable
#Because these are Type II tests, each row tells you “what happens if I add that term to a model that already 
#contains all the other main effects.” That makes them order-invariant and ideal for assessing each predictor’s
#unique, marginal contribution in an unbalanced design made of different types of variables.
anova_2 = car::Anova(regression, type = "II")
percent_var = (anova_2$`Sum Sq`/sum(anova_2$`Sum Sq`)*100)
percent_var

#take the sum of the % var explained by the variables, without residuals, and calculate the % of this sum that each variable takes. These will be the gower weights for hierarchical clustering.
sum_var_explained = sum(percent_var[-4])
sum_var_explained
percent_var_gower = percent_var[-4]/sum_var_explained

#these will be the weights for the gower distance used for hierarchical clustering
percent_var_gower

#calculate the distance matrix, specifying that the second column is ordinal and the third is binary
gower_distance = daisy(explanatory_variables, metric = "gower", weights = percent_var_gower,  type = list(ord = 2 ,asymm = 3))
gower_distance

hierarchical_clustering = hclust(gower_distance, method = "ward.D2")
dendogram = plot(hierarchical_clustering)
k= 2
clusters = cutree(hierarchical_clustering, k = k)
table(clusters)
fviz_cluster(list(data = explanatory_variables, cluster = clusters),
             geom = "point", 
             main = "Cluster Plot (Gower + Ward)")
plot_ly(explanatory_variables, 
        x     = ~explanatory_variables$`data_scatter$PC1`, 
        y     = ~explanatory_variables$`Salary_Data_scaled$Education_Level`, 
        z     = ~explanatory_variables$`Salary_Data_scaled$Gender_Male`, 
        color = ~factor(clusters),       
        colors= c("red","green"),  
        type  = "scatter3d", 
        mode  = "markers") %>%
  layout(
    scene = list(
      xaxis = list(title = "PC1 - Years"),
      yaxis = list(title = "Education Level"),
      zaxis = list(title = "Gender (0/1)")
    )
  )

#now calculate the descriptive statistics of salary for cohorts belonging to cluster 1 and cluster 2
#the discriminant factor in calculating salary is determined to be the position of the individual wrt the average value of age for their cohort
Salary_Data_2 = na.omit(Salary_Data_2)
Salary_Data_2$clusters = clusters

#mean, sd, median, IQR, min, max
descriptive_stats = function(x) {
  c(
    n      = length(x),
    mean   = mean(x),
    sd     = sd(x),
    median = median(x),
    IQR    = IQR(x),
    min    = min(x),
    max    = max(x)
  )
}

#age descriptive stats by cluster
by(explanatory_variables$`data_scatter$PC1`, Salary_Data_2$clusters, descriptive_stats)
boxplot(explanatory_variables$`data_scatter$PC1` ~ Salary_Data_2$clusters,
        main = "Salary by Cluster",
        xlab = "Cluster", ylab = "PC1 - Years")

#salary descriptive stats by cluster
by(Salary_Data_2$Salary, Salary_Data_2$clusters, descriptive_stats)

boxplot(Salary ~ clusters, data = Salary_Data_2,
        main = "Salary by Cluster",
        xlab = "Cluster", ylab = "Salary")

#then make a mlr with the original non_scaled variables, using only the one with the highest weight in PCA to choose btween age and exp

#add binary gender variable to the non-scaled dataset
Salary_Data_2 = cbind.data.frame(Salary_Data_2, Salary_Data_scaled$Gender_Male)
colnames(Salary_Data_2)[6] = "Gender_Male"
View(Salary_Data_2)

# Set a random seed for reproducibility
set.seed(123)

# Split the data
train_indices = sample(1:nrow(Salary_Data_2), size = 0.8 * nrow(Salary_Data_2))
Salary_Data_2.train = Salary_Data_2[train_indices, ]
Salary_Data_2.test  = Salary_Data_2[-train_indices, ]

# Load required libraries
library(rpart)
library(rpart.plot)

# Fit the decision tree regression model
salaries.dt = rpart(Salary ~ Years.of.Experience + Education.Level.num + Gender_Male, data = Salary_Data_2.train)

# Visualize the decision tree
options(scipen = 999)
rpart.plot(salaries.dt,
           type =2,
           digits = 2, 
           roundint = TRUE,
           main = "Salary Regression Tree")

# Predict salary on the test set
salaries.dt.pred = predict(salaries.dt, newdata = Salary_Data_2.test)

# Plot predictions vs actual values
plot(salaries.dt.pred, Salary_Data_2.test$Salary,
     xlab = "Predicted Salary",
     ylab = "Actual Salary",
     main = "Decision Tree: Predicted vs Actual Salary")
abline(0, 1, col = "blue")  # reference line

# Compute regression metrics
# Mean Squared Error (MSE)
mse_dt = mean((salaries.dt.pred - Salary_Data_2.test$Salary)^2)
print(paste("Decision Tree - Mean Squared Error:", mse_dt))

# R-squared
rsq_dt = 1 - sum((salaries.dt.pred - Salary_Data_2.test$Salary)^2) / 
  sum((mean(Salary_Data_2.train$Salary) - Salary_Data_2.test$Salary)^2)
print(paste("Decision Tree - R-squared:", rsq_dt))

