# Script for analyzing depression
# Loading packages 
library(data.table) # Fread and data.table functionality
library(tidyverse) # Multiple packages including dplyr
library(kableExtra) # Output tables
library(caret) # Cross-validation
library(ggplot2) # Plots
library(ggthemr) # Theme of ggplots
library(rcompanion) # Cramer v test
library(randomForest) # Random forest model
library(pROC) # ROC plots
library(rcompanion)
library(DescTools)

# Function for applying chi-squared test
apply_chi_cramer = function(pairs,dt){
  # column_pairs : Names of categorical columns
  # dt: Data table that is passed
  
  # Getting the first and second categorical columns
  v1 = pairs[1]
  v2 = pairs[2]
  
  # Creating a table with frequencies
  tbl =  table(dt[[v1]],
               dt[[v2]])
  
  # Computing Chi-Squared values
  res_chi = chisq.test(tbl)
  
  # Computing Cramer's V Measure
  res_cramer_v = rcompanion::cramerV(tbl)
  
  # Returning Results
  return(data.table::data.table(variable1 = v1,
                                variable2 = v2, 
                                p_value_chi = res_chi$p.value,
                                statistic_chi = res_chi$statistic,
                                cramer_V = res_cramer_v
  )
  )
  
}


# Missing values function
missing_val_func = function(col){
  n = length(col)
  total_missing = sum(is.na(col))
  percent_missing = round(total_missing/n*100, digits = 2)
  return(list(total_missing,percent_missing))
}

# Loading in needed datasets
DIDA = fread("Depression_In_Depth.csv") 

# Dimensions of the dataset
dim(DIDA)

# Getting the class counts for depressed
depressed_counts = DIDA[,.(total = .N), keyby = DEPRESSED]

# Changing depressed into a factor
depressed_counts$DEPRESSED = factor(depressed_counts$DEPRESSED,
                                    levels = c(0,1),
                                    labels = c("Not Depressed",
                                               "Depressed"))
# Setting the theme for ggplot
ggthemr("fresh")

# Displaying class counts
ggplot(depressed_counts,
       aes(x = DEPRESSED,
           y = total,
           fill = DEPRESSED)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total),
            position = position_stack(vjust = 0.5), 
            size = 3,
            color = "white") + 
  labs(title = "Class Count of Depression Variable",
       x = "Class",
       y = "Count",
       fill = "Category") 

# Getting a subset of all numerical variables
numerical_DIDA = DIDA[,.SD,.SDcols = sapply(DIDA,is.numeric)] 

# The number of numerical variables
dim(numerical_DIDA)[2]

# aggregation of age range
age_dt = DIDA[,.(total = .N), keyby = AGERNG]

# The color palette for fresh plus two additional colors
fresh_colors = c("#111111",
                 "#65ADC2",
                 "#233B43", 
                 "#E84646", 
                 "#C29365", 
                 "#362C21",
                 "#316675",
                 "#168E7F",
                 "#109B37",
                 "#F8766D",
                 "#B79F00")


# Displaying the age range distribution
ggplot(age_dt,
       aes(x = AGERNG,
           y = total,
           fill = AGERNG)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = fresh_colors) +
  geom_text(aes(label = total),
            position = position_stack(vjust = 0.5), 
            size = 3,
            color = "white") + 
  labs(title = "Distribution of Age Range",
       x = "Age Range",
       y = "Count",
       fill = "Category") 

# aggregated subset of educated students with/out depression
gender_dt = DIDA[,.(count = .N), 
                 keyby = .(GENDER,DEPRESSED)]

#Plot of participant counts who are educated
ggplot(gender_dt, aes(x = GENDER,
                      y= count,
                      fill = factor(DEPRESSED))) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), 
            position = position_dodge(width = 0.9), 
            vjust = 1.5, 
            size = 3,
            color = "white") +
  labs(title = "Gender ~ Depression Count", 
       x = "Gender",
       y = "Count",
       fill = "Depression Status")

# Creating a percentage column
gender_dt2 = DIDA[,.(Count = .N), 
                  keyby = .(GENDER)][,Percentage := round(Count/sum(Count),
                                                          digits = 2)*100]

# Pie chart of the count of females and males
ggplot(gender_dt2, aes(x = "",
                       y= Percentage,
                       fill = factor(GENDER))) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste(Percentage,"%")), 
            position = position_stack(vjust = 0.5), 
            size = 3,
            color = "white") +
  labs(title = "Percentage of Men and Women", 
       x = NULL,
       y = NULL,
       fill = "Gender") +
  theme_void()

# Adding a percentage column 
gender_dt[,Percentage := round(count/sum(count), digits = 2)*100,
          keyby = GENDER]

# Creating labels
gender_dt$Label = factor(ifelse(gender_dt$DEPRESSED == 1, "Depressed", "Not Depressed"),
                         levels = c("Not Depressed","Depressed"))


# Excluding variables that have been analyzed 
variables_exclude = c("AGERNG","GENDER")

# Casting to long format
DIDA_long = DIDA[,melt(.SD, id.vars = "DEPRESSED",
                       variable.name = "Variable",
                       value.name = "Value")]

# Counting the number of depressed
DIDA_long = DIDA_long[,.(Count = .N),
                      by = .(DEPRESSED, Variable, Value)]

# Calculating the percentage 
DIDA_long[,Percentage := Count/sum(Count),
          by = .(Variable, Value)]

# Making another aggregated table for the bar charts

DIDA_long_agg = DIDA_long[,.(Count = sum(Count)),
                          by = .(Variable, Value)]

# Getting a subset of the variables
subset_columns1 = as.character(unique(DIDA_long$Variable)[3:10])


# Creating bar plots of counts 3-10
ggplot(DIDA_long_agg[Variable %in% subset_columns1,],
       aes(x = Value,
           y= Count)) +
  geom_bar(stat = "identity")  +
  geom_text(aes(label = Count),
            vjust = 1.5,
            size = 3,
            color = "white") +
  facet_wrap(~Variable, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Pie charts 3-10
ggplot(DIDA_long[Variable %in% subset_columns1,], 
       aes(x = "", y = Percentage,
           fill = factor(DEPRESSED))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~Variable + Value, scales = "free") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage*100)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "Depression") 

# Getting a subset of the variables 11-18
subset_columns2 = as.character(unique(DIDA_long$Variable)[11:18])

# Creating bar plots of counts
ggplot(DIDA_long_agg[Variable %in% subset_columns2,],
       aes(x = Value,
           y= Count)) +
  geom_bar(stat = "identity")  +
  geom_text(aes(label = Count),
            vjust = 1.5,
            size = 3,
            color = "white") +
  facet_wrap(~Variable, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Pie charts 11-18
ggplot(DIDA_long[Variable %in% subset_columns2,], 
       aes(x = "", y = Percentage,
           fill = factor(DEPRESSED))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~Variable + Value, scales = "free") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage*100)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "Depression") 

# Getting a subset of the variables
subset_columns3 = as.character(unique(DIDA_long$Variable)[19:26])

# Creating bar plots of counts 19-26
ggplot(DIDA_long_agg[Variable %in% subset_columns3,],
       aes(x = Value,
           y= Count)) +
  geom_bar(stat = "identity")  +
  geom_text(aes(label = Count),
            vjust = 1.5,
            size = 3,
            color = "white") +
  facet_wrap(~Variable, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Pie chart 19-26
ggplot(DIDA_long[Variable %in% subset_columns3,], 
       aes(x = "", y = Percentage,
           fill = factor(DEPRESSED))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~Variable + Value, scales = "free") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage*100)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "Depression")

# Getting a subset of the variables
subset_columns4 = as.character(unique(DIDA_long$Variable)[27:30])

# Creating bar plots of counts 27-30
ggplot(DIDA_long_agg[Variable %in% subset_columns4,],
       aes(x = Value,
           y= Count)) +
  geom_bar(stat = "identity")  +
  geom_text(aes(label = Count),
            vjust = 1.5,
            size = 3,
            color = "white") +
  facet_wrap(~Variable, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Pie charts 27-30
ggplot(DIDA_long[Variable %in% subset_columns3,], 
       aes(x = "", y = Percentage,
           fill = factor(DEPRESSED))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~Variable + Value, scales = "free") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage*100)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "Depression")

# Pearson’s Chi-Squared Test for Correlation between Categorical Variables

# Columns of data.table
columns = names(DIDA)

# Generating all pairs for categorical columns
column_pairs = combn(columns, 2, simplify = FALSE)

# Getting chi-squared results for all pairs
suppressWarnings({
chi_cramer_results = lapply(column_pairs,
                            function(x) apply_chi_cramer(pairs = x,
                                                         dt = DIDA))
})

# Creating a data.table from all results
chi_cramer_results = do.call(rbind, chi_cramer_results)

# Getting a subset of all pair that have a significant p-value
chi_cramer_significant = chi_cramer_results[p_value_chi < 0.05,][order(p_value_chi)]

# Labeling rows based on Cramer V association.
cut_b = c(0,0.1,0.3,0.5,1)
cut_labs = c("Weak", "Moderate", "Strong", "Very Strong")

# Assigning it to chi_cramer_significant
chi_cramer_significant[,association_type := cut(cramer_V, 
                                                breaks = cut_b,
                                                labels = cut_labs,
                                                include.lowest = TRUE)]

# Getting the first 10 rows
dis_tbl1 = chi_cramer_significant[1:10,]

# Displaying the first 10 rows
knitr::kable(dis_tbl1,booktabs=TRUE, 
             format="latex",
             caption = "Categorical Association")%>%
  kable_styling(latex_options = "HOLD_position")

# Heatmap of association
ggplot(chi_cramer_significant, 
       aes(x = variable1, 
           y = variable2, 
           fill = cramer_V)) +
  geom_tile() +
  labs(title = "Heatmap of Cramer's V (Significant Variables)",
       x = " ",
       y = " ",
       fill =  "Cramer's Value") +
  theme(axis.text.x = element_text(size = 8,
                                   angle = 45,
                                   hjust = 1))

# Getting a subset where pairs contain depressed
cc_depressed_subset = chi_cramer_significant[variable1 == "DEPRESSED" | variable2 == "DEPRESSED"]

# Displaying the results
knitr::kable(cc_depressed_subset,booktabs=TRUE, 
             format="latex",
             caption = "Categorical Association with Depression")%>%
  kable_styling(latex_options = "HOLD_position")

# Aggregating the columns based on association type
assoc_type_agg =  cc_depressed_subset[,.(Count = .N),
                                      keyby = association_type]
# Plot of association 
ggplot(assoc_type_agg, aes(x = association_type,
                           y = Count,
                           fill = association_type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count ~ Association Strength") +
  geom_text(aes(label = Count),
            vjust = 1.5,
            size = 3,
            color = "white")

# Heatmap of association
ggplot(cc_depressed_subset, 
       aes(x = variable1, 
           y = variable2, 
           fill = cramer_V)) +
  geom_tile() +
  labs(title = "Heatmap of Cramer's V",
       x = " ",
       y = " ",
       fill =  "Cramer's Value") +
  theme(axis.text.x = element_text(size = 8,
                                   angle = 45,
                                   hjust = 1))
# Calculating the X squared with dependent variable
dependent_variables = columns[-length(columns)]

# Passing variables to function 
suppressWarnings({
X_squared_results = lapply(dependent_variables,
                           function(x){
                             tbl = table(DIDA[[x]],DIDA$DEPRESSED)
                             res_chi = as.numeric(chisq.test(tbl)$p.value)
                             res_cramer_v = as.numeric(rcompanion::cramerV(tbl))
                             dt = data.table(variable = x,
                                             chi_squared = res_chi,
                                             cramer_v = res_cramer_v)
                             return(dt)
                           })
})

# Creating a data.table from all results
chi_cramer_selection = do.call(rbind, X_squared_results)

# Selecting the variables based on a cut-off criterion
chi_cutoff = 0.05
cramer_cutoff = 0.2

# Data table containing the selected variables
selected_variables_dt = chi_cramer_selection[chi_squared < chi_cutoff &
                                               cramer_v > cramer_cutoff ]
# Making a display table
display_selected_variables = selected_variables_dt

# Changing names of the dataset
names(display_selected_variables) = c("Variables", "P-Values", "Cramer's V")


# Getting the selected variables
DIDA_Sub = DIDA[,selected_variables_dt$variable, with = F]


# Changing all the character variables to integers
DIDA_Sub[,names(DIDA_Sub):= lapply(.SD,function(x) as.integer(x == "Yes") 
)]

# Adding the response variable
DIDA_Sub$DEPRESSED = DIDA$DEPRESSED

# Changing the response into a factor
DIDA_Sub$DEPRESSED = factor(DIDA_Sub$DEPRESSED)

# Training and Testing Models 

set.seed(1255)
# Training/validation index 65%
train_index = createDataPartition(DIDA_Sub$DEPRESSED, 
                                  p = 0.65,
                                  list = FALSE)

# Training/validation data
train_set = DIDA_Sub[train_index]

# Testing set 35%
test_set = DIDA_Sub[-train_index]

# Creating hyperparameters for Random Forest 
rf_hyper_grid = expand.grid(
  mtry = c(3, 5, 8, 10, 13)
)

# Other hyper parameters for random forest
ntree = c(100, 200, 300, 500, 1000)
nodesize = c(1, 5, 10)
maxnodes = c(10, 20, 30)

# Creating hyperparameters for logistic regression
lr_hyper_grid = expand.grid(
  alpha = seq(0, 1, by = 0.1),  
  lambda = 10^seq(-3, 1, length = 100)  
)

# Creating hyperparameters for gradient boosting
gb_hyper_grid = expand.grid(
  n.trees = c(50, 100, 200, 300, 400, 500),
  interaction.depth = c(1, 3, 5, 7, 9),
  shrinkage = c(0.01, 0.05, 0.1, 0.2),
  n.minobsinnode = c(5, 10, 15, 20)
)

set.seed(409)
# Settings for caret train
tc = trainControl(
  method = "cv", 
  number = 5,
  search = "grid")

# Training and tuning random forest model
# Creating a list to store the results
rfm_results = list()

# Note that the code is commented out because it saved as RDS

# # Applying all the hyperparameters
# for(ntree_value in ntree){
#   for(nodesize_value in nodesize){
#     for(maxnodes_value in maxnodes){
#       set.seed(123)
#       rfm =  train(
#         DEPRESSED ~ .,
#         data = train_set,
#         method = "rf",
#         trControl = tc,
#         tuneGrid = rf_hyper_grid,
#         ntree = ntree_value,
#         nodesize = nodesize_value,
#         maxnodes = maxnodes_value
#       )
# 
#       # Storing the results of the model
#       rfm_results[[paste("ntree=",
#                          ntree_value,
#                          "nodesize=",
#                          nodesize_value,
#                          "maxnodes=",
#                          maxnodes_value,
#                          sep = "_")]] = rfm
#     }
#   }
# }
# 
# # Saving as RDS to not run computationally expensive code
# saveRDS(rfm_results, file = "rfm_results.rds")

# Loading the stored results
rfm_results =readRDS(file = "rfm_results.rds")

# # Training and tuning logistic regression model
# lrm = train(
#   DEPRESSED ~ .,
#   data = train_set,
#   method = "glmnet",
#   trControl = tc,
#   tuneGrid = lr_hyper_grid,
#   family = "binomial"
# )
# 
# # # Saving logistic regression results
# saveRDS(lrm, file = "lrm.rds")

# Loading the stored results
lrm =readRDS(file = "lrm.rds")

# # Training the gradient boosting model
# gbm = train(
#   DEPRESSED ~ .,
#   data = train_set,
#   method = "gbm",
#   preProcess = c("scale", "center"),
#   trControl = tc,
#   tuneGrid = gb_hyper_grid,
#   verbose = FALSE
# )
# 
# # Saving gradient boosting results
# saveRDS(gbm, file = "gbm.rds")

# Loading the stored results
gbm =readRDS(file = "gbm.rds")

# Creating a list of best models from RF
rfm_metrics_list = lapply(seq_along(rfm_results),function(y){
  x = rfm_results[[y]]
  best_row = x$results[x$results$mtry == as.numeric(x$bestTune),]
  best_row$model_number = y
  return(best_row)
})

# Changing list into a data table
rfm_metrics_dt = data.table(
  bind_rows(rfm_metrics_list))[,.(mtry, Accuracy,Kappa,model_number)]

# Casting the data to long
rfm_met_long = melt(rfm_metrics_dt,
                    id.vars = "model_number",
                    measure.vars = c("Accuracy","Kappa"),
                    variable.name = "Metric",
                    value.name = "Value")

# Getting the max accuracy and kappa
max_acc = rfm_met_long[Metric == "Accuracy",.SD[which.max(Value)]]
max_kap = rfm_met_long[Metric == "Kappa",.SD[which.max(Value)]]

# ggplot of results
ggplot(rfm_met_long, aes(x = model_number,
                         y = Value, 
                         color = Metric)) +
  geom_point() +
  geom_line() +
  geom_point(data = max_acc,
             aes(x = model_number,y = Value),
             color = "red") +
  geom_point(data = max_kap,
             aes(x = model_number,y = Value),
             color = "red") +
  labs(title = "Accuracy and Kappa for Random Forest Models",
       x = "Model Number",
       y = "Metric")

# Best Random Forest Model
rf_best_model = rfm_results[[18]]$finalModel

# Getting subset of the results (Accuracy and Kappa)
lm_df = lrm$results[,3:4]

# Creating an ID for rows
lm_df$model_number = 1:nrow(lm_df)

# Casting logistic regression results to long
lrm_met_long = melt(lm_df,
                    id.vars = "model_number",
                    measure.vars = c("Accuracy","Kappa"),
                    variable.name = "Metric",
                    value.name = "Value")

# Changing to a data.table
lrm_met_long = data.table(lrm_met_long)

# Getting the max accuracy and kappa
max_acc2 = lrm_met_long[Metric == "Accuracy",.SD[which.max(Value)]]
max_kap2 = lrm_met_long[Metric == "Kappa",.SD[which.max(Value)]]

# ggplot of results
ggplot(lrm_met_long, aes(x = model_number,
                         y = Value, 
                         color = Metric)) +
  geom_point() +
  geom_line() +
  geom_point(data = max_acc2,
             aes(x = model_number,y = Value),
             color = "red") +
  geom_point(data = max_kap2,
             aes(x = model_number,y = Value),
             color = "red") +
  labs(title = "Accuracy and Kappa for Logistic regression",
       x = "Model Number",
       y = "Metric")

# Best logistic regression model
lrm_best_model = lrm$finalModel 

# Accuracy vs Regularization Parameter
plot(lrm)

# Plot of performance metrics
plot(gbm)

# Getting subset of the results (Accuracy and Kappa)
gbm_df = gbm$results[,5:6]

# Creating an ID for rows
gbm_df$model_number = 1:nrow(gbm_df)

# Casting logistic regression results to long
gbm_met_long = melt(gbm_df,
                    id.vars = "model_number",
                    measure.vars = c("Accuracy","Kappa"),
                    variable.name = "Metric",
                    value.name = "Value")

# Changing to a data.table
gbm_met_long = data.table(gbm_met_long)

# Getting the max accuracy and kappa
max_acc3 = gbm_met_long[Metric == "Accuracy",.SD[which.max(Value)]]
max_kap3 = gbm_met_long[Metric == "Kappa",.SD[which.max(Value)]]

# ggplot of results
ggplot(gbm_met_long, aes(x = model_number,
                         y = Value, 
                         color = Metric)) +
  geom_point() +
  geom_line() +
  geom_point(data = max_acc3,
             aes(x = model_number,y = Value),
             color = "red") +
  geom_point(data = max_kap3,
             aes(x = model_number,y = Value),
             color = "red") +
  labs(title = "Accuracy and Kappa for Gradient Boosting",
       x = "Model Number",
       y = "Metric")


# Best gradient boosting model
gbm_best_model = gbm$finalModel

# Random Forest predictions
rf_pred = predict(rf_best_model,newdata = test_set)

# Confusion matrix
rf_cm = confusionMatrix(rf_pred,test_set$DEPRESSED)

# Converting confusion matrix to a data frame
rf_cm_df = as.data.frame(rf_cm$table)

# Changing test set into matrix
test_matrix = test_set[,-which(colnames(test_set) == "DEPRESSED"),
                       with = FALSE]
test_matrix = as.matrix(test_matrix)

# Best tuned lambda
lambda_best = lrm$bestTune$lambda

# # Logistic Regression predictions
lrm_pred_prod = predict(lrm_best_model,
                        newx = test_matrix,
                        s = lambda_best,
                        type = "response")

# Setting seed for reproducibility 
set.seed(991)

# Creating lrm_pred into a factor of 1 and 0
lrm_pred = as.factor(ifelse(lrm_pred_prod > 0.5, 1, 0))

# Confusion matrix logistic regression
lrm_cm = confusionMatrix(lrm_pred,test_set$DEPRESSED)

# Converting confusion matrix to a data frame
lrm_cm_df = as.data.frame(lrm_cm$table)

# Setting seed for reproducibility 
set.seed(981)

# Gradient Boosting predictions
gbm_pred = predict(gbm,test_set)

# Confusion matrix for Gradient Boosting
gbm_cm = confusionMatrix(gbm_pred,test_set$DEPRESSED)

# Converting confusion matrix to a data frame
gbm_cm_df = as.data.frame(gbm_cm$table)

# Plot of the confusion matrix
ggplot(rf_cm_df,
       aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq),color = "white", vjust = 1) +
  #scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Confusion Matrix Random Forest",
       x = "Actual",
       y = "Predicted") +
  theme_minimal()

# Plot of the confusion matrix logistic regression
ggplot(lrm_cm_df,
       aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq),color = "white", vjust = 1) +
  #scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Confusion Matrix Logistic Regression",
       x = "Actual",
       y = "Predicted") +
  theme_minimal()

# Plot of the confusion matrix
ggplot(gbm_cm_df,
       aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq),color = "white", vjust = 1) +
  #scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Confusion Matrix Gradient Boosting",
       x = "Actual",
       y = "Predicted") +
  theme_minimal()

# Creating predictions of the type probability 
rfpred_prob = predict(rf_best_model, test_set, type = "prob")

# Computing ROC curve
rf_roc = roc(test_set$DEPRESSED,rfpred_prob[, 2])



# Plotting ROC curve
plot(rf_roc, main = "Random Forest ROC Curve")


# Computing ROC curve
lrm_roc = roc(test_set$DEPRESSED,lrm_pred_prod)

# AUC
auc(lrm_roc)

# Plotting ROC curve
plot(lrm_roc, main = "Logistic Regression ROC Curve")

# Creating predictions of the type probability 
gbmpred_prob = predict(gbm, test_set, type = "prob")

# Computing ROC curve
gbm_roc = roc(test_set$DEPRESSED,gbmpred_prob[, 2])

# AUC
auc(gbm_roc)

# Plotting ROC curve
plot(gbm_roc , main = "Gradient Boosting ROC Curve")

# Getting the best thresholds
rf_threshold = coords(rf_roc, "best", ret = "threshold")
lrm_threshold = coords(lrm_roc, "best", ret = "threshold")
gbm_threshold = coords(gbm_roc, "best", ret = "threshold")

thresh_auc_dt = data.table(Model = c("Random Forest",
                                     "Logistic Regression",
                                     "Gradient Boosting"),
                           AUC = c(auc(rf_roc),
                                   auc(lrm_roc),
                                   auc(gbm_roc)),
                           Best_Threshold = c(rf_threshold,
                                              lrm_threshold,
                                              gbm_threshold))


# Displaying the results
knitr::kable(thresh_auc_dt,
             booktabs=TRUE, 
             format="latex",
             caption = "AUC and Best Thresholds")%>%
  kable_styling(latex_options = "HOLD_position")

holder_colnames = c("Model", "Sensitivity","Specificity",
                    "Pos Pred Value" ,"Neg Pred Value",
                    "Precision","Recall","F1",
                    "Prevalence","Detection Rate" ,
                    "Detection Prevalence" ,"Balanced Accuracy")

# Creating a data table of best results
best_metrics = data.table(rbind(c("Random Forest",
                                  round(as.numeric(rf_cm$byClass),3)),
                                c("Logistic Regression",
                                  round(as.numeric(lrm_cm$byClass),3)),
                                c("Gradient Boosting",
                                  round(as.numeric(gbm_cm$byClass),3))
)) 

# Adding column names
setnames(best_metrics, holder_colnames)

# Changing columns to be numeric instead of character
best_metrics[,(names(best_metrics)[-1]):= lapply(.SD, as.numeric),
             .SDcols = names(best_metrics)[-1]]

# Displaying the results
knitr::kable(best_metrics,
             booktabs=TRUE, 
             format="latex",
             caption = "AUC and Best Thresholds")%>%
  kable_styling(latex_options = "HOLD_position")

# Best Model for Gradient Boosting

# Splitting the data again
# Setting seed for reproducibility 
set.seed(311)

# Second partition of dataset. Training is 
train_index2 = createDataPartition(DIDA_Sub$DEPRESSED, 
                                   p = 0.65,
                                   list = FALSE)

# Training set 65%
train_set2 = DIDA_Sub[train_index2]

# Testing set 35%
test_set2 = DIDA_Sub[-train_index2]

# Best hyper-parameters 
gbm_grid = expand.grid(
  n.trees = gbm_best_model$tuneValue$n.trees,
  interaction.depth = gbm_best_model$tuneValue$interaction.depth,
  shrinkage = gbm_best_model$tuneValue$shrinkage,
  n.minobsinnode = gbm_best_model$tuneValue$n.minobsinnode
)


# # # Training model with the best parameters
# Commented out because the results are saved as RDS

# final_gbm = train(
#   DEPRESSED ~ .,
#   data = train_set2,
#   method = "gbm",
#   preProcess = c("scale", "center"),
#   trControl = tc,
#   tuneGrid = gbm_grid,
#   verbose = FALSE
# )
# # 
# # # Saving best gbm
# saveRDS(final_gbm, file = "final_gbm.rds")

# Loading the stored results
final_gbm =readRDS(file = "final_gbm.rds")

# Setting seed for reproducibility 
set.seed(89)

# Getting predictions of the new model
fgbm_pred = predict(final_gbm, newdata = test_set2, type = "prob")

# ROC calculations for ROC curve
fgbm_roc = roc(test_set2$DEPRESSED,fgbm_pred[,2])

# ROC curve of the best gradient boosting model
plot(fgbm_roc,main = "ROC Curve for Final GB Model")

# Getting 1 and 0 predictions
fgbm_pred2 = predict(final_gbm, newdata = test_set2)

# Confusion matrix for Gradient Boosting
fgbm_cm = confusionMatrix(fgbm_pred2,test_set2$DEPRESSED)

# Converting confusion matrix to a data frame
fgbm_cm_df = as.data.frame(fgbm_cm$table)

# Plot of the confusion matrix
ggplot(fgbm_cm_df,
       aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq),color = "white", vjust = 1) +
  labs(title = "Confusion Matrix GB-Model",
       x = "Actual",
       y = "Predicted")


# Getting the names of the metric
fgbm_names = names(fgbm_cm$byClass)

# Creating a data table of results
fgbm_metrics = data.table(Metric = c("AUC",fgbm_names),
                          Values = c(as.numeric(auc(fgbm_roc)),
                                     as.numeric(fgbm_cm$byClass)))

# Table of Gradient boosting metrics
knitr::kable(fgbm_metrics,booktabs=TRUE, 
             format="latex",
             caption = "Best Gradient Boosting Model Results")%>%
  kable_styling(latex_options = "HOLD_position")


# variable importance 
VI = summary(final_gbm$finalModel, plotit = F)

# Variable iportance plot
ggplot(VI, aes(x = reorder(var, rel.inf), 
               y = rel.inf)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(rel.inf, 2)),
            hjust = -0.1,
            size = 3) +
  coord_flip() +
  labs(title = "Variable Importance GB Model",
       x = "Variable",
       y = "Relative Influence")

# Creating a pdp plot for the predictor ANXI
pdp_anxi = plot(final_gbm$finalModel,'ANXI', return.grid=TRUE)
ggplot(pdp_anxi, aes(x = ANXI, y = y)) +
  geom_line() +
  labs(title = "Partial Dependence Plot for ANXI",
       x = "ANXI",
       y = "Predicted Response")

# Creating a pdp plot for the predictor POSSAT
pdp_POSSAT = plot(final_gbm$finalModel,'POSSAT', return.grid=TRUE)
ggplot(pdp_POSSAT , aes(x = POSSAT, y = y)) +
  geom_line() +
  labs(title = "Partial Dependence Plot for POSSAT",
       x = "POSSAT",
       y = "Predicted Response")

# Creating a pdp plot for the predictor FINSTR
pdp_FINSTR = plot(final_gbm$finalModel,'FINSTR', return.grid=TRUE)
ggplot(pdp_FINSTR , aes(x = FINSTR, y = y)) +
  geom_line() +
  labs(title = "Partial Dependence Plot for FINSTR",
       x = "FINSTR",
       y = "Predicted Response")

# Creating hyperparameters for Random Forest 
rf_hyper_grid2 = expand.grid(
  mtry = 3
)


# # Training model with the best parameters
# Commented out because results are saved as an RDS

# brfm =  train(
#   DEPRESSED ~ ., 
#   data = train_set2, 
#   method = "rf",
#   trControl = tc,
#   tuneGrid = rf_hyper_grid2,
#   ntree = rf_best_model$ntree,
#   nodesize = rf_best_model$param$nodesize,
#   maxnodes = rf_best_model$param$maxnodes
# )

# # Saving best random forest model
# saveRDS(brfm, file = "brfm.rds")

# Loading the stored results
brfm =readRDS(file = "brfm.rds")

# Setting seed for reproducibility 
set.seed(899)

# Getting predictions of the new model
brfm_pred = predict(brfm, newdata = test_set2, type = "prob")

# Calculations for ROC curve
brfm_roc = roc(test_set2$DEPRESSED,brfm_pred[,2])

# Plotting ROC curve for Gradient Boosting Model
plot(brfm_roc,main = "ROC Curve for Final GB Model")

# Getting 1 and 0 predictions
brfm_pred2 = predict(brfm, newdata = test_set2)

# Confusion matrix for Gradient Boosting
brfm_cm = confusionMatrix(brfm_pred2,test_set2$DEPRESSED)

# Converting confusion matrix to a data frame
brfm_cm_df = as.data.frame(brfm_cm$table)

# Plot of the confusion matrix
ggplot(brfm_cm_df,
       aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq),color = "white", vjust = 1) +
  #scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Confusion Matrix GB-Model",
       x = "Actual",
       y = "Predicted") 

# Creating a data table of results
brm_metrics = data.table(Metric = c("AUC",names(brfm_cm$byClass)),
                          Values = c(as.numeric(auc(brfm_roc))))

# Table of random forest                                                                          as.numeric(brfm_cm$byClass)))
knitr::kable(brm_metrics,
             booktabs=TRUE, 
             format="html",
             caption = "Best Random Forest Model Results")%>%
  kable_styling(latex_options = "HOLD_position")

### Variable Importance from Random Forest Model
# variable importance for random forest
# Getting only the data frame from results
VI2 = varImp(brfm, scale = FALSE)$importance

# Extracting the row names
vi_names = rownames(VI2)

# Creating a data table from the results
VI_rf = data.table(var = vi_names,
                   rel.inf = VI2$Overall)

# Variable importance plot
ggplot(VI_rf , aes(x = reorder(var, rel.inf), 
                   y = rel.inf)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(rel.inf, 2)),
            hjust = -0.1,
            size = 3) +
  coord_flip() +
  labs(title = "Variable Importance GB Model",
       x = "Variable",
       y = "Relative Influence")

# Grid with best parameters for the logistic regression
lrm_grid_best = expand.grid(
  alpha = lrm_best_model$tuneValue$alpha,  
  lambda = lrm_best_model$tuneValue$lambda  
)

# # Training logistic regression with the best parameters
# lrm_final = train(
#   DEPRESSED ~ .,
#   data = train_set2,
#   method = "glmnet",
#   trControl = tc,
#   tuneGrid  = lrm_grid_best,
#   family = "binomial"
# )
# 
# # Saving best trained logistic regression
# saveRDS(lrm_final, file = "lrm_final.rds")

# Loading the stored results
lrm_final =readRDS(file = "lrm_final.rds")

# Getting predictions of the new model
lrm_final_pred = predict(lrm_final, newdata = test_set2, type = "prob")

# Calculations for ROC curve
lrm_final_roc = roc(test_set2$DEPRESSED,lrm_final_pred[,2])

# Plotting the ROC curve
plot(lrm_final_roc,main = "ROC Curve for Final Logistic Regression Model")

# Getting 1 and 0 predictions
lrm_final_pred2 = predict(lrm_final, newdata = test_set2)

# Confusion matrix for final logistic regression
lrm_final_cm = confusionMatrix(lrm_final_pred2,test_set2$DEPRESSED)

# Creating a data table of results
lrm_final_metrics = data.table(Metric = c("AUC",names(lrm_final_cm$byClass)),
                         Values = c(as.numeric(auc(lrm_final_roc)),
                                    as.numeric(lrm_final_cm$byClass)))

# Getting the coefficients of the logistic regression model
flrm_coef = coef(lrm_final$finalModel,s = lrm_final$bestTune$lambda)

# Creating a data frame of results
flrm_coef_dt = data.table(Variables = rownames(flrm_coef),
                          Coefficients = as.numeric(flrm_coef[,1]))

# Variable importance of logistic regression
lrm_VI = varImp(lrm_final, scale = FALSE)$importance

# Extracting the row names
vi_names_lr = rownames(lrm_VI)

# Creating a data table from the results
VI_lrm = data.table(var = vi_names_lr,
                   rel.inf = lrm_VI$Overall)

### Exploratory Analysis
# Reading in the depression and exercise dataset
DE = data.table(readRDS("Depression_Exercise.rds"))

# Dimensions of the dataset
dim(DE)

# Looking for columns that have (character(0))
contains_character0 = DE[,sapply(.SD,function (x){
  holder = any(grepl("\\(character\\(0\\)\\)|character\\(0\\)", x))
  return(holder)
} )]

# Columns names that contain pattern
col_character0 = names(DE)[contains_character0]

# Replacing character(0) with ""
DE$age_range_or_sd =  gsub("\\(character\\(0\\)\\)|character\\(0\\)", "",DE$age_range_or_sd)

# Replacing blank entries with NA
DE$age_range_or_sd = ifelse(DE$age_range_or_sd== "", NA,DE$age_range_or_sd)

# Replacing character(0) with ""
DE$age =  gsub("\\(character\\(0\\)\\)|character\\(0\\)",
               "",DE$age)

# Replacing blank entries with NA
DE$age = ifelse(DE$age== "", NA, DE$age)

# missing_val_func is sourced from script in R folder
missing_data_list = DE[,lapply(.SD,missing_val_func)]

# Creating data.table with total missing and percentage
missing_dt = data.table(
  columns = names(missing_data_list),
  total_missing = unlist(missing_data_list[1,]),
  percent_missing = unlist(missing_data_list[2,])
)

# Plot of missing values
ggplot(missing_dt, aes(x = columns, y = percent_missing)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Missing Values per Column", x = " ", y = "Total") +
  coord_flip()

# Distribution of Baseline severity
baseline_dt = DE[,.(total = sum(pre_n)),
                 keyby = baseline_severity]

# Bar chart of baseline severity
ggplot(baseline_dt, aes(x = baseline_severity,
                        y = total)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Barchart of Baseline-Severity",
       x = "Depression Category",
       y = "total")

# Treatment barchart
trt_dt = DE[class == "Exercise",.(total = sum(pre_n)),
            keyby = trt]

# Class Treatment
class_dt = DE[,.(total = .N),
              keyby = class]


# Creating a subset
mean_scores_dt = DE[,.(pre_mean,mean)]

names(mean_scores_dt) = c("Pre-Intervention Mean", "Post-Intervention Mean")

# Casting to long
mean_scores_long = melt(mean_scores_dt,
                        variable.name = "Variable",
                        value.name = "Value")

# Density plot
ggplot(mean_scores_long, aes(x =Value, fill = Variable)) +
  geom_density()  +
  labs(title = "Distributions of Pre/Post-Intervention Depression Scores",
       x = "Score")

# Male and female data frame
mf_dt = DE[,.(pc_female)]

# Creating a new column percentage male
mf_dt$pc_male = 100 - mf_dt$pc_female

# Casting to long
mf_long = melt(mf_dt,
               variable.name = "Variable",
               value.name = "Value")

# Distribution of Male and Female participants
ggplot(mf_long, aes(x = Value, fill = Variable)) +
  geom_boxplot() +
  labs(title = "Boxplot of Male and Female Percentages",
       x = "Percentages")

# Getting current swatch inorder to add new colors
swatch_colors = ggthemr::swatch()

# Expanding swatch colors
new_swatch_colors = c(swatch_colors ,
                      "#800000",
                      "#008000",
                      "#000080")

# Setting the new swatch colors
set_swatch(new_swatch_colors)

# Adding new column
DE$pc_male = 100 - DE$pc_female

# Creating a scatter plot of women and pre_mean scores
ggplot(DE, aes(x = pc_male, y= pre_mean, color = class)) +
  geom_point() +
  labs(title = "Male Percentages ~ Pre Intervention Depression Score",
       x = "Male Percentage",
       y = "Depression Score")

# Creating a scatter plot of women and pre_mean scores
ggplot(DE, aes(x = sessions, y= mean, color = baseline_severity)) +
  geom_point() +
  labs(title = "Sessions ~ Post Intervention Depression Score",
       x = "Sessions",
       y = "Depression Score")

# Boxplot of post-intervention scores and sessions
ggplot(DE, aes(x = mean, fill = factor(sessions))) +
  geom_boxplot() +
  labs(title = "Mean Post-Intervention Depression Score",
       x = "Treatment Sessions",
       y = "Mean Depression Score",
       fill = "Treatment Sessions") +
  coord_flip() 

# Creating a scatter plot of women and pre_mean scores
ggplot(DE, aes(x = daily_dose, y= mean, color = class)) +
  geom_point() +
  labs(title = "Daily-Dose ~ Mean Post-Intervention Score",
       x = "Daily Dose",
       y = "Mean Depression Score")

# Boxplots of post-intervention scores according to depression severity
ggplot(DE, aes(x = pre_mean, fill = factor(baseline_severity))) +
  geom_boxplot() +
  labs(title = "Mean Pre-Intervention Depression Score",
       x = "Depression Severity",
       y = "Mean Depression Score",
       fill = "Depression Severity") +
  coord_flip() 

# Boxplots of post-intervention scores according to depression severity
ggplot(DE, aes(x = mean_diff, fill = factor(baseline_severity))) +
  geom_boxplot() +
  labs(title = "Boxplots of Difference of Means",
       x = "Baseline Severity",
       y = "Difference in Means",
       fill = "Depression Severity") +
  coord_flip() 

# Boxplots of post-intervention scores according to class
ggplot(DE, aes(y = mean_diff, 
               x = factor(class),
               fill = factor(class))) +
  geom_violin(trim = FALSE) +  # Violin plot
  stat_summary(fun = median,
               geom = "point",
               shape = 23, 
               size = 2, color
               = "black",
               fill = "white") + 
  labs(title = "Violin Plot of Mean Difference",
       x = "Treatment Class",
       y = "Mean Difference",
       fill = "Treatment Class")

# Getting a subset of the classes that have more than 2 groups
less_than3 = DE[, .(sample_size = .N), by = trt][sample_size < 3,]$trt
DE_exercise = DE[class == "Exercise" & !(trt %in% less_than3),]

# QQ plots within each group
ggplot(DE_exercise, aes(sample = mean_diff)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ trt, scales = "free")

# Histograms
ggplot(DE_exercise, aes(x = mean_diff)) +
  geom_histogram() +
  facet_wrap(~ trt, scales = "free")

# Shapiro Wilk test
shapiro_test = DE_exercise[, .(p_value = shapiro.test(mean_diff)$p.value), by = trt]

# Changing names for displaying
names(shapiro_test) = c("Exercise Treatment",
                        "P-Value")

# Displaying results of test
knitr::kable(shapiro_test,booktabs=TRUE, 
             format="latex",
             caption = "Shapiro-Wilk Test")%>%
  kable_styling(latex_options = "HOLD_position")

# Checking if all mean_diff are negative
krus_test = kruskal.test(mean_diff ~ trt, data = DE_exercise)
krus_test

### Post-Hoc Analysis
# Multiple comparisons with wilcox test among groups
suppressWarnings({
pairwise.wilcox.test(DE_exercise$mean_diff,
                     DE_exercise$trt,
                     p.adjust.method = "bonferroni")
})

# Eta-Squared calculation
h = as.numeric(krus_test$statistic)
n = length(DE_exercise$mean_diff)
k = length(unique(DE_exercise$trt))

# Calculating eta squared effect size
eta_squared = (h-k+1)/(n-k)

# Checking if all mean_diff are negative
krus_test_class = kruskal.test(mean_diff ~ trt, data = DE_exercise)
krus_test_class

# Multiple comparisons with Wilcox test among class
suppressWarnings({
post_hoc_class = pairwise.wilcox.test(DE$mean_diff,
                                      DE$class,
                                      p.adjust.method = "bonferroni")
})


# Eta-Squared calculation for class
h_class = as.numeric(krus_test_class$statistic)
n_class = length(DE$mean_diff)
k_class = length(unique(DE$class))

# Calculating eta squared effect size
eta_squared_class = (h_class-k_class+1)/(n_class-k_class)
eta_squared_class

# Plot to check for interactions
ggplot(DE, aes(x = trt, y = mean_diff, color = baseline_severity)) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = baseline_severity)) +
  labs(title = "Interaction Plot",
       x = "Treatment",
       y = "Mean Difference in Depression Scores",
       color = "Baseline Severity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Fitting a generalized linear model to assess interaction 
glm1 = glm(mean_diff ~trt*baseline_severity,
           data = DE, 
           family = gaussian())

# Summary of the model
glm_summ = summary(glm1)

# Filtering out only significant rows since output is large
glm_coef = glm_summ$coefficients

# Making it a data table
glm_coef = data.frame(glm_coef)

# Getting the significant interactions
significant_interactions = glm_coef[glm_coef$`Pr...t..` < 0.05,]  

# Changing the names of the table
names(significant_interactions) = c("estimate",
                                    "std. Error",
                                    "t-value" ,
                                    "p-value")
