#*Churn* 
#The goal of the Churn models is to determine, for each customer, a probability of abandonment / termination of relationships
#The procedure are: 
#  1. choosing a reference date in the past:
  
# Reference Date: 01/01/2019

churn_study_period <- df_7_tic_clean_final %>%
                        filter(DIREZIONE == 1,
                               TIC_DATE < as.Date("01/01/2019",
                                                 format = "%d/%m/%Y"),
                               TIC_DATE > as.Date("01/10/2018",
                                                 format = "%d/%m/%Y"))
churn_study_period

#2  Imposing the length (frequencey of the distribution and/or the purchase time scale) of an holdout period after each reference date:
  
# Holdout Period: 28/02/2019
churn_holdout <- df_7_tic_clean_final %>%
                  filter(DIREZIONE == 1,
                          TIC_DATE < as.Date("28/02/2019",
                                             format = "%d/%m/%Y"),
                          TIC_DATE > as.Date("01/01/2019",
                                             format = "%d/%m/%Y"))

no_churner <- unique(churn_holdout$ID_CLI)
# Choosing the length of a lookback period before the reference date (No constraints to define this length,
                                                                    #but it should be taken into account the
                                                                    #purchase time scale)

#3 Choosing the lenght of a lookback period before the reference date:
  
# Lookback Period: 3 months

#Recency to Merge
churn_recency <- churn_study_period %>%
                  filter(DIREZIONE == 1) %>%
                  group_by(ID_CLI) %>%
                  summarise(LAST_PURCHASE_DATE = max(TIC_DATE))

churn_recency$RECENCY <- difftime(as.Date("01/01/2019",
                                          format = "%d/%m/%Y"),          #-- Recency
                                  churn_recency$LAST_PURCHASE_DATE,
                                  units = "days")

churn_recency
# Frequency to Merge
churn_frequency <- churn_study_period %>%
                    filter(DIREZIONE == 1) %>%
                    group_by(ID_CLI) %>%
                    summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>%
                    arrange(desc(TOT_PURCHASE))
churn_frequency
# Monetary to Merge
churn_monetary <- churn_study_period %>%
                   filter(DIREZIONE == 1) %>%
                   group_by(ID_CLI) %>%
                   summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO),
                              SCONTO = sum(SCONTO),
                              SPESA = IMPORTO_LORDO - SCONTO) %>%
                   ungroup() %>%
                   as.data.frame() %>%
                   arrange(desc(IMPORTO_LORDO))
churn_monetary

churn <- merge(churn_recency, churn_frequency, by = "ID_CLI")
churn <- merge(churn, churn_monetary, by = "ID_CLI") %>%
          select(ID_CLI,
                 RECENCY,
                 SPESA, 
                 TOT_PURCHASE)
churn

#4 Assigning to each customer a target 0/1 variable such that 1 is assigned to customers who churned in the holout period:
  
churn$CHURN <- 1

for (i in c(1:nrow(churn))){
  if (churn$ID_CLI[i] %in% no_churner) churn$CHURN[i] <- 0
}
churn$CHURN <- as.factor(churn$CHURN)

table(churn$CHURN)


#5 Defining a set of potentially relevant predictors variables to be computed within the lookback period:
  
#* RECENCY;
#* SPESA;
#* TOT_PURCHASE;
#* REGION;
#* LAST_COD_FID;
#* TYP_JOB.


churn <- left_join(churn, df_2_cli_account_clean[, c("ID_CLI", "TYP_JOB")], by = "ID_CLI")  #-- Add Type Job
churn <- left_join(churn, df_1_cli_fid_clean[, c("ID_CLI", "LAST_COD_FID")], by = "ID_CLI") #-- Add Type of Fidelity Card
region <- left_join(df_2_cli_account_clean[, c("ID_CLI", "ID_ADDRESS")],
                    df_3_cli_address_clean[, c("ID_ADDRESS", "REGION")], by = "ID_ADDRESS") #-- Add Region
churn <- left_join(churn, region, by = "ID_CLI")
churn <- churn[, -8]
head(churn)
# Model training and testing
  
#Training and testing partitions are created using 70%- 30% of total data respectively.


churn <- na.omit(churn)


train_index <- createDataPartition(churn$CHURN, 
                                   p = .70, 
                                   list = FALSE, 
                                   times = 1)

#-- Train Test Split
train <- churn[train_index,]
test <- churn[-train_index,]

table(train$CHURN)


#Model trained are
  
# sismple tree
# random forest
# logistic regression
# lasso
# neural network
#tree
tree <- rpart(CHURN ~ RECENCY + SPESA + TOT_PURCHASE + REGION + TYP_JOB,
              data = train)

rpart.plot(tree, extra = "auto")

summary(tree) 
printcp(tree) 
#random forest

random_forest <- randomForest(CHURN ~ RECENCY + SPESA + TOT_PURCHASE + REGION + TYP_JOB,
                        data = train, ntree = 100)

print(random_forest )


#Logistic regression
logistic <- train(CHURN ~ RECENCY + SPESA + TOT_PURCHASE + REGION + TYP_JOB,
                  data = train,
                  method = "glm")
logistic
summary(logistic)

#Lasso
lasso <- train(CHURN ~ RECENCY + SPESA + TOT_PURCHASE + REGION + TYP_JOB,
               data = train,
               method = "glmnet",
               family = "binomial")
lasso
plot(lasso)

#Neural network
neural_network <- randomForest(CHURN ~ RECENCY+ SPESA + TOT_PURCHASE + REGION +TYP_JOB,
                     data = train, size = 2, rang = 0.1,
                     decay = 5e-4, maxit = 200)
print(neural_network )

plot(neural_network )
#Models evaluation
  
#Model's confusions matrix are computed. Accuracy metric is used to compare models performances on test set.
## Prediction
#tree

pred <- predict(tree, test[, -5], type = "class")
p1 <- unlist(pred)
confusionMatrix(p1, test$CHURN)

#RF
pred_rf <- predict(random_forest , test[,-5], type = "class")
confusionMatrix(pred_rf, test$CHURN)

#Logistic regression
pred_logistic <- predict(logistic, test[, -5], type = "raw")
confusionMatrix(pred_logistic, test$CHURN)

#Lasso
pred_lasso <- predict(lasso, test[,-5], type = "raw")
confusionMatrix(pred_lasso, test$CHURN)

#Neural network
pred_nnet<-predict(neural_network , test[-5],type = "class")
confusionMatrix(pred_nnet, test$CHURN)

accuracy <- as.data.frame(t(cbind(confusionMatrix(pred_lasso, test$CHURN)$overall[1],
      confusionMatrix(pred_logistic, test$CHURN)$overall[1],
      confusionMatrix(pred_rf , test$CHURN)$overall[1],
      confusionMatrix(pred_nnet, test$CHURN)$overall[1],
      confusionMatrix(pred, test$CHURN)$overall[1])))
accuracy

accuracy <- as.data.frame(cbind(c("Lasso", "Logistic","Rf","Nnet","Tree"),
                                accuracy))

colnames(accuracy) <- c("Models", "Accuracy")

ggplot(data = accuracy,
         aes(x = Models,
             y = Accuracy,
             fill = Models)) +
    geom_bar(stat = "identity") +
    coord_cartesian(ylim = c(0.681, 0.693)) +
    theme_minimal() +
    guides(fill = FALSE) +
    labs(title = "Accurancy",
         x = "Models",
         y = " ") +
    scale_fill_manual(values = c("#FF1053","#6C6EA0","#66C7F4","#FF1C00","orange")) +
    theme(plot.title = element_text(hjust = 0.5))
    

plot(accuracy$Accuracy)

  
# Probability
p_tree = predict(tree, test[,-5], "prob")[,1]
p_rf = predict(random_forest, test[,-5], "prob")[,1]
p_log = predict(logistic, test[,-5], "prob")[,1]
p_lasso = predict(lasso, test[,-5], "prob")[,1]
p_nnet = predict(neural_network , test[,-5], "prob")[,1]
# Data Frame
data_class = as.data.frame(cbind(p_tree, p_rf, p_log, p_lasso, p_nnet))
data_class = cbind(data_class, test$CHURN)
colnames(data_class) <- c("p_tree", "p_rf", "p_log", "p_lasso","p_nnet", "churn")
head(data_class)
# Curve Lift
lift_tree = gain_lift(data = data_class, score = 'p_tree', target = 'churn')
lift_rf = gain_lift(data = data_class, score = 'p_rf', target = 'churn')
lift_log = gain_lift(data = data_class, score = 'p_log', target = 'churn')
lift_lasso = gain_lift(data = data_class, score = 'p_lasso', target = 'churn')
lift_nnet=gain_lift(data = data_class, score = 'p_nnet', target = 'churn')


# Score on the future using best model "logistic"
# Reference Date: 01/01/2019
new_churn_study_period <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1,
         TIC_DATE < as.Date("30/04/2019",
                            format = "%d/%m/%Y"),
         TIC_DATE > as.Date("01/02/2019",
                            format = "%d/%m/%Y"))
# Recency to Merge
new_churn_recency <- new_churn_study_period %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(LAST_PURCHASE_DATE = max(TIC_DATE))

new_churn_recency$RECENCY <- difftime(as.Date("30/04/2019",
                                              format = "%d/%m/%Y"),          #-- Recency
                                      new_churn_recency$LAST_PURCHASE_DATE,
                                      units = "days")
# Frequency to Merge
new_churn_frequency <- new_churn_study_period %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>%
  arrange(desc(TOT_PURCHASE))
# Monetary to Merge
new_churn_monetary <- new_churn_study_period %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO),
            SCONTO = sum(SCONTO),
            SPESA = IMPORTO_LORDO - SCONTO) %>%
  ungroup() %>%
  as.data.frame() %>%
  arrange(desc(IMPORTO_LORDO))
new_churn <- merge(new_churn_recency, new_churn_frequency, by = "ID_CLI")
new_churn <- merge(new_churn, new_churn_monetary, by = "ID_CLI") %>%
  select(ID_CLI,
         RECENCY,
         SPESA, 
         TOT_PURCHASE)
new_churn
new_churn <- left_join(new_churn, df_2_cli_account_clean[, c("ID_CLI", "TYP_JOB")],
                       by = "ID_CLI")  #-- Add Type Job
new_churn <- left_join(new_churn, df_1_cli_fid_clean[, c("ID_CLI", "LAST_COD_FID")],
                       by = "ID_CLI") #-- Add Type of Fidelity Card
region <- left_join(df_2_cli_account_clean[, c("ID_CLI", "ID_ADDRESS")],
                    df_3_cli_address_clean[, c("ID_ADDRESS", "REGION")],
                    by = "ID_ADDRESS") #-- Add Region
new_churn <- left_join(new_churn, region, by = "ID_CLI")
new_churn <- new_churn[, -7]
new_churn
new_churn <- na.omit(new_churn)
new_churn$prob_to_churn <- predict(logistic, new_churn, type = "prob")[,2]




