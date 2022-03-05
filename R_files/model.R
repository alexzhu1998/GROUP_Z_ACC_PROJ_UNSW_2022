load("data/model.RData")
load("data/model2.RData")


library(dplyr)
library(gbm)
# LINEAR REGRESSION


cols_to_remove <- c("Pos_new","Player","Nation","League","Squad")
pos_levels <- c("MF","DF","FW")

for (level in pos_levels) {
    eval(call("<-",paste0(level,"_nonRFL"),
              nonRFL %>% filter(Pos_new == level) %>% select(-cols_to_remove)))
    eval(call("<-",paste0(level,"_RFL"),
              RFL %>% filter(Pos_new == level) %>% select(-cols_to_remove)))
    eval(call("<-",paste0(level,"_nonRFL_mod"),
              glm(Annualized_Salary ~ ., data = eval(str2lang(paste0(level,"_nonRFL"))))))
    
    Predicted_Sal<- predict(eval(str2lang(paste0(level,"_nonRFL_mod"))),newdata = eval(str2lang(paste0(level,"_RFL"))))
    eval(call("<-",paste0(level,"_RFL"),
              cbind(get(paste0(level,"_RFL")),Predicted_Sal)))
    
    Diff <- eval(str2lang(paste0(level,"_RFL")))["Predicted_Sal"] - eval(str2lang(paste0(level,"_RFL")))["Annualized_Salary"]
    names(Diff) <- "Diff"
    eval(call("<-",paste0(level,"_RFL"),
              cbind(get(paste0(level,"_RFL")),Diff)))
    
    
}


plot(MF_RFL$Annualized_Salary,MF_RFL$Predicted_Sal, main= "MF RFL")
plot(DF_RFL$Annualized_Salary,DF_RFL$Predicted_Sal, main= "DF RFL")
plot(FW_RFL$Annualized_Salary,FW_RFL$Predicted_Sal, main= "FW RFL")


MF_RFL %>% arrange(Diff,descending = T)
DF_RFL %>% arrange(Diff,descending = T)
FW_RFL %>% arrange(Diff,descending = T)


## DO NOT CHANGE COLUMN ORDER -> CHANGING COLUMN ORDER WILL REQUIRE THE FOLLOWING CODE TO BE MODIFIED ##

#Fit boosting model on annualised salary dataset with non RFL football leagues and ALL predictors
gbmFit.param <- gbm(Annualized_Salary ~., data = df[df['League'] != "RFL",-c(1,2,3,4,5,71)], distribution = "gaussian", cv.fold = 10, n.trees = 10000, interaction.depth = 1, shrinkage = 0.01)
gbmFit.param

min <- which.min(gbmFit.param$cv.error)
min
gbm.perf(gbmFit.param, method = "cv")

gbmFit <- gbm(Annualized_Salary ~., df[df['League'] != "RFL",-c(1,2,3,4,5,71)], distribution = "gaussian", n.trees = min, interaction.depth = 1, shrinkage = 0.01)

summary(gbmFit)

gbm.predict = predict(gbmFit, newdata = df[,-c(1,2,3,4,5,70,71)], n.trees = min, type = "response")


#New dataframe with correlated vairables removed + identifiable attributes
cor_df_merge <- cbind(cor_df,df[,c("Player","Nation","Pos_new","League","Squad")])

#MF model
gbmFit.param_MF <- gbm(Annualized_Salary ~., data = cor_df_merge[(cor_df_merge['League'] != "RFL") & (cor_df_merge['Pos_new'] == "MF"),-c(19,20,21,22,23)], distribution = "gaussian", cv.fold = 10, n.trees = 10000, interaction.depth = 1, shrinkage = 0.01)
gbmFit.param_MF

min_MF <- which.min(gbmFit.param_MF$cv.error)
min_MF
gbm.perf(gbmFit.param_MF, method = "cv")

gbmFit_MF <- gbm(Annualized_Salary ~., cor_df_merge[(cor_df_merge['League'] != "RFL") & (cor_df_merge['Pos_new'] == "MF"),-c(19,20,21,22,23)], distribution = "gaussian", n.trees = min_MF, interaction.depth = 1, shrinkage = 0.01)

summary(gbmFit_MF)

gbm.predict_MF = predict(gbmFit_MF, newdata = cor_df_merge[(cor_df_merge['Pos_new'] == "MF"),-c(17, 19,20,21,22,23)], n.trees = min_MF, type = "response")
hist(gbm.predict_MF)

#DF model
gbmFit.param_DF <- gbm(Annualized_Salary ~., data = cor_df_merge[(cor_df_merge['League'] != "RFL") & (cor_df_merge['Pos_new'] == "DF"),-c(19,20,21,22,23)], distribution = "gaussian", cv.fold = 10, n.trees = 10000, interaction.depth = 1, shrinkage = 0.01)
gbmFit.param_DF

min_DF <- which.min(gbmFit.param_DF$cv.error)
min_DF
gbm.perf(gbmFit.param_DF, method = "cv")

gbmFit_DF <- gbm(Annualized_Salary ~., cor_df_merge[(cor_df_merge['League'] != "RFL") & (cor_df_merge['Pos_new'] == "DF"),-c(19,20,21,22,23)], distribution = "gaussian", n.trees = min_DF, interaction.depth = 1, shrinkage = 0.01)

summary(gbmFit_DF)

gbm.predict_DF = predict(gbmFit_DF, newdata = cor_df_merge[(cor_df_merge['Pos_new'] == "DF"),-c(17, 19,20,21,22,23)], n.trees = min_DF, type = "response")
hist(gbm.predict_DF)

#FW model
gbmFit.param_FW <- gbm(Annualized_Salary ~., data = cor_df_merge[(cor_df_merge['League'] != "RFL") & (cor_df_merge['Pos_new'] == "FW"),-c(19,20,21,22,23)], distribution = "gaussian", cv.fold = 10, n.trees = 10000, interaction.depth = 1, shrinkage = 0.01)
gbmFit.param_FW

min_FW <- which.min(gbmFit.param_FW$cv.error)
min_FW
gbm.perf(gbmFit.param_FW, method = "cv")

gbmFit_FW <- gbm(Annualized_Salary ~., cor_df_merge[(cor_df_merge['League'] != "RFL") & (cor_df_merge['Pos_new'] == "FW"),-c(19,20,21,22,23)], distribution = "gaussian", n.trees = min_FW, interaction.depth = 1, shrinkage = 0.01)

summary(gbmFit_FW)

gbm.predict_FW = predict(gbmFit_FW, newdata = cor_df_merge[(cor_df_merge['Pos_new'] == "FW"),-c(17, 19,20,21,22,23)], n.trees = min_FW, type = "response")
hist(gbm.predict_FW)

#Actual salary histograms
hist(df$Annualized_Salary[(df['League'] != "RFL") & (df['Pos_new'] == "MF")], breaks = 20)
hist(df$Annualized_Salary[(df['League'] != "RFL") & (df['Pos_new'] == "DF")], breaks = 20)
hist(df$Annualized_Salary[(df['League'] != "RFL") & (df['Pos_new'] == "FW")], breaks = 20)