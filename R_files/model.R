load("data/model.RData")
load("data/model2.RData")


library(dplyr)
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


# GBM
gbmFit.param <- gbm(Annualized_Salary ~., data = df[,-c(1,2,3,4,5,71)], distribution = "gaussian", cv.fold = 10, n.trees = 10000, interaction.depth = 1, shrinkage = 0.01)
gbmFit.param

min <- which.min(gbmFit.param$cv.error)
min
gbm.perf(gbmFit.param, method = "cv")

gbmFit <- gbm(Annualized_Salary ~., data = df[,-c(1,2,3,4,5,71)], distribution = "gaussian", n.trees = min, interaction.depth = 1, shrinkage = 0.01)

summary(gbmFit)

#With correlated variables removed
keep <- c('Age','Tackles_Tkl','Vs_Dribbles_Att','Pressures_%','Blocks_Sh',
          'Int','Clr','Total_Cmp%','xA','Standard_SoT/90',
          'Standard_Sh/90','Standard_G/SoT','Standard_Dist','Standard_FK',
          'Performance_PK','Expected_xG','Annualized_Salary','90s_avg')
#
for (level in pos_levels) {
    pos_df <- nonRFL %>% filter(Pos_new == level)
    
    gbmFit.param <- gbm(Annualized_Salary ~., data = pos_df[,keep], distribution = "gaussian", cv.fold = 10, n.trees = 10000, interaction.depth = 1, shrinkage = 0.01)
    gbmFit.param
    
    min <- which.min(gbmFit.param$cv.error)
    min
    gbm.perf(gbmFit.param, method = "cv")
    
    gbmFit <- gbm(Annualized_Salary ~., data = pos_df[,keep], distribution = "gaussian", n.trees = min, interaction.depth = 1, shrinkage = 0.01)
    
    summary(gbmFit)
    
}


