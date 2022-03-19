load("data/model.RData")
load("data/model2.RData")
load("data/cor_df.RData")
load("data/tourn_merge.Rdata")
load("data/gk_df.RData")
load("data/gk_tourn_df.RData")
load("data/tournament_result.RData")

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
set.seed(1)
colnames(df)[c(1,2,3,4,5,71)]

gbmFit.param <- gbm(Annualized_Salary ~., data = df[df['League'] != "RFL",-c(1,2,3,4,5,71)], distribution = "gaussian", cv.fold = 10, n.trees = 10000, interaction.depth = 1, shrinkage = 0.01)
gbmFit.param

min <- which.min(gbmFit.param$cv.error)
min
gbm.perf(gbmFit.param, method = "cv")

gbmFit <- gbm(Annualized_Salary ~., df[df['League'] != "RFL",-c(1,2,3,4,5,71)], distribution = "gaussian", n.trees = min, interaction.depth = 1, shrinkage = 0.01)

summary(gbmFit)

gbm.predict = predict(gbmFit, newdata = df[,-c(1,2,3,4,5,70,71)], n.trees = min, type = "response")


plot(gbm.predict[(df['League'] != "RFL")], df$Annualized_Salary[(df['League'] != "RFL")])
plot(gbm.predict[(df['League'] == "RFL")], df$Annualized_Salary[(df['League'] == "RFL")])

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
gbm.predict_MF = predict(gbmFit_MF, newdata = cor_df_merge[,-c(17, 19,20,21,22,23)], n.trees = min_MF, type = "response")

#Comparing actual vs expected in MF model
hist(gbm.predict_MF[(cor_df_merge['Pos_new'] == "MF") & (df['League'] != "RFL")])
hist(df$Annualized_Salary[(df['League'] != "RFL") & (df['Pos_new'] == "MF")], breaks = 20)

plot(gbm.predict_MF[(df['League'] == "RFL") & (cor_df_merge['Pos_new'] == "MF")], df$Annualized_Salary[(df['League'] == "RFL") & (cor_df_merge['Pos_new'] == "MF")])
plot(gbm.predict_MF[(df['League'] != "RFL") & (cor_df_merge['Pos_new'] == "MF")], df$Annualized_Salary[(df['League'] != "RFL") & (cor_df_merge['Pos_new'] == "MF")])

colnames(cor_df_merge)[c(17,19,20,21,22,23)]

#DF model
gbmFit.param_DF <- gbm(Annualized_Salary ~., data = cor_df_merge[(cor_df_merge['League'] != "RFL") & (cor_df_merge['Pos_new'] == "DF"),-c(19,20,21,22,23)], distribution = "gaussian", cv.fold = 10, n.trees = 10000, interaction.depth = 1, shrinkage = 0.01)
gbmFit.param_DF

min_DF <- which.min(gbmFit.param_DF$cv.error)
min_DF
gbm.perf(gbmFit.param_DF, method = "cv")

gbmFit_DF <- gbm(Annualized_Salary ~., cor_df_merge[(cor_df_merge['League'] != "RFL") & (cor_df_merge['Pos_new'] == "DF"),-c(19,20,21,22,23)], distribution = "gaussian", n.trees = min_DF, interaction.depth = 1, shrinkage = 0.01)

summary(gbmFit_DF)

gbm.predict_DF = predict(gbmFit_DF, newdata = cor_df_merge[,-c(17, 19,20,21,22,23)], n.trees = min_DF, type = "response")

#Comparing actual vs expected in DF model
hist(gbm.predict_DF[(cor_df_merge['Pos_new'] == "DF") & (df['League'] != "RFL")])
hist(df$Annualized_Salary[(df['League'] != "RFL") & (df['Pos_new'] == "DF")], breaks = 20)

plot(gbm.predict_MF[(df['League'] == "RFL") & (cor_df_merge['Pos_new'] == "DF")], df$Annualized_Salary[(df['League'] == "RFL") & (cor_df_merge['Pos_new'] == "DF")])
plot(gbm.predict_MF[(df['League'] != "RFL") & (cor_df_merge['Pos_new'] == "DF")], df$Annualized_Salary[(df['League'] != "RFL") & (cor_df_merge['Pos_new'] == "DF")])

#FW model
gbmFit.param_FW <- gbm(Annualized_Salary ~., data = cor_df_merge[(cor_df_merge['League'] != "RFL") & (cor_df_merge['Pos_new'] == "FW"),-c(19,20,21,22,23)], distribution = "gaussian", cv.fold = 10, n.trees = 10000, interaction.depth = 1, shrinkage = 0.01)
gbmFit.param_FW

min_FW <- which.min(gbmFit.param_FW$cv.error)
min_FW
gbm.perf(gbmFit.param_FW, method = "cv")

gbmFit_FW <- gbm(Annualized_Salary ~., cor_df_merge[(cor_df_merge['League'] != "RFL") & (cor_df_merge['Pos_new'] == "FW"),-c(19,20,21,22,23)], distribution = "gaussian", n.trees = min_FW, interaction.depth = 1, shrinkage = 0.01)

summary(gbmFit_FW)

gbm.predict_FW = predict(gbmFit_FW, newdata = cor_df_merge[,-c(17,19,20,21,22,23)], n.trees = min_FW, type = "response")

#Comparing actual vs expected in FW model
hist(gbm.predict_FW[(cor_df_merge['Pos_new'] == "FW") & (df['League'] != "RFL")])
hist(df$Annualized_Salary[(df['League'] != "RFL") & (df['Pos_new'] == "FW")], breaks = 20)

plot(gbm.predict_FW[(df['League'] == "RFL") & (cor_df_merge['Pos_new'] == "FW")], df$Annualized_Salary[(df['League'] == "RFL") & (cor_df_merge['Pos_new'] == "FW")])
plot(gbm.predict_FW[(df['League'] != "RFL") & (cor_df_merge['Pos_new'] == "FW")], df$Annualized_Salary[(df['League'] != "RFL") & (cor_df_merge['Pos_new'] == "FW")])


#GK model
gbmFit.param_GK <- gbm(Annualized_Salary ~., data = gk_df[(gk_df['League'] != "RFL"),-c(16,17,18,19,20)], distribution = "gaussian", cv.fold = 10, n.trees = 10000, interaction.depth = 1, shrinkage = 0.01)
gbmFit.param_GK

min_GK <- which.min(gbmFit.param_GK$cv.error)
min_GK
gbm.perf(gbmFit.param_GK, method = "cv")

gbmFit_GK <- gbm(Annualized_Salary ~., data = gk_df[(gk_df['League'] != "RFL"),-c(16,17,18,19,20)], distribution = "gaussian", cv.fold = 10, n.trees = min_GK, interaction.depth = 1, shrinkage = 0.01)
summary(gbmFit_GK)

gbm.predict_GK = predict(gbmFit_GK, newdata = gk_df[,-c(15,16,17,18,19,20)], n.trees = min_GK, type = "response")

#Comparing actual vs expected in GK model
plot(gbm.predict_GK[(df['League'] == "RFL")], gk_df$Annualized_Salary[(df['League'] == "RFL")])
plot(gbm.predict_GK[(df['League'] != "RFL")], gk_df$Annualized_Salary[(df['League'] != "RFL")])



#Actual salary histograms
hist(df$Annualized_Salary[(df['League'] != "RFL") & (df['Pos_new'] == "DF")], breaks = 20)
hist(df$Annualized_Salary[(df['League'] != "RFL") & (df['Pos_new'] == "FW")], breaks = 20)
hist(df$Annualized_Salary[(df['League'] != "RFL") & (df['Pos_new'] == "MF")], breaks = 20)
hist(gk_df$Annualized_Salary[(gk_df['League'] != "RFL")], breaks = 20)


## Apply model to tournament data, obtain national team "scores"

gbm.tourn_MF = predict(gbmFit_MF, newdata = cor_tourn_merge[,-c(17,19,20,21,22,23)], n.trees = min_MF, type = "response")
gbm.tourn_DF = predict(gbmFit_DF, newdata = cor_tourn_merge[,-c(17,19,20,21,22,23)], n.trees = min_DF, type = "response")
gbm.tourn_FW = predict(gbmFit_FW, newdata = cor_tourn_merge[,-c(17,19,20,21,22,23)], n.trees = min_FW, type = "response")
gbm.tourn_GK = predict(gbmFit_GK, newdata = gk_tourn_df[,-c(1,2,3,4)], n.trees = min_GK, type = "response")

MF_tourn <- cbind(cor_tourn_merge[,c(19,20,21)], gbm.tourn_MF)
MF_tourn_df <- MF_tourn %>%
    filter(Pos_new == "MF")

MF_stats <- MF_tourn_df %>%
    group_by(Nation) %>%
    summarise(MF_Score = mean(gbm.tourn_MF))
MF_stats %>% arrange(MF_Score,descending = T)

DF_tourn <- cbind(cor_tourn_merge[,c(19,20,21)], gbm.tourn_DF)
DF_tourn_df <- DF_tourn %>%
    filter(Pos_new == "DF")

DF_stats <- DF_tourn_df %>%
    group_by(Nation) %>%
    summarise(DF_Score = mean(gbm.tourn_DF))
DF_stats %>% arrange(DF_Score,descending = T)

FW_tourn <- cbind(cor_tourn_merge[,c(19,20,21)], gbm.tourn_FW)
FW_tourn_df <- FW_tourn %>%
    filter(Pos_new == "FW")

FW_stats <- FW_tourn_df %>%
    group_by(Nation) %>%
    summarise(FW_Score = mean(gbm.tourn_FW))
FW_stats %>% arrange(FW_Score,descending = T)

GK_tourn_df <- cbind(gk_tourn_df[,c(1,2)], gbm.tourn_GK)
GK_stats <- GK_tourn_df %>%
    group_by(Nation) %>%
    summarise(GK_Score = mean(gbm.tourn_GK))
GK_stats %>% arrange(GK_Score, descending = T)

colnames(PLAYER_tourn_res_2021)[2] <- "Nation"
team_stats <- merge(merge(merge(merge(MF_stats, DF_stats), FW_stats), GK_stats),PLAYER_tourn_res_2021)
total_score <- team_stats$FW_Score*2/11 + team_stats$MF_Score*4/11 + team_stats$DF_Score*4/11 + team_stats$GK_Score*1/11

team_stats <- cbind(team_stats,total_score)

plot(team_stats$`2021 Tournament Place`, team_stats$total_score)
#PREDICT SALARY FROM PPL IN THE TOURNAMENT USING predict
#Group by tournament team, average salary for each role (FW, MF, DF, GK score, overall average sal) (relo between scores and placement - classification model)
#Either clasify ranks directly, 1 beats 2,3,4,5 -> Response Win/Loss (these attributes beat other attributes -> "Win" -> 60% chance of win

# 1 vs (2 or 3)// 1 vs (4 or 5)// 1 vs 6,7,8, or 9, 1 vs 
#Pr(At least one win) -> mess around with this probability to be convincing

#Pick a bunch of players that we look at data and say "that might be economical" -> score -> spit out probability -> threshold for our object



# Raritian players updated table ------------------------------------------
gbm.vector <- c(gbm.predict_DF,gbm.predict_FW,gbm.predict_GK,gbm.predict_MF)

player.names <- cor_df_merge[,c('Player','Annualized_Salary','Pos_new','Nation')]

# for (vector in gbm.vector) {
#     
#     player.names <- cbind(player.names, )
# }

#Field players
player.salary <- cbind(player.names, gbm.predict_DF)
player.salary <- cbind(player.salary, gbm.predict_MF)
player.salary <- cbind(player.salary, gbm.predict_FW)

#Filter out RFL players
rarita.players <- player.salary %>% filter(Nation == 'Rarita')

rarita.mf <- rarita.players %>%
    filter(Pos_new == 'MF') %>%
    select(Player, Annualized_Salary, gbm.predict_MF)%>%
    mutate(salary.ratio = gbm.predict_MF/Annualized_Salary)%>%
    arrange(desc(salary.ratio))

rarita.df <- rarita.players %>%
    filter(Pos_new == 'DF') %>%
    select(Player, Annualized_Salary, gbm.predict_DF)%>%
    mutate(salary.ratio = gbm.predict_DF/Annualized_Salary)%>%
    arrange(desc(salary.ratio))

rarita.fw <- rarita.players %>%
    filter(Pos_new == 'FW') %>%
    select(Player, Annualized_Salary, gbm.predict_FW)%>%
    mutate(salary.ratio = gbm.predict_FW/Annualized_Salary)%>%
    arrange(desc(salary.ratio))

#Goalkeepers
goalkeepers <- gk_df[,c('Player','Annualized_Salary','Nation')]
gk.salary <- cbind(goalkeepers, gbm.predict_GK)
rarita.gk <- gk.salary %>%
    filter(Nation == 'Rarita')%>%
    select(Player, Annualized_Salary, gbm.predict_GK)%>%
    mutate(salary.ratio = gbm.predict_GK/Annualized_Salary)%>%
    arrange(desc(salary.ratio))
