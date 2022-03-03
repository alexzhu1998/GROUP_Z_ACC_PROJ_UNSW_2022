load("data/merged_league.RData")
load("data/merged_tourn.RData")
install.packages("gbm")

library(dplyr)
library(corrplot)
library(ggplot2)
library(gbm)


df <- PLAYER_league_non_goal_salary

#Edit in a new column that takes first two characters from position
df['Pos_new'] <- substr(df$Pos,1,2)


colnames(df) <- gsub(" ","_", colnames(df))

vec_of_attr <- c("Total_Cmp%", 
                 "Short_Cmp%", 
                 "Medium_Cmp%", 
                 "Long_Cmp%",
                 "Performance_PK",
                 "Performance_PKatt",
                 "Tackles_TklW",
                 "Pressures_%",
                 "Clr",
                 "Blocks_Blocks",
                 "Blocks_Sh",
                 "Blocks_ShSv",
                 "Blocks_Pass",
                 "Gls",
                 "Standard_SoT%",
                 "Standard_G/Sh")

pos <- c("DF","MF","FW")

## Took average of all 90s columns
df$`90s_avg` <- rowMeans(df %>% select("90s","90s.x","90s.y")) * 90
df$`90s_avg` <- ifelse(df$`90s_avg` < 0.1,0,df$`90s_avg`)
df <- filter(df,`90s_avg` > 0)
df[vec_of_attr] <- df[vec_of_attr]/df$`90s_avg`



nat <- df %>%
    group_by(Nation) %>%
    summarise(across(vec_of_attr,~mean(.x,na.rm=T)))


l <- df %>%
    group_by(League) %>%
    summarise(across(vec_of_attr,~mean(.x,na.rm=T)))


## Clean Missing Data from each column

summary(df)

# Replaced all negative values with 0
for (c in colnames(df))
    df[[c]] <- replace(df[[c]],which(df[[c]] <0) ,0) 


# Replace all NA with 0
for (c in colnames(df))
    df[[c]] <- replace(df[[c]],which(is.na(df[[c]])) ,0) 


summary(df)



table(subset(df,is.na(`Standard_G/Sh`))$Pos)


for (i in colnames(df)) {
    for (j in colnames(df)) {
        if (i == j) {
            next
        } else {
            if (all(!is.na(df[[i]])) & all(!is.na(df[[j]])) & sum(df[[i]] == df[[j]])/nrow(df) > 0.9) {
                print(paste(i,j))
            }
        }
    }
}

df<- df %>% select(-c("90s","90s.x","90s.y"))

df<- df %>% select(-c("Position", "Country","Pos"))

nonRFL <- filter(df,League != "RFL")

mod <- glm(Annualized_Salary ~ .-Player-Nation, data = nonRFL)



#Separate players by position
pos_coef_list <- list()
for (i in seq_along(pos)) {
    pos_df <- df %>%
        filter(Pos_new == pos[1])%>%
        filter(League != 'RFL')
    
    
    mod <- glm(Annualized_Salary ~ .-Player-Nation, data = pos_df[,-71])
    s <- summary(mod)
    
    coeff_table <- s$coefficients
    colnames(coeff_table)[4] <- "p_value"
    coeff_table[coeff_table[,"p_value"]<0.05,]

    
}

mod <- glm(Annualized_Salary ~ .-Player-Nation-Pos_new, data = df)

s <- summary(mod)

coeff_table <- s$coefficients
colnames(coeff_table)[4] <- "p_value"
coeff_table[coeff_table[,"p_value"]<0.05,] 

## Correlation heatmap
temp_df <- df%>% select(-c("Player","Nation","Pos_new","League","Squad"))
cormat <- cor(temp_df[1:20], method = "pearson")
cormat <- corrplot(cormat, method = "number")
cormat <- cor(temp_df[21:40], method = "pearson")
cormat <- corrplot(cormat, method = "number")
cormat <- cor(temp_df[41:60], method = "pearson")
cormat <- corrplot(cormat, method = "number")
cormat <- cor(temp_df[61:67], method = "pearson")
cormat <- corrplot(cormat, method = "number")


##Distribution of salary
ggplot(df)+
    geom_histogram(aes(x = Annualized_Salary, y = ..density..), color = "black", fill="#33AFFF")+
    labs(x = "Annualised Salary", y = "Density", title = "Distribution of Annualised Salary")+
    theme_bw() +
    theme(axis.text=element_text(size=9.5), axis.title=element_text(size=13, face = "bold"), plot.title = element_text(size=16, face = "bold"), plot.subtitle=element_text(size=13))


##Distribution of salary
ggplot(PLAYER_league_non_goal_salary)+
    geom_histogram(aes(x = `Annualized Salary`, y = ..density..), color = "black", fill="#33AFFF")+
    labs(x = "Annualised Salary", y = "Density", title = "Distribution of Annualised Salary")+
    theme_bw() +
    theme(axis.text=element_text(size=9.5), axis.title=element_text(size=13, face = "bold"), plot.title = element_text(size=16, face = "bold"), plot.subtitle=element_text(size=13))



gbmFit.param <- gbm(Annualized_Salary ~., data = df[,-c(1,2,3,4,5,71)], distribution = "gaussian", cv.fold = 10, n.trees = 10000, interaction.depth = 1, shrinkage = 0.01)
gbmFit.param

min <- which.min(gbmFit.param$cv.error)
min
gbm.perf(gbmFit.param, method = "cv")

gbmFit <- gbm(Annualized_Salary ~., data = df[,-c(1,2,3,4,5,71)], distribution = "gaussian", n.trees = min, interaction.depth = 1, shrinkage = 0.01)

summary(gbmFit)








