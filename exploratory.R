load("data/merged_league.RData")
load("data/merged_tourn.RData")


library(dplyr)
library(corrplot)
library(caret)
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

#Separate players by position
pos_coef_list <- list()
for (i in seq_along(pos)) {
    pos_df <- df %>%
        filter(Pos_new == pos[3])%>%
        filter(League != 'RFL')
    
    
    mod <- glm(Annualized_Salary ~ .-Player-Nation-Squad-League-Year, data = pos_df[,-71],
               family = )
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
cormat <- cor(temp_df, method = "pearson")
remove <- c('Total_Att','')

colnames(df)
keep <- c('Age','Tackles_Tkl','Vs_Dribbles_Att','Pressures_%','Blocks_ShSv',
          'Int','Clr','Total_Cmp%',
          'Standard_Sh/90','Standard_G/SoT','Standard_Dist','Standard_FK',
          'Performance_PK','Expected_xG','Annualized_Salary','90s_avg')

temp_df <- df%>% select(-c("Player","Nation","Pos_new","League","Squad"))
cormat <- cor(temp_df[c(1:25,66)], method = "pearson")
corrplot(cormat, method = "number")
cormat <- cor(temp_df[c(26:51,66)], method = "pearson")
corrplot(cormat, method = "number")
cormat <- cor(temp_df[48:67], method = "pearson")
corrplot(cormat, method = "number")

cor_df <- df[,keep]
cormat <- cor(cor_df, method = "pearson")
corrplot(cormat, method = "number")
