load("data/merged_league.RData")
load("data/merged_tourn.RData")


library(dplyr)
df <- PLAYER_league_non_goal_salary
colnames(df)
colnames(df) <- gsub(" ", "_",colnames(df))
colnames(df) <- gsub("%", "_per",colnames(df))
colnames(df)

vec_of_attr <- c("Total_Cmp_per", 
                 "Short_Cmp_per", 
                 "Medium_Cmp_per", 
                 "Long_Cmp_per",
                 "Performance_PK",
                 "Performance_PKatt",
                 "Tackles_TklW",
                 "Pressures__per",
                 "Clr",
                 "Blocks_Blocks",
                 "Blocks_Sh",
                 "Blocks_ShSv",
                 "Blocks_Pass")

## Took average of all 90s columns
df$`90s_avg` <- rowMeans(df %>% select("90s","90s.x","90s.y")) * 90
df$`90s_avg` <- ifelse(df$`90s_avg` < 0.1,0,df$`90s_avg`)
df <- filter(df,`90s_avg` > 0)
df[vec_of_attr] <- df[vec_of_attr]/df$`90s_avg`




## Standardised

# 
# str<- paste(paste0(vec_of_attr,' = mean(',vec_of_attr, ')'), collapse=",")
# 
# 
# nation <- df %>% 
#     group_by(Nation) %>%
#     summarise(eval(parse(text = str)))

league <- df %>%
    group_by(League)  %>%
    summarise(Total_Cmp_per = mean(Total_Cmp_per,na.rm=T),
              Short_Cmp_per = mean(Short_Cmp_per,na.rm=T),
              Medium_Cmp_per = mean(Medium_Cmp_per,na.rm=T),
              Long_Cmp_per = mean(Long_Cmp_per,na.rm=T),
              Performance_PK = mean(Performance_PK,na.rm=T),
              Performance_PKatt = mean(Performance_PKatt,na.rm=T),
              Tackles_TklW = mean(Tackles_TklW,na.rm=T),
              Pressures__per = mean(Pressures__per,na.rm=T),
              Clr = mean(Clr,na.rm=T),
              Blocks_Blocks = mean(Blocks_Blocks,na.rm=T),
              Blocks_Sh = mean(Blocks_Sh,na.rm=T),
              Blocks_ShSv = mean(Blocks_ShSv,na.rm=T),
              Blocks_Pass = mean(Blocks_Pass,na.rm=T))

nation <- df %>%
    group_by(Nation)  %>%
    summarise(Total_Cmp_per = mean(Total_Cmp_per,na.rm=T),
              Short_Cmp_per = mean(Short_Cmp_per,na.rm=T),
              Medium_Cmp_per = mean(Medium_Cmp_per,na.rm=T),
              Long_Cmp_per = mean(Long_Cmp_per,na.rm=T),
              Performance_PK = mean(Performance_PK,na.rm=T),
              Performance_PKatt = mean(Performance_PKatt,na.rm=T),
              Tackles_TklW = mean(Tackles_TklW,na.rm=T),
              Pressures__per = mean(Pressures__per,na.rm=T),
              Clr = mean(Clr,na.rm=T),
              Blocks_Blocks = mean(Blocks_Blocks,na.rm=T),
              Blocks_Sh = mean(Blocks_Sh,na.rm=T),
              Blocks_ShSv = mean(Blocks_ShSv,na.rm=T),
              Blocks_Pass = mean(Blocks_Pass,na.rm=T))



## 
## Clean outliers from each column
## 
## Correlation heatmap

