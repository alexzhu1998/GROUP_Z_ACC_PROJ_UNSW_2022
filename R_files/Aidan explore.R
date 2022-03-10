#Analysing other team compositions
df <- PLAYER_tourn_non_goal
#df_2020 <- df %>% filter(Year == 2020)
#df_2021 <- df %>% filter(Year == 2021)

pos <- c("DF","MF","FW",'GK')
winning_teams <- c('Sobianitedrucy',
"People's Land of Maneau",
                  'Nganion',
                  'Mico',
                  'Quewenia',
                  'Southern Ristan',
                  'Galamily',
                  'Bernepamar',
                  'Dosqaly',
                  'Giumle Lizeibon',
                  'Greri Landmoslands',
                  'Xikong',
                  'Manlisgamncent',
                  'Esia',
                  'Byasier Pujan',
                  'Djipines',
                  'Leoneku Guidisia',
                  'Ledian',
                  'Eastern Sleboube',
                  'New Uwi',
                  'Ngoque Blicri',
                  'Nkasland Cronestan',
                  'Eastern Niasland',
                  'Varijitri Isles')

attach(df)    
#Linear regression analaysis
pos_df <- df %>%
    filter(Pos_new == "DF")

pos_df <- pos_df%>% select(-c("Player","Nation","Pos_new","League","Squad"))
        
lm <-lm(Annualized_Salary ~ ., data = pos_df)
c <- summary(lm)
coeff_table <- c$coefficients
colnames(coeff_table)[4] <- "p_value"
coeff_table[coeff_table[,"p_value"]<0.05,]

pos_df <- df %>%
    filter(Pos_new == "MF")

pos_df <- pos_df%>% select(-c("Player","Nation","Pos_new","League","Squad"))
        
lm <-lm(Annualized_Salary ~ ., data = pos_df)
c <- summary(lm)
coeff_table <- c$coefficients
colnames(coeff_table)[4] <- "p_value"
coeff_table[coeff_table[,"p_value"]<0.05,]

pos_df <- df %>%
    filter(Pos_new == "FW")

pos_df <- pos_df%>% select(-c("Player","Nation","Pos_new","League","Squad"))
        
lm <-lm(Annualized_Salary ~ ., data = pos_df)
c <- summary(lm)
coeff_table <- c$coefficients
colnames(coeff_table)[4] <- "p_value"
coeff_table[coeff_table[,"p_value"]<0.05,]
