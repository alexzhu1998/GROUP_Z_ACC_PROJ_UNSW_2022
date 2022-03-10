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
lm <-lm(Annualized_Salary ~ ., data = df)
