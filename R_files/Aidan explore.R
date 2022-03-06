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

    
  
    
    
    
    
    
    
    
    



squad_names <- unique(df_2020$Squad)
team_size <- vector(length = length(squad_names))
for (i in seq_along(squad_names)) {
    
    temp <- df_2020 %>% 
        filter(Squad == squad_names[i]) %>%
        group_by(Pos_new) %>% 
        summarise(count = n())
    pos<- cbind(pos, temp$count)
}
mean(team_size)

pos <- as.numeric(pos)
temp <- data.frame(pos)
rownames(temp) <- c("DF","MF","FW",'GK')
rowMeans(temp)
