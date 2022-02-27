load("data/all_raw_data.RData")


##### Load Packages #####
library(dplyr)



##### Pre-cleaning Examination #####
dats <- c("PLAYER_league_def","PLAYER_league_pass","PLAYER_league_shoot")
vec = c("Player","Nation","Pos", "Squad","Age") # for some reason "Born" does not cover the whole set like "Age"
mat = matrix(0,
             nrow = length(vec),
             ncol = length(dats),
             dimnames = list(vec,dats))


# Count how many uniques in each combination of player profile
for (j in (1:length(dats))) {
    for (i in (1:length(vec))) {
        tmp <- vec[1:i]
        mat[i,j] = as.numeric(count(unique(select(eval(parse(text = dats[j])),tmp))))
    }
}
## Alex's Comment: It is possible they are all the same player combinations

# Checking if the sets are different - they are the exact same.
setdiff(unique(select(eval(parse(text= dats[1])),vec[1:5])),unique(select(eval(parse(text = dats[2])), vec[1:5])))
setdiff(unique(select(eval(parse(text= dats[1])),vec[1:5])),unique(select(eval(parse(text = dats[3])), vec[1:5])))

### Merging Data ###
overlapping_cols <- c("Player","Nation","Pos", "Squad","Age", "Year", "Born", "League")
PLAYER_league_non_goal <- merge(x = eval(parse(text = dats[1])),
                                y = eval(parse(text = dats[2])), 
                                by = overlapping_cols,
                                all = T)
PLAYER_league_non_goal <- merge(x = PLAYER_league_non_goal,
                                y = eval(parse(text = dats[3])),
                                by = overlapping_cols,
                                all = T)

# A check to see if the merged data rows are increased
stopifnot(count(PLAYER_league_non_goal) == count(PLAYER_league_def))

Year <- rep(2021,count(PLAYER_salary2021))
a <- cbind(Year,PLAYER_salary2021)
Year <- rep(2020,count(PLAYER_salary2020))
PLAYER_salary <- rbind(a,cbind(Year,PLAYER_salary2020))
names(PLAYER_salary)[names(PLAYER_salary) == "Player Name"] <- "Player"


# setdiff(unique(select(PLAYER_salary, c("Player Name","Year","League"))),
overlapping_cols2 <- c("Player","Year","League","Squad")
setdiff(unique(select(PLAYER_salary, overlapping_cols2)),
        unique(select(PLAYER_league_non_goal, overlapping_cols2)))

PLAYER_league_non_goal_salary <- merge(
    x = PLAYER_league_non_goal,
    y = PLAYER_salary,
    by = overlapping_cols2)


# data_to_remove <- c(dats,"PLAYER_league_non_goal", "PLAYER_salary", "PLAYER_salary2020", "PLAYER_salary2021")
# rm(list = list(data_to_remove)[[1]])

save(PLAYER_league_non_goal_salary, file = "data/merged_league_non_goal_sal.RData")




dats2 <- c("PLAYER_tourn_def","PLAYER_tourn_pass","PLAYER_tourn_shoot_2021")
vec = c("Player","Year","League", "Age", "Born")#,"Nation","Pos") # for some reason "Born" does not cover the whole set like "Age"
mat = matrix(0,
             nrow = length(vec),
             ncol = length(dats2),
             dimnames = list(vec,dats2))


# Count how many uniques in each combination of player profile
for (j in (1:length(dats2))) {
    for (i in (1:length(vec))) {
        tmp <- vec[1:i]
        mat[i,j] = as.numeric(count(unique(select(eval(parse(text = dats2[j])),tmp))))
    }
}
## Alex's Comment: It is possible they are all the same player combinations

# Checking if the sets are different - they are the exact same.
setdiff(unique(select(eval(parse(text= dats2[1])),vec[1])),unique(select(eval(parse(text = dats2[2])), vec[1])))
setdiff(unique(select(eval(parse(text= dats2[1])),vec[1])),unique(select(eval(parse(text = dats2[3])), vec[1])))

##### Merging Data #####

overlapping_cols2 = c(vec)
PLAYER_tourn_non_goal <- merge(x = eval(parse(text = dats2[1])),
                                y = eval(parse(text = dats2[2])), 
                                by = overlapping_cols2,
                                all = T)
PLAYER_tourn_non_goal <- merge(x = PLAYER_tourn_non_goal,
                                y = eval(parse(text = dats2[3])),
                                by = overlapping_cols2,
                                all = T)

# A check to see if the merged data rows are increased
stopifnot(count(PLAYER_tourn_non_goal) == count(PLAYER_tourn_def))


#### Remove Useless Data #####
# data_to_remove <- c(dats2,"PLAYER_tourn_shoot")
# rm(list = list(data_to_remove)[[1]])

save(PLAYER_tourn_non_goal, file = "data/merged_tourn_non_goal_sal.RData")
##### Join Data #####


