load("all_data.RData")


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


overlapping_cols <- c("Player","Nation","Pos", "Squad","Age", "Year", "Born", "League")
PLAYER_league_non_goal <- merge(x = eval(parse(text = dats[1])),
                                y = eval(parse(text = dats[2])), 
                                by = overlapping_cols,
                                all = T)
PLAYER_league_non_goal <- merge(x = PLAYER_league_non_goal,
                                y = eval(parse(text = dats[3])),
                                by = overlapping_cols,
                                all = T)



##### Examining Data #####



##### Join Data #####


