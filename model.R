load("data/model.RData")


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
    eval(call("<-",paste0(level,"_RFL"),
              cbind(get(paste0(level,"_RFL")),Diff)))
    
    
}


plot(MF_RFL$Annualized_Salary,MF_RFL$Predicted_Sal, main= "MF RFL")
plot(DF_RFL$Annualized_Salary,DF_RFL$Predicted_Sal, main= "DF RFL")
plot(FW_RFL$Annualized_Salary,FW_RFL$Predicted_Sal, main= "FW RFL")


MF_RFL %>% arrange(Diff,descending = T)
DF_RFL %>% arrange(Diff,descending = T)
FW_RFL %>% arrange(Diff,descending = T)