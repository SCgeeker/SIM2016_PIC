library(dplyr)
library(stringi)

with(data = data_EUR, tapply(correct,paste(ID,correct_response),mean) )  # check accuracy by participant

CRITIC_EUR <- subset(data_EUR, (correct_response == "/" & correct == 1))
with(data = CRITIC_EUR, tapply(correct,ID,table)/80 )
#CORRECT_EUR <- subset(CRITIC_EUR, correct == 1)

# check outliers
up_out <- function(x){
        mean(x) + 2*sd(x)
}
        
down_out <- function(x){
        mean(x) - 2*sd(x)
}

#CRITIC_EUR = arrange(CRITIC_EUR, desc(size))

UP <- with(data = CRITIC_EUR, tapply(response_time, paste(size, Pic, Match, sep = "_"), up_out))

DOWN <- with(data = CRITIC_EUR, tapply(response_time, paste(size, Pic, Match, sep = "_"), down_out))

FILTER_EUR <- NULL

for(i in 1:length(UP)){
        FILTER_EUR = rbind(
                FILTER_EUR,
                filter(CRITIC_EUR, paste(size, Pic, Match, sep = "_") == names(UP)[i], response_time > DOWN[i], response_time < UP[i])
        )
}

rm(i, UP, DOWN, down_out, up_out)
#filter(CRITIC_EUR, paste(size, Pic, Match) == names(DOWN)[1])$response_time < DOWN[1]

with(data = FILTER_EUR, tapply(response_time, paste(size, Pic, Match), mean))


RT_DF = data.frame(
          matrix(unlist(stri_split_fixed(names(RT), "_")), ncol = 7, byrow = TRUE),
          with(data = FILTER_EUR, tapply(response_time, paste(ID, size, Pic, Match, sep = "_"), mean)),
          with(data = FILTER_EUR, tapply(correct, paste(ID, size, Pic, Match, sep = "_"), table))/10,
        row.names = NULL
)

colnames(RT_DF) = c("Lang", "Cond", "List","ID","Size","Pic","Match","RT","PE")

with(data = RT_DF, tapply(RT, paste(Size, Pic, Match), mean))
with(data = RT_DF, tapply(PE, paste(Size, Pic, Match), mean))

KICK_EUR = filter(RT_DF, ID != "001",ID != "008",ID != "011",ID != "002")
with(data = KICK_EUR, tapply(RT, paste(Size, Pic, Match), mean))
with(data = KICK_EUR, tapply(PE, paste(Size, Pic, Match), mean))
