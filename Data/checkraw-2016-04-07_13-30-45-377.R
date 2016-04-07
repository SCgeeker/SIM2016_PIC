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

RT = with(data = FILTER_EUR, tapply(response_time, paste(ID, size, Pic, Match, sep = "_"), mean))

RT_DF = data.frame(
        cbind(
          matrix(unlist(stri_split_fixed(names(RT), "_")), ncol = 6),
          RT
                )
)

KICK_EUR = filter(CRITIC_EUR, ID != "E_C1_L1_001",ID != "E_C1_L1_008",ID != "E_C1_L1_011",ID != "E_C1_L2_002")
with(data = KICK_EUR, tapply(response_time, paste(size, Pic, Match), mean))
