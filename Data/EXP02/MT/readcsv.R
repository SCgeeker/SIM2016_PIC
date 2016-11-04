RAW_PATH = "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP02\\MT\\1027_100subjects_pilot\\"

subj_path <- dir(RAW_PATH,pattern = "list[1-4]_[0-9]{1,2}-[0-9]{1,2}.csv")
        #dir(RAW_PATH, pattern = "[0-5]{1,3}.csv")

#tmp <- read.csv(paste0(RAW_PATH,subj_path[1]), allowEscapes = TRUE, na.strings = " ", quote = "", comment.char = "", check.names = TRUE, skipNul = TRUE)

subj_raw <- data.frame(
        Size <- factor(),
        Orien <- factor(),
        Match <- factor(),
        BlockID <- as.character(),
        TrialNr <- as.numeric(),
        Sent_RT <- as.numeric(),
        RT <- as.numeric(),
        ACC <- as.numeric()
)

subj_raw01 <- NULL
subj_raw02 <- NULL
subj_raw03 <- NULL
subj_raw04 <- NULL

for(i in 1:5){
        subj_raw01 <-  rbind( subj_raw01, read.csv(paste0(RAW_PATH,subj_path[i]), allowEscapes = TRUE, na.strings = " ", quote = "", comment.char = "", check.names = FALSE, skipNul = TRUE, header = TRUE, encoding = "utf-16LE")[,c("Size","Orien","Match","BlockId","SP_List1[TrialNr]","SentenceItem[RT]","PictureItem[RT]","PictureItem[ACC]")] )
}
colnames(subj_raw01) <- c("Size","Orien","Match","BlockID","TrialNr","Sent_RT","RT","ACC")

for(i in 6:10){
        subj_raw02 <-  rbind( subj_raw02, read.csv(paste0(RAW_PATH,subj_path[(i)]), allowEscapes = TRUE, na.strings = " ", quote = "", comment.char = "", check.names = FALSE, skipNul = TRUE, header = TRUE, encoding = "utf-16LE")[,c("Size","Orien","Match","BlockId","SP_List2[TrialNr]","SentenceItem[RT]","PictureItem[RT]","PictureItem[ACC]")] )
}
colnames(subj_raw02) <- c("Size","Orien","Match","BlockID","TrialNr","Sent_RT","RT","ACC")

for(i in 11:15){
        subj_raw03 <-  rbind( subj_raw03, read.csv(paste0(RAW_PATH,subj_path[(i)]), allowEscapes = TRUE, na.strings = " ", quote = "", comment.char = "", check.names = FALSE, skipNul = TRUE, header = TRUE, encoding = "utf-16LE")[,c("Size","Orien","Match","BlockId","SP_List3[TrialNr]","SentenceItem[RT]","PictureItem[RT]","PictureItem[ACC]")] )
}
colnames(subj_raw03) <- c("Size","Orien","Match","BlockID","TrialNr","Sent_RT","RT","ACC")
## List3, #19 has 133 trials

for(i in 16:20){
        subj_raw04 <- rbind( subj_raw04, read.csv(paste0(RAW_PATH,subj_path[(i)]), allowEscapes = TRUE, na.strings = " ", quote = "", comment.char = "", check.names = FALSE, skipNul = TRUE, header = TRUE, encoding = "utf-16LE")[,c("Size","Orien","Match","BlockId","SP_List4[TrialNr]","SentenceItem[RT]","PictureItem[RT]","PictureItem[ACC]")] )
}
colnames(subj_raw04) <- c("Size","Orien","Match","BlockID","TrialNr","Sent_RT","RT","ACC")

subj_raw = rbind(rbind( rbind(subj_raw01,subj_raw02), subj_raw03), subj_raw04)

subj_raw = cbind(ID = paste0("pilot1027",sprintf("%03d",c(rep(1:68,134),rep(69,133),rep(70:100,134) )) ),subj_raw)

subj_critical = subj_raw[subj_raw$Size == "L" | subj_raw$Size == "S",]

subj_ACC_check <- with(data=subj_critical,tapply(ACC, ID, mean)*100)

List_count <- with(data=subj_critical,table(BlockID))

subj_RT_median <- with(data=subj_critical[subj_critical$ACC == 1,],tapply(RT, paste(ID,Size, Orien, Match), median))

subj_RT_median2 <- with(data=subj_critical[subj_critical$ACC == 1,],tapply(RT, paste(ID,Size, Match), median))

subj_ACC_means <- with(data=subj_critical,tapply(ACC, paste(ID,Size, Orien, Match), mean)*100)

subj_ReadTime_means <- with(data=subj_critical[subj_critical$ACC == 1,],tapply(Sent_RT, paste(ID,Size, Orien, Match), mean))

library(stringi)

subj_table = data.frame( matrix(unlist(stri_split_fixed(names(subj_RT_median), " ")),nc=4, byrow = TRUE ), RT = subj_RT_median, ACC = subj_ACC_means)

row.names(subj_table) <- NULL
colnames(subj_table) = c("ID","Size","Orientation","Match","RT","ACC")
# Calculate conditions medians
with(data = subj_table, tapply(RT, paste(Size,Orientation, Match), median))

with(data = subj_table, tapply(RT, paste(Size, Match), median))

with(data = subj_table, tapply(ACC, paste(Size,Orientation, Match), mean))

with(data = subj_table, tapply(ACC, paste(Size, Match), mean))

jasp_data_3IV <- matrix(subj_table$RT,nc=8,byrow = TRUE, dimnames = list(levels(subj_table$ID),with(subj_table, paste(Size, Orientation, Match))[1:8]))

jasp_data_2IV <- matrix(subj_RT_median2,nc=4,byrow = TRUE, dimnames = list(levels(subj_table$ID),with(subj_table, paste(Size, Match))[c(1:2,5:6)]))

write.csv(jasp_data_3IV, file = paste0(RAW_PATH,"pilot1027_3IV.csv"))
write.csv(jasp_data_2IV, file = paste0(RAW_PATH,"pilot1027_2IV.csv"))
write.csv(subj_table, file = paste0(RAW_PATH,"subj_table1027.csv"))


#subj001$SentenceItem.CalculatedDuration.
#names(subj001)
#col_summary <- c(17:19, 56, 54, 33, 43,45)


#assign( paste0("subj",sprintf("%03d",i)),read.csv(paste0(RAW_PATH,subj_path[i]), allowEscapes = TRUE, na.strings = " ", quote = "", comment.char = "", check.names = FALSE, skipNul = TRUE, encoding = "utf-16LE")[c(17:19, 56, 54, 33, 43,45)] )
