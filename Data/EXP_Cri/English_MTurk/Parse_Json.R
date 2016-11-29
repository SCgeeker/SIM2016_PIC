library(dplyr)
library(jsonlite)
## Define the source folder
RAW_PATH = "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP_Cri\\English_MTurk\\"
## Define the parsed raw data; File name will be replaced.
FILE <- "full_raw.csv"
## Define the available Qualtrics ID
## Download the selected columns: date, Research_ID, mTurk, QRTE_id
## File name will be replaced.
ID <- "meta.csv"

## Import the parsed raw data
Resp_RAW = read.csv(paste0(RAW_PATH,FILE), encoding = "utf-8")
## Import the Qualtrics research ID
Research_ID = read.csv(paste0(RAW_PATH,ID),encoding="utf-8", skip = 1)

## Merge by research ID
Resp_RAW = merge(Resp_RAW, Research_ID, by="mTurkCode")

# Isloate the cells filled with JSON
JSON_RAW = Resp_RAW[,grep("X[0-9]{1,3}_QID[0-9]{1,3}_1",names(Resp_RAW))]
# Order the seqnece as the sequence of stimuli
JSON_RAW = JSON_RAW[,c(grep("X[0-9]{1,1}_",names(JSON_RAW)), grep("X[0-9]{2,2}_",names(JSON_RAW)),grep("X[0-9]{3,3}_",names(JSON_RAW)))]
# Keep the columns of language, gender, and age.
meta_seq <- c(grep("L1",names(Resp_RAW)), grep("Gender",names(Resp_RAW)), grep("Age",names(Resp_RAW)), grep("endDate",names(Resp_RAW)) )

T_name <- c("BlockId","TrialNr","Size","Orien","Match","SentenceItem\\[RT\\]","PictureItem\\[RT\\]","PictureItem\\[ACC\\]","ComprehensionItem\\[ACC\\]")

# Aggregate the raw data we need
# Stack Verication responses
RAW_V <- data.frame()
for(i in seq(dim(JSON_RAW)[1]) ){
        RAW01 <- NULL  # Temporary store of every trial
        ind <- 0  # Record the stimuli sequence in the list
        for(j in seq(dim(JSON_RAW)[2])[!is.na(JSON_RAW[i,]) & JSON_RAW[i,] != "" ]){
        # Get the raw data from the experimental session
        if(sum( grep("SP_List",names(fromJSON(as.vector(JSON_RAW[i,j]))) ) ) > 0){
                # Record the stimuli sequence in the list
                ind = ind + 1
                RAW_COL <- NULL
                for(k in seq(T_name)){
                        # Retrive the columns we need
                        RAW_COL <- c(RAW_COL, grep(T_name[k],names(fromJSON(as.character(JSON_RAW[i,j]))) ) )
                }
                # Append the stimuli seqnece in the list
                tmp <- c(ind, unlist(fromJSON(as.character(JSON_RAW[i,j]))[RAW_COL]) )
                # Check the row without recorded ComprehensionItem[ACC]
                if("ComprehensionItem[ACC]" %in% names(tmp) == FALSE){tmp <- c(tmp, "")}
                # Stack the raw data of this participant
                RAW01 <- rbind(RAW01, tmp) 
        }else{;}
        }
                # Append the random participant ID
                RAW01 <- cbind(ID = rep(Resp_RAW$mTurkCode[i],dim(RAW01)[1]), RAW01 )
                # Append the participant's age
                RAW01 <- cbind(Age = rep( as.character(Resp_RAW[i,meta_seq[3]]), dim(RAW01)[1]), RAW01)
                # Append the participant's gender
                RAW01 <- cbind(Gender = rep(as.character(Resp_RAW[i,meta_seq[2]] ),dim(RAW01)[1]), RAW01 )
                # Append the participant's first language
                RAW01 <- cbind(Lang = rep( as.character(Resp_RAW[i,meta_seq[1]]), dim(RAW01)[1]), RAW01)
                # Append the participant's finished time
                RAW01 <- cbind(RAW01, EndDate = rep( as.character(Resp_RAW[i,meta_seq[4]]) ,dim(RAW01)[1]) )
                RAW01 = unname(RAW01)
                # Append the least participant's raw data
                RAW_V <- rbind(RAW_V,RAW01)
}

# Switch the columns to defined positions
RAW_V = RAW_V[,c(1:4,6:10,5,11:15)]

# Make the consistent column names
names(RAW_V) <- c("Lang","Gender","Age","ID","List","Trial_Seq","Size","Orien","Match","Item_index","sentence_read_time","Target_response_time","correct","Comprehension_correct","datetime")

# Export Qualtrics Raw Data
write.csv(RAW_V, file = paste0(RAW_PATH,"NET_Verication_rawdata.csv"),row.names = FALSE)

rm(list = ls())
