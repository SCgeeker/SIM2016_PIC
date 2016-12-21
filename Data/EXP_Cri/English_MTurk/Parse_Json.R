library(dplyr)
library(jsonlite)
## Define the source folder
RAW_PATH = "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP_Cri\\English_MTurk\\"
## Define the parsed raw data; File name will be replaced.
FILE <- "cd6c3d38-93ae-4a8d-ae78-b48d27c04ff8.csv"
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

for(i in seq(dim(JSON_RAW)[1]) ){
        for(j in seq(dim(JSON_RAW)[2])[!is.na(JSON_RAW[i,])]){
                print(names(fromJSON(as.character(JSON_RAW[i,j])))[grep("PictureItem[RT]",names(fromJSON(as.character(JSON_RAW[i,j]))) )] )
        }
}

# Aggregate the raw data we need
# Stack Verication responses
RAW_V <- NULL
for(i in seq(dim(JSON_RAW)[1]) ){
        RAW01 <- NULL
        for(j in seq(dim(JSON_RAW)[2])[!is.na(JSON_RAW[i,])]){
        if(sum( grep("V_",names(fromJSON(as.character(JSON_RAW[i,j]))) ) ) > 0){
                T_name <- names(fromJSON(as.character(JSON_RAW[i,j])))[grep("TrialNr",names(fromJSON(as.character(JSON_RAW[i,j]))) )]
                RAW01 <- rbind(RAW01, unlist(fromJSON(as.character(JSON_RAW[i,j])))[c("BlockId", T_name,"Size","Orien","Match","TargetItem[CRESP]","SentenceItem[RT]","TargetItem[RT]","TargetItem[ACC]")] )
        }else{;}
        }
                RAW01 <- cbind(Item_idx = seq(dim(RAW01)[1]), RAW01)
                RAW01 <- cbind(ID = rep(Resp_RAW$mTurkCode[i],dim(RAW01)[1]), RAW01 )
                RAW01 <- cbind(Age = rep( as.character(Resp_RAW[i,meta_seq[3]]), dim(RAW01)[1]), RAW01)
                RAW01 <- cbind(Gender = rep(as.character(Resp_RAW[i,meta_seq[2]] ),dim(RAW01)[1]), RAW01 )
                RAW01 <- cbind(Lang = rep( as.character(Resp_RAW[i,meta_seq[1]]), dim(RAW01)[1]), RAW01)
                RAW01 <- cbind(RAW01, EndDate = rep( as.character(Resp_RAW[i,meta_seq[4]]) ,dim(RAW01)[1]) )
                RAW_V <- rbind(RAW_V,RAW01)
}

# Stack Memory responses
RAW_M <- NULL
for(i in seq(dim(JSON_RAW)[1]) ){
        RAW02 <- NULL
        for(j in seq(dim(JSON_RAW)[2])[!is.na(JSON_RAW[i,])]){
                if(sum( grep("M_",names(fromJSON(as.character(JSON_RAW[i,j]))) ) ) > 0){
                T_name <- names(fromJSON(as.character(JSON_RAW[i,j])))[grep("TrialNr",names(fromJSON(as.character(JSON_RAW[i,j]))) )]
                RAW02 <- rbind(RAW02, unlist(fromJSON(as.character(JSON_RAW[i,j])))[c("BlockId", T_name,"TargetItem[CRESP]", "TargetItem[ACC]")] )
                }else{;}
        }
        RAW02 <- cbind(Item_idx = seq(dim(RAW02)[1]), RAW02)
        RAW02 <- cbind(ID = rep(Resp_RAW$mTurkCode[i],dim(RAW02)[1]), RAW02 )
        RAW02 <- cbind(Age = rep( as.character(Resp_RAW[i,meta_seq[3]]), dim(RAW02)[1]), RAW02)
        RAW02 <- cbind(Gender = rep(as.character(Resp_RAW[i,meta_seq[2]] ),dim(RAW02)[1]), RAW02 )
        RAW02 <- cbind(Lang = rep( as.character(Resp_RAW[i,meta_seq[1]]), dim(RAW02)[1]), RAW02)
        RAW02 <- cbind(RAW02, EndDate = rep( as.character(Resp_RAW[i,meta_seq[4]]) ,dim(RAW02)[1]) )
        RAW_M <- rbind(RAW_M,RAW02)
}        
# Delete temporary objects
rm(RAW01, RAW02, i, j, meta_seq, T_name, JSON_RAW, Resp_RAW,FILE,ID,RAW_PATH)
# Transfer matrix to data frame
RAW_V = data.frame(RAW_V)
RAW_M = data.frame(RAW_M)

# Make the consistent column names
names(RAW_V) <- c("Lang","Gender","Age","ID","Item_index","List","Trial_Seq","Size","Orien","Match","Answer","sentence_read_time","Target_response_time","correct","datetime")
names(RAW_M) <- c("Lang","Gender","Age","ID","List","Trial_Seq","Target","Answer","correct","datetime")

# Export Qualtrics Raw Data
write.csv(RAW_V, file = paste0(RAW_PATH,"NET_Verication_rawdata.csv"),row.names = FALSE)
write.csv(RAW_M, file = paste0(RAW_PATH,"NET_Memory_rawdata.csv"),row.names = FALSE)

rm(list = ls())
