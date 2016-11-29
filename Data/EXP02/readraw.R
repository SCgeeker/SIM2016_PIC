# Combine TW raw data
From_folder <- "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP02\\TAIWAN"
To_folder <- "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP02\\RAW"

# Copy raw data to working directory
file.copy(From_folder, To_folder, recursive = TRUE)
# Import multiple-bytes string in English system
Sys.setlocale("LC_ALL","English") 

# Import Rotterdam raw data to data frame
# This loop is modified from Ananda Mahto's reply @ http://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r
raw_TW <- list.files(list.dirs(To_folder)[2], pattern = "*.csv", recursive = TRUE)

# Take the columns for pre-processing
V <- c(
        "List",
        "count_Trial",
        "Size",  # within
        "Orien",    # within
        "Match", # within
        "Probe",
        "Target",
        "correct_response",
        "response_time_Probe_response",
        "response_time_Target_response", # measurement
        "correct"       # measurement
)

# create the raw data frame for pre-processing
data_TW <- NULL
for(i in seq(raw_TW)){
        data_TW <- rbind(
                data_TW,
                cbind(
                        ID = rep(gsub("*.csv$", "",raw_TW[i]), 128), 
                        read.csv(paste0(To_folder,"\\TAIWAN\\",raw_TW[i]), encoding = "utf-8")[,V]
                
                )
        )
}

# save filtered raw file to RAW folder
write.csv(data_TW, file=paste0(To_folder,"\\","TW_pilot01.csv"))
# remove the temporary raw files
file.remove(paste0(To_folder,"\\TAIWAN\\",raw_TW))

## ------------------------------------------------
# Combine NL raw data
From_folder <- "D:\\Clouds\\Dropbox\\Dropbox\\Orientation_STI\\NL_pilot"
To_folder <- "D:\\core\\Research\\projects\\Embodied\\EXPDATA\\ActionObject\\2016_Simulation\\Data\\EXP02\\RAW"

# Copy raw data to working directory
file.copy(From_folder, To_folder, recursive = TRUE)
# Import multiple-bytes string in English system
Sys.setlocale("LC_ALL","English") 

# Import Rotterdam raw data to data frame
# This loop is modified from Ananda Mahto's reply @ http://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r
raw_NL <- list.files(list.dirs(To_folder)[2], pattern = "*.csv", recursive = TRUE)

# Take the columns for pre-processing
V <- c(
        "logfile",
        "List",
        "count_Trial",
        "Size",  # within
        "Orien",    # within
        "Match", # within
        "Probe",
        "Target",
        "correct_response",
        "response_time_Probe_response",
        "response_time_Target_response", # measurement
        "correct"       # measurement
)

# create the raw data frame for pre-processing
data_NL <- NULL
for(i in seq(raw_NL)){
        data_NL <- rbind(
                data_NL,
                cbind(
                        read.csv(paste0(To_folder,"\\NL_pilot\\",raw_NL[i]), encoding = "utf-8")[,V]
                        
                )
        )
}

data_NL = data.frame(matrix(unlist(stri_split_fixed(data_NL$logfile, "-")), ncol = 4, byrow = TRUE,dimnames = list(seq(1,dim(data_NL)[1]),c("Lang","Gender","Age","ID"))), data_NL[,-1])

# save filtered raw file to RAW folder
write.csv(data_NL, file=paste0(To_folder,"\\","NL_pilot_raw.csv"))
# remove the temporary raw files
file.remove(paste0(To_folder,"\\NL_pilot\\",raw_NL))
rm(i,V,From_folder, To_folder,raw_TW,raw_NL)

# compare the probe sentence reading time
NL_Reading_Time = summary(data_NL$response_time_Probe_response)
TW_Reading_Time = summary(data_TW$response_time_Probe_response)
Comp_Reading_Time <- t.test(
        with(data = data_NL, tapply(response_time_Probe_response,ID,mean)), with(data = data_TW, tapply(response_time_Probe_response,ID,mean)), var.equal = TRUE)

