# Set working directory
setwd("D:/core/Research/projects/Embodied/EXPDATA/ActionObject/2016_Simulation/Data")

# Directory for store raw data
From_folder <- "D:/Clouds/Dropbox/Dropbox/DESKS/ActObj/2015NOV/DATA"
To_folder <- "D:/core/Research/projects/Embodied/EXPDATA/ActionObject/2016_Simulation/"

# Copy raw data to working directory
file.copy(From_folder, To_folder, recursive = TRUE)

Sys.setlocale("LC_ALL","English") # Import multiple-bytes string in English system

# Import Rotterdam raw data to data frame
# This loop is modified from Ananda Mahto's reply @ http://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r
raw_EUR <- list.files(list.dirs()[2], pattern = "*.csv", recursive = TRUE)

V <- c(
        "List1",
        "List2",
        "List3",
        "List4",
        "Target",
        "correct_response",
        "response_time_Probe_response_C",
        "response_time_Probe_response_E",
        "response_time", # measurement
        "correct",       # measurement
        "size",  # within
        "Match", # within
        "Pic"    # within
)

Q <- c(
        "response_Response_Q1", # First is English
        "response_Response_Q2", # Second is English
        "response_Response_Q3"  # Proficiency of English
)

data_EUR <- NULL

for(i in 1:length(raw_EUR)){
        data_EUR <- rbind(
                data_EUR,
                cbind(
                        ID = rep(gsub("*.csv$", "", raw_EUR[i]), 160), 
                        cbind(
                        Trial = 1:160,
                        read.csv(paste0("EUR/",raw_EUR[i]), fileEncoding = "UTF-8")[5:164,V]
                        )
                )
        )
}

Q_EUR <- NULL

for(i in 1:length(raw_EUR)){
        Q_EUR <- rbind(
                Q_EUR,
                cbind(
                        ID = rep(gsub("*.csv$", "", raw_EUR[i]), 3), 
                        cbind(
                                Q = as.factor( 1:3),
                                response = 
                                        c(
                                          as.character(read.csv(paste0("EUR/",raw_EUR[i]), fileEncoding = "UTF-8")[165,Q[1]]), 
                                          as.character( read.csv(paste0("EUR/",raw_EUR[i]), fileEncoding = "UTF-8")[166,Q[2]]),
                                          as.numeric(read.csv(paste0("EUR/",raw_EUR[i]), fileEncoding = "UTF-8")[167,Q[3]]) 
                                         )
                        )
                )
        )
}

# assign(make.names(gsub("*.csv$", "", raw_EUR[i])), )


## ------------------------------------------------
## Import Taiwan data to data frame
## The unicode in raw files have to be transfered to unicode numbers.
raw_TCU <- list.files(list.dirs()[3], pattern = "*.csv", recursive = TRUE)

data_TCU <- NULL

for(i in 1:length(raw_TCU)){
        data_TCU <- rbind(
                data_TCU,
                cbind(
                        ID = rep(gsub("*.csv$", "", raw_TCU[i]), 160), 
                        cbind(
                                Trial = 1:160,
                                read.csv(paste0("TCU/",raw_TCU[i]))[5:164,V]
                        )
                )
        )
}

rm(From_folder, To_folder, i, raw_EUR, raw_TCU, V, Q)
# Included columns:
# form_response, response_rtime, response_time_Probe_C, response_time_Probe_C, correct, correct_response

# compare the probe sentence reading time
E_Reading_Time = summary(data_EUR$response_time_Probe_response_E)
C_Reading_Time = summary(data_TCU$response_time_Probe_response_C)
Comp_Reading_Time <- t.test(
        with(data = data_EUR, tapply(response_time_Probe_response_E,ID,mean)), with(data = data_TCU, tapply(response_time_Probe_response_C,ID,mean)), var.equal = TRUE)
