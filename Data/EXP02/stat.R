# Read single csv files
setwd("D:/core/Research/projects/Embodied/EXPDATA/ActionObject/2016_Simulation/Data/EXP01")
source("readraw.R")
source("checkraw.R")

library(ggplot2)


dodge <- position_dodge(width=0.9)
jpeg("Chinese.jpg")
RT <- ggplot(subset(Conditions_Stat, Lang == "C"), aes(x = paste(Size, Pic), y = RT_MEAN, group = Match, fill = Match) ) + scale_fill_manual(values = c("grey","black")) + ylab("RT") + xlab("Size and Orientation") + theme_bw()
RT + geom_bar(stat = "identity", position = dodge) + geom_errorbar(aes(ymin=(RT_MEAN - RT_SD/sqrt(length(unlist(Sample_N$C)))), ymax = (RT_MEAN + RT_SD/sqrt(length(unlist(Sample_N$C)))) ), position = dodge, width=0.5) + coord_cartesian(ylim = c(500, 700))
dev.off()

jpeg("English.jpg")
RT <- ggplot(subset(Conditions_Stat, Lang == "E"), aes(x = paste(Size, Pic), y = RT_MEAN, group = Match, fill = Match) ) + scale_fill_manual(values = c("grey","black")) + ylab("RT") + xlab("Size and Orientation") + theme_bw()
RT + geom_bar(stat = "identity", position = dodge) + geom_errorbar(aes(ymin=(RT_MEAN - RT_SD/sqrt(length(unlist(Sample_N$E)))), ymax = (RT_MEAN + RT_SD/sqrt(length(unlist(Sample_N$E)))) ), position = dodge, width=0.5) + coord_cartesian(ylim = c(500, 800))
dev.off()

names(RT_DF) <- c("Lang","Cond","List","ID","Size","Orientation", "Match","RT","PE")

OVERALL_ANALYSIS <- list(
        RT_analysis = with(
                data = RT_DF,
                summary(aov(RT ~ Lang*Size*Orientation*Match + Error(paste0(Lang,ID)/(Size*Orientation*Match) )))
        ),
        PE_analysis = with(
                data = RT_DF,
                summary(aov(PE ~ Lang*Size*Orientation*Match + Error(paste0(Lang,ID)/(Size*Orientation*Match) )))
        )
)

ENGLISH_ANALYSIS <- list(
        RT_analysis = with(
                data = subset(RT_DF, Lang == "E"),
                summary(aov(RT ~ Size*Orientation*Match + Error(paste0(Lang,ID)/(Size*Orientation*Match) )))
        ),
        PE_analysis = with(
                data = subset(RT_DF, Lang == "E"),
                summary(aov(PE ~ Size*Orientation*Match + Error(paste0(Lang,ID)/(Size*Orientation*Match) )))
        )
)


CHINESE_ANALYSIS <- list(
        RT_analysis = with(
                data = subset(RT_DF, Lang == "C"),
                summary(aov(RT ~ Size*Orientation*Match + Error(paste0(Lang,ID)/(Size*Orientation*Match) )))
        ),
        PE_analysis = with(
                data = subset(RT_DF, Lang == "C"),
                summary(aov(PE ~ Size*Orientation*Match + Error(paste0(Lang,ID)/(Size*Orientation*Match) )))
        )
)

Match_Effect <- Conditions_Stat[Conditions_Stat$Match == "No",]$RT_MEAN - Conditions_Stat[Conditions_Stat$Match == "Yes",]$RT_MEAN

Match_Effect_Sp <- sqrt( ( c(rep(19,4),rep(24,4))*Conditions_Stat[Conditions_Stat$Match == "No",]$RT_SD^2 + c(rep(19,4),rep(24,4))*Conditions_Stat[Conditions_Stat$Match == "Yes",]$RT_SD^2 )/c(rep(38,4),rep(48,4)) )

save.image(file = "20160428.RData")
