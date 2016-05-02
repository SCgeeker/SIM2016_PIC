# Read single csv files
setwd("D:/core/Research/projects/Embodied/EXPDATA/ActionObject/2016_Simulation/Data")
source("readraw.R")
source("checkraw.R")

library(ggplot2)


dodge <- position_dodge(width=0.9)
jpeg("Chinese.jpg")
RT <- ggplot(subset(Conditions_Stat, Lang == "C"), aes(x = paste(Size, Pic), y = RT_MEAN, group = Match, fill = Match) ) + scale_fill_manual(values = c("grey","black")) + ylab("RT") + xlab("Size X Picture") + ggtitle("Match Effects(Chinese)") + theme_bw()
RT + geom_bar(stat = "identity", position = dodge) + geom_errorbar(aes(ymin=(RT_MEAN - RT_SD/sqrt(length(unlist(Sample_N$C)))), ymax = (RT_MEAN + RT_SD/sqrt(length(unlist(Sample_N$C)))) ), position = dodge, width=0.5) + coord_cartesian(ylim = c(500, 700))
dev.off()

jpeg("English.jpg")
RT <- ggplot(subset(Conditions_Stat, Lang == "E"), aes(x = paste(Size, Pic), y = RT_MEAN, group = Match, fill = Match) ) + scale_fill_manual(values = c("grey","black")) + ylab("RT") + xlab("Size X Picture") + ggtitle("Match Effects(English)") + theme_bw()
RT + geom_bar(stat = "identity", position = dodge) + geom_errorbar(aes(ymin=(RT_MEAN - RT_SD/sqrt(length(unlist(Sample_N$E)))), ymax = (RT_MEAN + RT_SD/sqrt(length(unlist(Sample_N$E)))) ), position = dodge, width=0.5) + coord_cartesian(ylim = c(500, 800))
dev.off()

OVERALL_ANALYSIS <- list(
        RT_analysis = with(
                data = RT_DF,
                summary(aov(RT ~ Lang*Size*Pic*Match + Error(paste0(Lang,ID)/(Size*Pic*Match) )))
        ),
        PE_analysis = with(
                data = RT_DF,
                summary(aov(PE ~ Lang*Size*Pic*Match + Error(paste0(Lang,ID)/(Size*Pic*Match) )))
        )
)

ENGLISH_ANALYSIS <- list(
        RT_analysis = with(
                data = subset(RT_DF, Lang == "E"),
                summary(aov(RT ~ Size*Pic*Match + Error(paste0(Lang,ID)/(Size*Pic*Match) )))
        ),
        PE_analysis = with(
                data = subset(RT_DF, Lang == "E"),
                summary(aov(PE ~ Size*Pic*Match + Error(paste0(Lang,ID)/(Size*Pic*Match) )))
        )
)


CHINESE_ANALYSIS <- list(
        RT_analysis = with(
                data = subset(RT_DF, Lang == "C"),
                summary(aov(RT ~ Size*Pic*Match + Error(paste0(Lang,ID)/(Size*Pic*Match) )))
        ),
        PE_analysis = with(
                data = subset(RT_DF, Lang == "C"),
                summary(aov(PE ~ Size*Pic*Match + Error(paste0(Lang,ID)/(Size*Pic*Match) )))
        )
)

save.image(file = "20160428.RData")
