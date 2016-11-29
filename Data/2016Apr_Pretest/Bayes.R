require(data.table)
require(stringi)
require(ggplot2)
require(BayesFactor)
load("20160428.RData")

RT1 = with(data = rbind(FILTER_EUR, FILTER_TCU), tapply(response_time, paste(ID, size, Match, sep = "-"), mean))
PE1 = 100*with(data = subset(rbind(data_EUR, data_TCU), size!="D"), tapply(correct, paste(ID, size, Match, sep = "-"), sum))/20

RT_DF1 = data.frame(
        matrix(unlist(stri_split_fixed(names(RT1), "-")), ncol = 6, byrow = TRUE),
        RT1,
        PE1,
        row.names = NULL
)

colnames(RT_DF1) = c("Lang", "Cond", "List","ID","Size","Match","RT","PE")

rm(RT1, PE1)

EFFECT = RT_DF1[RT_DF1$Match == 'N',]$RT - RT_DF1[RT_DF1$Match == 'Y',]$RT
RT_DF1 = data.table(RT_DF1[RT_DF1$Match == 'N',1:5], EFFECT)

RT <- ggplot(subset(Conditions_Stat, Lang == "C"), aes(x = paste(Size, Pic), y = RT_MEAN, group = Match, fill = Match) ) + scale_fill_manual(values = c("grey","black")) + ylab("RT") + xlab("Size and Orientation") + ggtitle("Chinese") + theme_bw()
RT + geom_bar(stat = "identity", position = dodge) + geom_errorbar(aes(ymin=(RT_MEAN - RT_SD/sqrt(length(unlist(Sample_N$C)))), ymax = (RT_MEAN + RT_SD/sqrt(length(unlist(Sample_N$C)))) ), position = dodge, width=0.5) + coord_cartesian(ylim = c(500, 700))

RT <- ggplot(subset(Conditions_Stat, Lang == "E"), aes(x = paste(Size, Pic), y = RT_MEAN, group = Match, fill = Match) ) + scale_fill_manual(values = c("grey","black")) + ylab("RT") + xlab("Size and Orientation") + ggtitle("English") + theme_bw()
RT + geom_bar(stat = "identity", position = dodge) + geom_errorbar(aes(ymin=(RT_MEAN - RT_SD/sqrt(length(unlist(Sample_N$E)))), ymax = (RT_MEAN + RT_SD/sqrt(length(unlist(Sample_N$E)))) ), position = dodge, width=0.5) + coord_cartesian(ylim = c(500, 800))

t_metric <- as.numeric(
        unlist( with(data = RT_DF1, tapply(V2, paste(Lang, Size), t.test)) )
)
t_N <- c(20,20,25,25)

BF.CL <- with(data = RT_DF1[Lang == 'C' & Size == 'L'],
ttestBF(x = V2, nullInterval = c(0,Inf), rscale = 1)
)
BF.CL[1]/BF.CL[2]

BF.CS <- with(data = RT_DF1[Lang == 'C' & Size == 'S'],
     ttestBF(x = V2, nullInterval = c(0,Inf), rscale = 1)
)
BF.CS[1]/BF.CS[2]

BF.EL <- with(data = RT_DF1[Lang == 'E' & Size == 'L'],
     ttestBF(x = V2, nullInterval = c(0,Inf), rscale = 1)
)
BF.EL[1]/BF.EL[2]

BF.ES <- with(data = RT_DF1[Lang == 'E' & Size == 'S'],
     ttestBF(x = V2, nullInterval = c(0,Inf), rscale = 1)
)
BF.ES[1]/BF.ES[2]

BFmeta <- meta.ttestBF( t = t_metric[seq(1,40, by=10)], n1 = t_N, nullInterval = c(0, Inf), rscale = 1)
BFmeta

#require(data.table)
EUR_D_L_M_RT <- RT_DF[RT_DF$Lang == "E" & RT_DF$Orientation == "D" & RT_DF$Size=="L" & RT_DF$Match == "Y",]$RT
EUR_D_L_N_RT <- RT_DF[RT_DF$Lang == "E" & RT_DF$Orientation == "D" & RT_DF$Size=="L" & RT_DF$Match == "N",]$RT

ttestBayesian_EUR_D_L <- ttestBF(x = EUR_D_L_M_RT, y = EUR_D_L_N_RT, paired = TRUE, nullInterval = c(0, Inf))
BFplus0_EUR_D_L <- extractBF(ttestBayesian_EUR_D_L, onlybf=TRUE)[1]

EUR_R_L_M_RT <- RT_DF[RT_DF$Lang == "E" & RT_DF$Orientation == "R" & RT_DF$Size=="L" & RT_DF$Match == "Y",]$RT
EUR_R_L_N_RT <- RT_DF[RT_DF$Lang == "E" & RT_DF$Orientation == "R" & RT_DF$Size=="L" & RT_DF$Match == "N",]$RT

ttestBayesian_EUR_R_L <- ttestBF(x = EUR_R_L_M_RT, y = EUR_R_L_N_RT, paired = TRUE, nullInterval = c(0, Inf))
BFplus0_EUR_R_L <- extractBF(ttestBayesian_EUR_R_L, onlybf=TRUE)[1]


EUR_S_M_RT <- RT_DF[RT_DF$Lang == "E" & RT_DF$Size=="S" & RT_DF$Match == "Y",]$RT
EUR_S_N_RT <- RT_DF[RT_DF$Lang == "E" & RT_DF$Size=="S" & RT_DF$Match == "N",]$RT

ttestBayesian_EUR_S <- ttestBF(x = EUR_S_M_RT, y =EUR_S_N_RT, paired = TRUE, nullInterval = c(0, Inf))
BFplus0_EUR_S <- extractBF(ttestBayesian_EUR_S, onlybf=TRUE)[1]

TCU_L_M_RT <- RT_DF[RT_DF$Lang == "C" & RT_DF$Size=="L" & RT_DF$Match == "Y",]$RT
TCU_L_N_RT <- RT_DF[RT_DF$Lang == "C" & RT_DF$Size=="L" & RT_DF$Match == "N",]$RT

ttestBayesian_TCU_L <- ttestBF(x=TCU_L_M_RT, y=TCU_L_N_RT, paired = TRUE,nullInterval = c(0, Inf))
BFplus0_TCU_L <- extractBF(ttestBayesian_TCU_L, onlybf=TRUE)[1]


TCU_S_M_RT <- RT_DF[RT_DF$Lang == "C" & RT_DF$Size=="S" & RT_DF$Match == "Y",]$RT
TCU_S_N_RT <- RT_DF[RT_DF$Lang == "C" & RT_DF$Size=="S" & RT_DF$Match == "N",]$RT

ttestBayesian_TCU_S <- ttestBF(x=TCU_L_M_RT, y=TCU_L_N_RT, paired = TRUE, nullInterval = c(0, Inf))
BFplus0_TCU_L <- extractBF(ttestBayesian_TCU_L, onlybf=TRUE)[1]
