setwd("D:/core/Research/projects/Embodied/EXPDATA/ActionObject/2016_Simulation/Data")

## This script analyze the priori power for the critical experiments.

load("20160428.RData")
source("D:/core/LAB/Analysis/R/functions.r")
library('pwr')
library('dplyr')
## Effect Size of Main Effect of Matching 
## Dependent measurement is RT
## Retrive from CHINESE_ANALYSIS$RT_analysis
## positions 26 ~ 35 are the parameters
CHINESE_Matching_Main <- unlist(CHINESE_ANALYSIS$RT_analysis)[26:35]
CHINESE_Matching_Main_Effect_Size <-  esComp(CHINESE_Matching_Main[7],
                                             CHINESE_Matching_Main[1],
                                             CHINESE_Matching_Main[2],
                                             N = 20,
                                             "F")

CHINESE_Matching_Main_Power <- data.frame(
        SIZE = seq(20,32),
        PRI_POWER = pwr.anova.test(f = CHINESE_Matching_Main_Effect_Size, k = 2, n = seq(20,32))$power
)

CHINESE_Matching_Means <- with(data = Conditions_Stat[Conditions_Stat$Lang == "C",], tapply(RT_MEAN, Match, mean))

CHINESE_Matching_F_Square <- (CHINESE_Matching_Main[1]/20)*(CHINESE_Matching_Main[7] - 1)

CHINESE_Matching_Main_Power1 <- data.frame(
        SIZE = seq(20,40),
        PRI_POWER = pwr.anova.test(f = CHINESE_Matching_F_Square, n = seq(20,40), k = 8)$power
)

CHINESE_Matching_Cohen_d <- ( CHINESE_Matching_Means[1] - CHINESE_Matching_Means[2])/sqrt(CHINESE_Matching_Main[6])

CHINESE_Matching_Main_Power2 <- data.frame(
        SIZE = seq(20,40),
        PRI_POWER = pwr.t.test(d = abs(CHINESE_Matching_Cohen_d), n = seq(20,40), type = "paired")$power
)


## Retrive from ENGLISH_ANALYSIS$RT_analysis
## positions 26 ~ 35 are the parameters
ENGLISH_Matching_Main <- unlist(ENGLISH_ANALYSIS$RT_analysis)[26:35]
ENGLISH_Matching_Main_Effect_Size <-  esComp(ENGLISH_Matching_Main[7], 
                                             ENGLISH_Matching_Main[1],
                                             ENGLISH_Matching_Main[2],
                                             N = 25,
                                             "F")

ENGLISH_Matching_Main_Power <- data.frame(
        SIZE = seq(25,32),
        PRI_POWER = pwr.anova.test(f = ENGLISH_Matching_Main_Effect_Size, k = 2, n = seq(25,32))$power
)

ENGLISH_Matching_F_Square <- (ENGLISH_Matching_Main[1]/25)*(ENGLISH_Matching_Main[7] - 1)

ENGLISH_Matching_Main_Power1 <- data.frame(
        SIZE = seq(20,40),
        PRI_POWER = pwr.anova.test(f = ENGLISH_Matching_F_Square, k = 8, n = seq(20,40))$power
)

ENGLISH_Matching_Means <- with(data = Conditions_Stat[Conditions_Stat$Lang == "E",], tapply(RT_MEAN, Match, mean))
ENGLISH_Matching_Cohen_d <- ( ENGLISH_Matching_Means[1] - ENGLISH_Matching_Means[2])/sqrt(ENGLISH_Matching_Main[6])

ENGLISH_Matching_Main_Power2 <- data.frame(
        SIZE = seq(20,40),
        PRI_POWER = pwr.t.test(d = abs(ENGLISH_Matching_Cohen_d), n = seq(20,40), type = "paired")$power
)

## Effect Size of Main Effect of Orientation 
## RT is the dependent measurement
## Retrive from CHINESE_ANALYSIS$RT_analysis
## positions 16 ~ 25 are the parameters
CHINESE_Orientation_Main <- unlist(CHINESE_ANALYSIS$RT_analysis)[16:25]
CHINESE_Orientation_Main_Effect_Size <-  esComp(CHINESE_Orientation_Main[7],
                                             CHINESE_Orientation_Main[1],
                                             CHINESE_Orientation_Main[2],
                                             N = 20,
                                             "F")

CHINESE_Orientation_Main_Power <- data.frame(
        SIZE = seq(20,32),
        PRI_POWER = pwr.anova.test(f = CHINESE_Orientation_Main_Effect_Size, k = 2, n = seq(20,32))$power
)

## Retrive from ENGLISH_ANALYSIS$RT_analysis
## positions 16 ~ 25 are the parameters
ENGLISH_Orientation_Main <- unlist(ENGLISH_ANALYSIS$RT_analysis)[16:25]
ENGLISH_Orientation_Main_Effect_Size <-  esComp(ENGLISH_Orientation_Main[7], 
                                             ENGLISH_Orientation_Main[1],
                                             ENGLISH_Orientation_Main[2],
                                             N = 25,
                                             "F")

ENGLISH_Orientation_Main_Power <- data.frame(
        SIZE = seq(25,32),
        PRI_POWER = pwr.anova.test(f = ENGLISH_Orientation_Main_Effect_Size, k = 2, n = seq(25,32))$power
)


## Effect Size of Main Effect of Size 
## RT is the dependent measurement
## Retrive from CHINESE_ANALYSIS$RT_analysis
## positions 16 ~ 25 are the parameters
CHINESE_Size_Main <- unlist(CHINESE_ANALYSIS$RT_analysis)[6:15]
CHINESE_Size_Main_Effect_Size <-  esComp(CHINESE_Size_Main[7],
                                                CHINESE_Size_Main[1],
                                                CHINESE_Size_Main[2],
                                                N = 20,
                                                "F")

CHINESE_Size_Main_Power <- data.frame(
        SIZE = seq(20,32),
        PRI_POWER = pwr.anova.test(f = CHINESE_Size_Main_Effect_Size, k = 2, n = seq(20,32))$power
)

## Retrive from ENGLISH_ANALYSIS$RT_analysis
## positions 6 ~ 15 are the parameters
ENGLISH_Size_Main <- unlist(ENGLISH_ANALYSIS$RT_analysis)[6:15]
ENGLISH_Size_Main_Effect_Size <-  esComp(ENGLISH_Size_Main[7], 
                                                ENGLISH_Size_Main[1],
                                                ENGLISH_Size_Main[2],
                                                N = 25,
                                                "F")

ENGLISH_Size_Main_Power <- data.frame(
        SIZE = seq(25,32),
        PRI_POWER = pwr.anova.test(f = ENGLISH_Size_Main_Effect_Size, k = 2, n = seq(25,32))$power
)



## Effect Size of Interaction 
## RT is the dependent measurement
## Retrive from CHINESE_ANALYSIS$RT_analysis
## positions 66 ~ 75 are the parameters
CHINESE_Inter_Main <- unlist(CHINESE_ANALYSIS$RT_analysis)[66:75]
CHINESE_Inter_Main_Effect_Size <-  esComp(CHINESE_Inter_Main[7],
                                         CHINESE_Inter_Main[1],
                                         CHINESE_Inter_Main[2],
                                         N = 20,
                                         "F")
CHINESE_Inter_Main_Power <- data.frame(
        SIZE = seq(20,32),
        PRI_POWER = pwr.anova.test(f = CHINESE_Inter_Main_Effect_Size, k = 8, n = seq(20,32))$power
)

## Retrive from ENGLISH_ANALYSIS$RT_analysis
## positions 66 ~ 75 are the parameters
ENGLISH_Inter_Main <- unlist(ENGLISH_ANALYSIS$RT_analysis)[66:75]
ENGLISH_Inter_Main_Effect_Size <-  esComp(ENGLISH_Inter_Main[7], 
                                         ENGLISH_Inter_Main[1],
                                         ENGLISH_Inter_Main[2],
                                         N = 25,
                                         "F")

ENGLISH_Inter_Main_Power <- data.frame(
        SIZE = seq(25,32),
        PRI_POWER = pwr.anova.test(f = ENGLISH_Inter_Main_Effect_Size, k = 2, n = seq(25,32))$power
)

## Estimate the priori power by overall effect size
## Chinese
CHINESE_Overall_Power <- data.frame(
        SIZE = seq(20,32),
        PRI_POWER = pwr.t.test(d = -0.223, n = seq(20,32), type = "paired")$power
)

ENGLISH_Overall_Power <- data.frame(
        SIZE = seq(25e,32),
        PRI_POWER = pwr.t.test(d = -0.258, n = seq(25,32), type = "paired")$power
)


## We estimate the priori power of the confirmatory experiment based on the main effect of match effects.
## The effect size of match effects refer to Cohen's f square.
save.image("20160428.RData")
