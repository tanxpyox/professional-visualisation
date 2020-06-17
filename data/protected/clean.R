# Clean data for USA 2020

library(dplyr)
library(magrittr)
library(rstatix)

df <- read.csv("data/protected/usa-2020.csv")
# Remove Disqualified respondents
df <- subset(df,
            waiver!=2 &   # Drop if refuse to participate
            yob>=9 &      # Drop age less than 18
            citizen==1 &  # Drop Non-US citizen
            Duration..in.seconds. > 100
          )

# Module: Reputation
## Gen credibility scores (7-point DV)
df$cred <- rowSums(df[, c("credyes", "credlean", "credno")], na.rm=TRUE)

## Summarise average credibility scores under different treatments
df %>%
  group_by(rep) %>%
  get_summary_stats(cred)

## Analyse average treatment effects
rep_ttest <- df %>% pairwise_t_test(cred ~ rep)

# Module: Security Dilemma

## Analyse average treatment effects for spend 1 to 5
### DV 1: Bell
df %>% filter(spend<=6) %>% pairwise_t_test(bell ~ spend)
### DV 2: Offense
df %>% filter(spend<=6) %>% pairwise_t_test(offense ~ spend)
### DV 3: Threat
df %>% filter(spend<=6) %>% pairwise_t_test(offense ~ spend)

## Analyse average treatment effects for spend = 11 to 12
### DV1: stg2
df %>% filter(spend>=11) %>% pairwise_t_test(stg2 ~ spend)
### DV2: offense
df %>% filter(spend>=11) %>% pairwise_t_test(offense ~ spend)
### DV3: threat
df %>% filter(spend>=11) %>% pairwise_t_test(threat ~ spend)

# Module: Exceptionalism

## Tablulate responses
summary(df$ex2)  # IV1: peace loving
summary(df$ex3)  # IV2: hardworking
summary(df$ex45) # IV3: unique quality

## Analyse average treatment effects
### DV1: nativist
df$except <- ifelse(df$except==5, 4, df$except) # if except = 5, replace --> 4
df %>% pairwise_t_test(nativist ~ except)
### DV2: hawk
df$hawk = df$hawk1 + df$hawk2 - 2
df %>% pairwise_t_test(hawk ~ except)
### DV3: trust
df %>% pairwise_t_test(trust ~ except)

## Subset analysis
### By gender
df %>% group_by(gender) %>% pairwise_t_test(nativist ~ except)
df %>% group_by(gender) %>% pairwise_t_test(hawk ~ except)
df %>% group_by(gender) %>% pairwise_t_test(trust ~ except)

### By education
df$uni = df$edu>=5
df %>% group_by(uni) %>% pairwise_t_test(nativist ~ except)
df %>% group_by(uni) %>% pairwise_t_test(hawk ~ except)
df %>% group_by(uni) %>% pairwise_t_test(trust ~ except)

### By age
df$age28 = (df$yob+9) >= 28
df %>% group_by(age28) %>% pairwise_t_test(nativist ~ except)
df %>% group_by(age28) %>% pairwise_t_test(hawk ~ except)
df %>% group_by(age28) %>% pairwise_t_test(trust ~ except)

### By party identification
df$repub <- ifelse(df$pid1==2 | (df$pid1==3 & df$pid2n==1) | (df$pid1==4 & df$pid2n==1), 1, 0)
df %>% group_by(repub) %>% pairwise_t_test(nativist ~ except)
df %>% group_by(repub) %>% pairwise_t_test(hawk ~ except)
df %>% group_by(repub) %>% pairwise_t_test(trust ~ except)

### By national pride
df$pride <- ifelse(df$pride1>=3, 1, 0)
df %>% group_by(pride) %>% pairwise_t_test(nativist ~ except)
df %>% group_by(pride) %>% pairwise_t_test(hawk ~ except)
df %>% group_by(pride) %>% pairwise_t_test(trust ~ except)

df$hipride <- ifelse(df$pride1>=4, 1, 0)
df %>% group_by(hipride) %>% pairwise_t_test(nativist ~ except)
df %>% group_by(hipride) %>% pairwise_t_test(hawk ~ except)
df %>% group_by(hipride) %>% pairwise_t_test(trust ~ except)

# Module: Untying Hands
## Generate approval scores (7-pt DV)
df$untie <- rowSums(df[, c("untie1d", "untie1n", "untie1a")], na.rm=TRUE)
## Generate reputation scores (7-pt DV)
df$damage1a <- ifelse(df$damage1==3, 4, df$damage1a)
df$reputation <- rowSums(df[, c("damage1a", "damage1b")], na.rm=TRUE)

## Analyse average treatment effects
df %>% pairwise_t_test(untie ~ hand) ### DV1: untie
df %>% pairwise_t_test(reputation ~ hand) ### DV2: reputation

## Subset analysis
### By gender
df %>% group_by(gender) %>% pairwise_t_test(untie ~ hand)
df %>% group_by(gender) %>% pairwise_t_test(reputation ~ hand)
## By education
df %>% group_by(uni) %>% pairwise_t_test(untie ~ hand)
df %>% group_by(uni) %>% pairwise_t_test(reputation ~ hand)
## By age
df %>% group_by(age28) %>% pairwise_t_test(untie ~ hand)
df %>% group_by(age28) %>% pairwise_t_test(reputation ~ hand)
## By party identification
df %>% group_by(repub) %>% pairwise_t_test(untie ~ hand)
df %>% group_by(repub) %>% pairwise_t_test(reputation ~ hand)
## By national pride
df %>% group_by(pride) %>% pairwise_t_test(untie ~ hand)
df %>% group_by(pride) %>% pairwise_t_test(reputation ~ hand)
df %>% group_by(hipride) %>% pairwise_t_test(untie ~ hand)
df %>% group_by(hipride) %>% pairwise_t_test(reputation ~ hand)

# Save cleaned data frame
save(df, file="data/protected/usa-2020-cleaned.Rda")
# Save as CSV for cross-platform analysis
write.csv(df, file="data/protected/usa-2020-cleaned.csv")
