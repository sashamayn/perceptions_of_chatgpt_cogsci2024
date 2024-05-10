library(dplyr)
library(lme4)
library(lmerTest)

df <- read.csv('../data/experiment_data.csv')
df <- subset(df, df$itemtype_coarser != 'filler ambiguous')
df$condition <- df$itemtype_coarser
df$condition[df$itemtype_coarser == 'filler unambiguous'] <- 'control'
df$condition <- factor(df$condition, levels = c('critical','control'))
df$msgtype <- as.factor(ifelse(df$msg %in% c('re','gr'),'color','shape'))
df$speaker_identity <- factor(df$speaker_identity, levels=c('gpt','adult','child'))
mean_trialid <- mean(df$trialid)
df$trialid_centered <- df$trialid - mean_trialid
df$targetpos <- factor(df$targetpos, levels = c(2,1,3))
contrasts(df$condition) = contr.sum(2)

m <- lmer(prob_target ~ speaker_identity + condition + speaker_identity:condition +
                     trialid_centered + msgtype + targetpos +
                     (1+trialid_centered|participant_id),
                   data = df, control = lmerControl(optimizer='bobyqa'))
summary(m)
