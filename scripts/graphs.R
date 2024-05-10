library('dplyr')
library('tidyr')
library('ggplot2')
library('ggpubr')

df <- read.csv('../data/experiment_data.csv')
df <- rename(df, speaker = speaker_identity, condition = itemtype_coarser)
strategies <- read.csv('../data/strategies.csv')
df$speaker[df$speaker == 'gpt'] <- 'ChatGPT'
df$speaker <- factor(df$speaker, levels = c('adult','child','ChatGPT'))
posttest <- read.csv('../data/post_test_questionnaire_answers.csv')

se <- function(x) sd(x)/sqrt(length(x))

### Figure 2: Average target rating by condition ###
avg_scores <- df %>% group_by(condition,speaker) %>% summarize(mean_target = mean(prob_target), 
                                   se_target = se(prob_target),
                                   mean_competitor = mean(prob_competitor), se_competitor = se(prob_competitor),
                                   mean_distractor = mean(prob_distr), se_distractor = se(prob_distr))

avg_scores$condition <- gsub("filler","control",avg_scores$condition)

average_certainty_plot <- ggplot(data = avg_scores, aes(x = speaker, y = mean_target)) + 
  geom_bar(stat="identity") +  
  geom_errorbar( aes(x=speaker, ymin=mean_target-se_target, ymax=mean_target+se_target), width=0.4,color="black") +
  facet_wrap(~factor(condition,levels=c('control unambiguous','critical','control ambiguous'))) + 
  xlab('speaker type') + ylab('target certainty') + theme_bw() + theme(text = element_text(size=25,family="serif"),
                                                            axis.text.x = element_text(size=25),
                                                          legend.text = element_text(size=25))
average_certainty_plot

### Figure 3: Frequency and average target rating of each annotation tag ###
targetprob_by_partic <- subset(df, df$condition == 'critical') %>% group_by(participant_id) %>% summarize(mean_target = mean(prob_target),
                                                                                                    sd_target = sd(prob_target))
strategies$speaker <- df$speaker[match(strategies$participant_id,df$participant_id)]
strategies <- merge(strategies,targetprob_by_partic,by='participant_id')

tag_counts <- strategies %>% group_by(speaker, strategy_tag) %>% summarize(n=n(),
                                            mean_prob = mean(mean_target), se_prob = se(mean_target))

tag_counts <- subset(tag_counts, tag_counts$strategy_tag != 'exclude')
tag_counts <- rename(tag_counts, strategy = strategy_tag)

tag_counts$sp <- as.character(tag_counts$speaker)
tag_counts$sp[tag_counts$speaker == 'adult'] <- 'adult (M&D)'
tag_counts$sp[tag_counts$speaker == 'child'] <- 'child (M&D)'
tag_counts$sp <- factor(tag_counts$sp, levels =c('adult (M&D)','child (M&D)','ChatGPT'))

tag_count_plot <- ggplot(data=tag_counts, aes(x=strategy,y=n,fill=strategy)) +   
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~sp)  + xlab('strategy')+ylab('frequency')+theme_bw()+
    theme(
      text = element_text(size=20,family="serif"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.text = element_text(size=20))+
      scale_fill_manual(values = c("grey2","grey25","grey37","grey53","grey73","grey90"))+
      xlab('')

avg_probs_per_tag_plot <- ggplot(data = tag_counts, aes(x=strategy,y=mean_prob,fill=strategy)) +   
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar( aes(x=strategy, ymin=mean_prob-se_prob, ymax=mean_prob+se_prob), 
                 width=0.4,color="black")+
  facet_wrap(~sp)  + xlab('strategy')+ylab('mean target probability')+theme_bw()+
  theme(
    text = element_text(size=20,family="serif"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.text = element_text(size=20))+
    scale_fill_manual(values = c("grey2","grey25","grey37","grey53","grey73","grey90"))+
    xlab('')

combined_tag_plot <- ggarrange(tag_count_plot, avg_probs_per_tag_plot, nrow=2,ncol=1)
combined_tag_plot

### Figure 5: Average target rating by estimate of ChatGPT's capability ###
n_each_response <- posttest %>% group_by(certainty_of_chatgpt_capability) %>% summarize(n = n())
posttest <- left_join(posttest,n_each_response,by="certainty_of_chatgpt_capability")
posttest$mean_target <- targetprob_by_partic$mean_target[match(posttest$participant_id,targetprob_by_partic$participant_id)]
posttest$certainty <- paste(posttest$certainty_of_chatgpt_capability,' (N=',posttest$n,')',sep="")

posttest$certainty <- as.factor(posttest$certainty)
boxplot <- ggplot(data=posttest, aes(x=factor(certainty), y=mean_target)) + geom_boxplot()+
  xlab('certainty of ChatGPT\'s capability')+ylab('mean target probability')+theme_bw()+
  theme(text = element_text(size=25,family="serif"),
        legend.text = element_text(size=25))

boxplot

### Figure 6: Average target rating per participant ###
targetprob_by_partic$speaker <- strategies$speaker[match(targetprob_by_partic$participant_id,
                                                         strategies$participant_id)]

adult_targetprobs <- subset(targetprob_by_partic,targetprob_by_partic$speaker =='adult')
adult_targetprobs_sorted <- adult_targetprobs[order(-adult_targetprobs$mean_target),]
adult_targetprobs_sorted$nrow <- seq.int(nrow(adult_targetprobs_sorted))

child_targetprobs <- subset(targetprob_by_partic,targetprob_by_partic$speaker =='child')
child_targetprobs_sorted <- child_targetprobs[order(-child_targetprobs$mean_target),]
child_targetprobs_sorted$nrow <- seq.int(nrow(child_targetprobs_sorted))

gpt_targetprobs <- subset(targetprob_by_partic,targetprob_by_partic$speaker =='ChatGPT')
gpt_targetprobs_sorted <- gpt_targetprobs[order(-gpt_targetprobs$mean_target),]
gpt_targetprobs_sorted$nrow <- seq.int(nrow(gpt_targetprobs_sorted))

all_targetprobs_sorted <- rbind(adult_targetprobs_sorted,child_targetprobs_sorted,gpt_targetprobs_sorted)
all_targetprobs_sorted$speaker <- as.character(all_targetprobs_sorted$speaker)
all_targetprobs_sorted$speaker[all_targetprobs_sorted$speaker == 'adult'] <- 'adult (M&D)'
all_targetprobs_sorted$speaker[all_targetprobs_sorted$speaker == 'child'] <- 'child (M&D)'
all_targetprobs_sorted$speaker <- factor(all_targetprobs_sorted$speaker, levels=c('adult (M&D)','child (M&D)','ChatGPT'))

sorted_participant_plot <- ggplot(data=all_targetprobs_sorted,aes(x=nrow,y=mean_target,color=speaker))+
  geom_line()+geom_point(aes(shape=speaker),size=4.5)+
  xlab('participant (sorted)')+ylab('mean target probability')+theme_bw()+
  theme(text = element_text(size=25,family="serif"),
  legend.text = element_text(size=25))+
  scale_color_manual(values = c("adult (M&D)" = "grey72", "child (M&D)" = "grey45", "ChatGPT" = "grey22"))+
  guides(color = guide_legend(override.aes = list(linetype = "blank")))

sorted_participant_plot
