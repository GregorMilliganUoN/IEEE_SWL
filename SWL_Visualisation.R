library(readr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(readr)
library(sqldf)
library(gridExtra)
swl_data <- read_csv("/Users/gregormilligan/Desktop/PhD Folder/Year 3/IEEE Big Dagta SWL Factors Paper/swl_with_loneliness_data.csv")

swl_data$satisfaction_with_life_group <- factor(swl_data$satisfaction_with_life_group, levels = c('low',  'high'))
swl_data$satisfaction_with_life <- factor(swl_data$satisfaction_with_life, levels = c(1,2,3,4,5,6,7,8,9,10))
swl_data$lack_companionship <- factor(swl_data$lack_companionship, levels = c(2,1,0))
swl_data$isolation <- factor(swl_data$isolation, levels = c(2,1,0))
#swl_data$isolation <- factor(data$isolation, levels = c(2,1, 0))
colnames(swl_data)

# network features of interest:
# "closeness_centrality"                                         
#  "betweenness_centrality"                                        
#  "degree_centrality"                                            
#  "clustering"                                                    
#  "pagerank" 

#---emdeddedness score----------------

swl_df <- swl_data %>%
  group_by(isolation,satisfaction_with_life_group) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

swl_df


p1 <- ggplot(swl_df, aes(fill=satisfaction_with_life_group, y=n, x=isolation)) + 
  geom_bar(position="fill", stat="identity", color="black")+
  scale_fill_brewer(palette = 'PuBuGn', direction = -1)+
  labs(x='isolation score',y='proportion',
       title = 'SWL and Isolation',
       fill = 'swl') +
  theme(text = element_text(family="Arial", size = 13))
p1

swl_df <- swl_data %>%
  group_by(lack_companionship,satisfaction_with_life_group) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

swl_df


p2 <- ggplot(swl_df, aes(fill=satisfaction_with_life_group, y=n, x=lack_companionship)) + 
  geom_bar(position="fill", stat="identity", color="black")+
  scale_fill_brewer(palette = 'PuBuGn', direction = -1)+
  labs(x='lack_companionship score',y='proportion',
       title = 'SWL and companionship',
       fill = 'swl') +
  theme(text = element_text(family="Arial", size = 13))
p2



swl_df <- swl_data %>%
  group_by(isolation,satisfaction_with_life) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

swl_df
p1 <- ggplot(swl_df, aes(fill=isolation, y=n, x=satisfaction_with_life)) + 
  geom_bar(position="stack", stat="identity", color="black")+
  scale_fill_brewer(palette = 'PuBuGn', direction = -1)+
  labs(x='swl_score',y='count',
       title = 'SWL and isolation',
       fill = 'isolation') +
  theme(text = element_text(family="Arial", size = 13))
p1


p2 <- ggplot(swl_df, aes(fill=isolation, y=n, x=satisfaction_with_life)) + 
  geom_bar(position="fill", stat="identity", color="black")+
  scale_fill_brewer(palette = 'PuBuGn', direction = -1)+
  labs(x='swl_score',y='proportion',
       title = 'SWL and isolation',
       fill = 'isolation') +
  theme(text = element_text(family="Arial", size = 13))
p2

swl_df <- swl_data %>%
  group_by(lack_companionship,satisfaction_with_life) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

swl_df


p3 <- ggplot(swl_df, aes(fill=lack_companionship, y=n, x=satisfaction_with_life)) + 
  geom_bar(position="stack", stat="identity", color="black")+
  scale_fill_brewer(palette = 'PuBuGn', direction = -1)+
  labs(x='swl_score',y='count',
       title = 'SWL and lack_companionship',
       fill = 'companionship') +
  theme(text = element_text(family="Arial", size = 13))
p3

p4 <- ggplot(swl_df, aes(fill=lack_companionship, y=n, x=satisfaction_with_life)) + 
  geom_bar(position="fill", stat="identity", color="black")+
  scale_fill_brewer(palette = 'PuBuGn', direction = -1)+
  labs(x='swl_score',y='proportion',
       title = 'SWL and companionship',
       fill = 'companionship') +
  theme(text = element_text(family="Arial", size = 13))
p4

grid.arrange(p1,p2,p3,p4, nrow=2)


summary(swl_df$satisfaction_with_life_group)
summary(swl_df$satisfaction_with_life)

grid.arrange(p2,p4, nrow=2)