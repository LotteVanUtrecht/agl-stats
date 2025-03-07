source("colors.R")

summary_stats <- all_matches %>% 
  group_by(Format) %>% 
  summarize("2_0perc"=mean(Score=="2-0",na.rm=T))

summary_stats %>% 
  ggplot(aes(y=`2_0perc`,x=Format,fill=Format)) +
  geom_col() +
  scale_fill_manual(values=colors) +
  theme_classic() +
  theme(legend.position = 'none') +
  scale_x_discrete(limits=rev) +
  scale_y_continuous(breaks=seq(0.45,0.57,by=0.01)) +
  ggtitle("Percentage of 2-0's by format") +
  ylab("Percentage") +
  coord_cartesian(ylim=c(0.45,0.57)) +
  geom_abline(intercept=0.5,slope=0,linetype=2)
  
ggsave(path="Plots/",filename = "2-0s.png",width=1150,height=450,units="px")
