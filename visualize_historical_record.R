source("calculate_top8_cutoffs.R")
source("colors.R")




visualize_league_history <- function(player){
  
  player_matches <- all_matches %>% 
    filter(Player==player,
           weeks_in_league==6)
  
  
  for (format in unique(player_matches$Format)){
    player_matches <- player_matches %>% 
      add_row(n_wins=0,n_losses=0,n_matches=0,Format=format,.before = 1)
  }
  
  player_matches$Format <- factor(player_matches$Format,levels=levels(all_matches$Format))
  
  n_rows <- case_when(length(unique(player_matches$Format)) < 6 ~ 1,
                      length(unique(player_matches$Format)) < 13 ~ 2,
                      length(unique(player_matches$Format)) < 22 ~ 3,
                      .default = 4)
  
  player_matches %>% 
    ggplot(aes(x=n_losses,y=n_wins)) +
    facet_wrap("Format",nrow=n_rows) +
    scale_x_continuous(breaks=seq(0,11,by=2),minor_breaks = 0:11) +
    scale_y_continuous(breaks=seq(0,30,by=4),minor_breaks = 0:max(player_matches$n_wins)) +
    scale_color_manual(values=colors) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white",
                                          colour = "white",
                                          linetype = "solid"),
          panel.grid.major = element_line(linetype = 'solid',
                                          colour = "#dadada"),
          panel.grid.minor = element_line(linetype = 'solid',
                                          colour = "#dadada"),
          legend.position="none") +
    #geom_rect(aes(ymin=18,ymax=20.5,xmin=-0.3,xmax=11),fill="lightgreen")  +
    geom_rect(aes(ymin=cutoff,ymax=30,xmin=-0.3,xmax=11),fill="#C1E1C1")  +
    geom_abline(slope=1,intercept=0,linetype="dashed") +
    geom_abline(slope=-1,intercept=5,linetype="dotted") +
    geom_abline(slope=-1,intercept=10,linetype="dotted") +
    geom_abline(slope=-1,intercept=15,linetype="dotted") +
    geom_abline(slope=-1,intercept=20,linetype="dotted") +
    geom_abline(slope=-1,intercept=25,linetype="dotted") +
    geom_abline(slope=-1,intercept=30,linetype="dotted") +
    geom_rect(aes(ymin=-2,ymax=-.3,xmin=-2,xmax=12),fill="white") +
    geom_rect(aes(ymin=-1,ymax=30,xmin=-2,xmax=-0.3),fill="white") +
    geom_rect(aes(ymin=0,ymax=30,xmin=11,xmax=12)) +
    geom_line(aes(col=Format),linewidth=2.5) +
    coord_fixed(xlim=c(0,11),ylim = c(0,max(max(player_matches$n_wins)-0.5,20))) +
    labs(title = paste0(player,'\'s historical record, Arena Gauntlet League, ',month(min(player_matches$Time,na.rm=T),label=T)," ",year(min(player_matches$Time,na.rm=T)),"-now."),
         x = "Losses",y="Wins")
  
}
