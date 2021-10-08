# NFL-Offensive-Line-Analysis
Load necessary libraries:

    library(tidyr)
    library(tidyverse)
    library(dplyr)
    library(data.table)
    library(R.utils)
    library(ggplot2)
    library(ggrepel)
    library(ggvoronoi)
    library(ggpubr)
    library(viridis)
    library(pracma)
    library(sf)
    library(DT)
    library(shiny)
    library(gridExtra)
    library(sfsmisc)
    library(useful)
    library(MASS)
    library(geometry)
    library(survival)

<br /> Below are the links to all the data necessary for the analysis.
The `players_df` is adapted from <https://www.nfl.com/> data,
`game_play_df` and `gameId_conv_df` is adapted from `nflfastR`, and all
others are data frames with sample data from the analysis we reproduce
below. Notably, `det_area_analysis_df` is empty, because this data set
was too large to upload to GitHub, but the function to produce this data
is included in this file. The analysis exampled below will largely be
oriented around one play whose data was uploaded to GitHub, stored in
`week_games_df`. The analysis is originally produced using tracking data
from
<https://github.com/903124/NFL-live-tracking-data/tree/main/output_file>.

    players_df <- read.csv("https://raw.githubusercontent.com/paulibrahim/NFL-Offensive-Line-Analysis/main/Data/players.csv")
    game_play_df <- read.csv("https://raw.githubusercontent.com/paulibrahim/NFL-Offensive-Line-Analysis/main/Data/game_play_index.csv")
    gameId_conv_df <- read.csv("https://raw.githubusercontent.com/paulibrahim/NFL-Offensive-Line-Analysis/main/Data/gameId_conversion.csv")
    summary_analysis_df <- read.csv("https://raw.githubusercontent.com/paulibrahim/NFL-Offensive-Line-Analysis/main/Data/sum_info_compiled.csv")
    play_info_df <- read.csv("https://raw.githubusercontent.com/paulibrahim/NFL-Offensive-Line-Analysis/main/Data/compiled_play_info.csv")
    play_info_df$frameId <- NULL
    det_area_analysis_df <- data.frame()
    compiled_bin_info_df <- read.csv("https://raw.githubusercontent.com/paulibrahim/NFL-Offensive-Line-Analysis/main/Data/comp_bin_info.csv")
    names(compiled_bin_info_df)[1] <- "bin_id"
    week_games_df <- read.csv("https://raw.githubusercontent.com/paulibrahim/NFL-Offensive-Line-Analysis/main/Data/week_games.csv")
    qb_df <- na.omit(players_df[players_df$position=="QB",])
    pos_df <- data.frame(nflId=players_df$nflId, PositionAbbr=players_df$position)

<br /> It should be noted that apart from the analysis presented in the
paper, for the sake of speed we present the same methodological analysis
using only a one game sample. If you wish to reproduce the entirety of
the analysis, download the original tracking data and use all six
elements of `file_vec` and `gameId_vec`. <br /> Below, we show
`game_conv_fun`, the function used to convert tracking data into a
manipulable form to be used in our analysis. The inputs to this function
are `file_vec` and `gameId_vec`. The former is a vector containing the
file names of the tracking data to be analyzed in a `csv.gz` format, and
the latter is a vector containing the game Id’s associated with the
games contained in the `file_vec`.

    # setwd("")
    file_vec <- c("broncos-at-jets-2020-reg-4.csv.gz", "chargers-at-saints-2020-reg-5.csv.gz",
                   "chiefs-at-ravens-2020-reg-3.csv.gz", "eagles-at-49ers-2020-reg-4.csv.gz",
                   "falcons-at-packers-2020-reg-4.csv.gz","vikings-at-seahawks-2020-reg-5.csv.gz") #vector of file names, function input
    gameId_vec <- c("2020_04_DEN_NYJ", "2020_05_LAC_NO", "2020_03_KC_BAL", "2020_04_PHI_SF", "2020_04_ATL_GB", "2020_05_MIN_SEA") #vector of gameId's corresponding to files, function input

    file_vec <- file_vec[1]
    gameId_vec <- gameId_vec[1]

    #we create this function to convert the raw data into a manipulable form
    game_conv_fun <- function(file_vec, gameId_vec){
      game_dt_list <- list()
      i <- 1
      for (file_name in file_vec) {
        dt <-  fread(file_name)
        dt <- as.data.frame(sapply(dt, function(x) gsub("'", "", x)))
        dt <- as.data.frame(sapply(dt, function(x) gsub("\\[", "", x)))
        dt <- as.data.frame(sapply(dt, function(x) gsub("\\]", "", x)))
        
        dt$V1 <- as.numeric(as.character(dt$V1))
        dt$play_id <- as.numeric(as.character(dt$play_id))
        
        dt_home <- (separate_rows(dt, data.home_player_x_coord,data.home_player_y_coord, data.home_player_id,
                                  sep=",", convert = TRUE))
        dt_home[ ,c('data.home_player_firstname', 'data.home_player_lastname', 'data.home_player_position',
                    'data.home_jersey_number','data.home_jeresy_number','data.home_jerset_number',
                    'data.home_jerest_number','data.away_player_x_coord','data.away_player_y_coord',
                    'data.away_player_id','data.away_player_firstname','data.away_player_lastname',
                    'data.away_player_position','data.away_jersey_number','data.away_jeresy_number',
                    'data.away_jerset_number','data.away_jerest_number','data.ball_x','data.ball_y')] <- list(NULL)
        
        dt_home$data.home_player_x_coord <- as.numeric(as.character(dt_home$data.home_player_x_coord))
        dt_home$data.home_player_y_coord <- as.numeric(as.character(dt_home$data.home_player_y_coord))
        dt_home$data.home_player_id <- as.numeric(as.character(dt_home$data.home_player_id))
        dt_home <- dt_home[dt_home$data.home_player_x_coord>30 & dt_home$data.home_player_x_coord<750 & dt_home$data.home_player_y_coord>35 & dt_home$data.home_player_y_coord<350,]
        
        dt_away <- (separate_rows(dt, data.away_player_x_coord,data.away_player_y_coord, data.away_player_id,
                                  sep=",", convert = TRUE))
        dt_away[ ,c('data.away_player_firstname', 'data.away_player_lastname', 'data.away_player_position',
                    'data.away_jersey_number','data.away_jeresy_number','data.away_jerset_number',
                    'data.away_jerest_number','data.home_player_x_coord','data.home_player_y_coord',
                    'data.home_player_id','data.home_player_firstname','data.home_player_lastname',
                    'data.home_player_position','data.home_jersey_number','data.home_jeresy_number',
                    'data.home_jerset_number','data.home_jerest_number','data.ball_x','data.ball_y')] <- list(NULL)
        
        dt_away$data.away_player_x_coord <- as.numeric(as.character(dt_away$data.away_player_x_coord))
        dt_away$data.away_player_y_coord <- as.numeric(as.character(dt_away$data.away_player_y_coord))
        dt_away$data.away_player_id <- as.integer(as.character(dt_away$data.away_player_id))
        dt_away <- dt_away[dt_away$data.away_player_x_coord>30 & dt_away$data.away_player_x_coord<750 & dt_away$data.away_player_y_coord>35 & dt_away$data.away_player_y_coord<350,]
        
        dt_ball <- (separate_rows(dt, data.ball_x,data.ball_y,
                                  sep=",", convert = TRUE))
        dt_ball[ ,c('data.home_player_x_coord', 'data.home_player_y_coord', 'data.home_player_id',
                    'data.home_player_firstname','data.home_player_lastname','data.home_player_position',
                    'data.home_jersey_number','data.home_jeresy_number','data.home_jerset_number',
                    'data.home_jerest_number','data.away_player_x_coord','data.away_player_y_coord',
                    'data.away_player_id','data.away_player_firstname','data.away_player_lastname',
                    'data.away_player_position','data.away_jersey_number','data.away_jeresy_number',
                    'data.away_jerset_number', 'data.away_jerest_number')] <- list(NULL)
        
        dt_ball$data.ball_x <- as.numeric(as.character(dt_ball$data.ball_x))
        dt_ball$data.ball_y <- as.numeric(as.character(dt_ball$data.ball_y))
        dt_ball <- dt_ball[dt_ball$data.ball_x>30 & dt_ball$data.ball_x<750 & dt_ball$data.ball_y>35 & dt_ball$data.ball_y<350,]
        dt_ball$gen_indexer <- 1:nrow(dt_ball)
        
        home_v1_agg <- aggregate(data.home_player_id~V1, dt_home, length)
        away_v1_agg <- aggregate(data.away_player_id~V1, dt_away, length)
        ball_v1_agg <- aggregate(gen_indexer~V1, dt_ball, length)
        
        home_v1_agg <- home_v1_agg[home_v1_agg$data.home_player_id==11,]
        away_v1_agg <- away_v1_agg[away_v1_agg$data.away_player_id==11,]
        ball_v1_agg <- ball_v1_agg[ball_v1_agg$gen_indexer==1,]
        
        all_v1_agg <- merge(home_v1_agg, away_v1_agg, by="V1")
        all_v1_agg <- merge(all_v1_agg, ball_v1_agg, by="V1")
        
        dt_home <- dt_home[dt_home$V1 %in% all_v1_agg$V1,] 
        dt_away <- dt_away[dt_away$V1 %in% all_v1_agg$V1,] 
        dt_ball <- dt_ball[dt_ball$V1 %in% all_v1_agg$V1,] 
        
        dt_home <- dt_home[order(dt_home$play_id, dt_home$V1),]
        dt_away <- dt_away[order(dt_away$play_id, dt_away$V1),]
        dt_ball <- dt_ball[order(dt_ball$play_id, dt_ball$V1),]
        
        dt_home$V2 <- sort(rep(1:(nrow(dt_home)/11), 11))
        dt_away$V2 <- sort(rep(1:(nrow(dt_away)/11), 11))
        dt_ball$V2 <- sort(rep(1:(nrow(dt_ball)), 1))
        
        home_play_agg <- aggregate(V2~play_id, dt_home, length)
        home_play_agg$tot_frames <- (home_play_agg$V2)/11
        all_frames_vec <- as.vector(ceiling(1:sum(home_play_agg$tot_frames)/11))
        home_play_agg$V2 <- NULL
        home_play_agg$tot_frames_cumsum <- c(0,cumsum(home_play_agg$tot_frames)[-length(home_play_agg$tot_frames)])
        all_frames_df <- data.frame(play_id=(home_play_agg[rep(row.names(home_play_agg), 11*home_play_agg$tot_frames), 1]), V2=dt_home$V2)
        home_play_agg$tot_frames <- NULL
        all_frames_df <- merge(all_frames_df, home_play_agg, by="play_id")
        all_frames_df$frameId <- all_frames_df$V2-all_frames_df$tot_frames_cumsum
        all_frames_df <- all_frames_df[seq(1, nrow(all_frames_df), 11), ]
        all_frames_df$tot_frames_cumsum <- NULL
        all_frames_df$play_id <- NULL
        
        dt_home <- merge(dt_home, all_frames_df, by=("V2"))
        dt_away <- merge(dt_away, all_frames_df, by=("V2"))
        dt_ball <- merge(dt_ball, all_frames_df, by=("V2"))
        
        names(dt_home)[names(dt_home) == "data.home_player_x_coord"] <- "x_coords"
        names(dt_home)[names(dt_home) == "data.home_player_y_coord"] <- "y_coords"
        names(dt_home)[names(dt_home) == "data.home_player_id"] <- "nflId"
        dt_home$team <- "home"
        
        names(dt_away)[names(dt_away) == "data.away_player_x_coord"] <- "x_coords"
        names(dt_away)[names(dt_away) == "data.away_player_y_coord"] <- "y_coords"
        names(dt_away)[names(dt_away) == "data.away_player_id"] <- "nflId"
        dt_away$team <- "away"
        
        names(dt_ball)[names(dt_ball) == "data.ball_x"] <- "x_coords"
        names(dt_ball)[names(dt_ball) == "data.ball_y"] <- "y_coords"
        dt_ball$nflId <- -1
        dt_ball$team <- "ball"
        dt_ball$gen_indexer <- NULL
        
        dt_all <- bind_rows(dt_home, dt_away, dt_ball)
        names(dt_all)[names(dt_all) == "data.event"] <- "event"
        names(dt_all)[names(dt_all) == "play_id"] <- "playId"
        dt_all$V1 <- NULL
        dt_all$V2 <- NULL
        dt_all$x_coords <- (dt_all$x_coords-30)/6
        dt_all$y_coords <- (dt_all$y_coords-30)/6
        
        dt_all$gameId <- gameId_vec[i]
        
        game_dt_list[[i]] <- dt_all
        
        i <- i+1
      }
      dt_binded_exp <- rbindlist(game_dt_list)
      
      dt_binded_exp[ ,c('data.pass_chance','data.exact_los','data.exact_first_down')] <- list(NULL)
      return(dt_binded_exp)
    } 

    # week_games_df <- game_conv_fun(file_vec, gameId_vec)

<br /> The `base_vor_fun` function below returns information concerning
Voronoi area for each unique inputted frame. The function calls upon the
`game_conv_fun` function, and thus inputs `file_vec` and `gameId_vec`,
as well as `playId` for our demonstrative analysis (for the sake of
speed, we’ll only run the function on one play), though this can be
removed and the function can be easily altered to iterate through all
plays within the selected sample of games.

    base_vor_fun <- function(file_vec, gameId_vec, playId){
      # week_games_df <- game_conv_fun(file_vec, gameId_vec)
      week_games_df$event[week_games_df$event == "Pass Attempt"] <- "pass_forward"
      week_games_df$event[week_games_df$event == "Snap"] <- "ball_snap"
      
      return_list <- list()
      vor_plot_list <- list()
      vor_info_list <- list()
      k <- 1
      for (game_id in unique(gameId_vec)) {
        # print("game")
        # print(k)
        ind_game_df <- week_games_df[week_games_df$gameId==game_id,]
        ind_game_df <- merge(ind_game_df, game_play_df, by=c("gameId", "playId"))
        
        
        ball_df <- ind_game_df[ind_game_df$nflId==-1,]
        
        pass_forward_events <- (unique((ind_game_df[ind_game_df$play_type=="pass",])$playId))
        pass_forward_events <- pass_forward_events[!is.na(pass_forward_events)]
        
        
        pass_forward_info_df <- ind_game_df[ind_game_df$event=="pass_forward" & ind_game_df$playId %in% pass_forward_events,]
        pass_forward_events_df <- aggregate(frameId ~ playId, data = pass_forward_info_df , min)
        
        vor_plot_play_list <- list()
        vor_info_play_list <- list()
        j <- 1
        for (play_id in unique(playId)) {
          # print("play")
          # print(j)
          full_play_df <- ind_game_df[ind_game_df$playId==play_id,]
          line_set_frame <- min(unique(full_play_df[full_play_df$event=="Line Set",]$frameId))
          if (line_set_frame==Inf) {
            line_set_frame <- min(unique(na.omit(full_play_df[full_play_df$event=="ball_snap",]$frameId)))-1
            if (line_set_frame<1) {
              next
            }
          }
          initial_configuration_df <- full_play_df[full_play_df$frameId==line_set_frame & full_play_df$nflId==-1,]
          spec_frame_id <- pass_forward_events_df[pass_forward_events_df$playId==play_id,]$frameId
          
          ball_snap_frame <- min(unique(na.omit(full_play_df[full_play_df$event=="ball_snap",]$frameId)))
          pass_forward_frame <- min(unique(na.omit(full_play_df[full_play_df$event=="pass_forward",]$frameId)))
          
          if (ball_snap_frame==Inf | pass_forward_frame==Inf | line_set_frame==Inf) {
            next
          }
          
          ball_snap_configuration_df <- full_play_df[full_play_df$frameId==ball_snap_frame & full_play_df$nflId==-1,]
          
          time_of_pass_df <- full_play_df[full_play_df$frameId==spec_frame_id & full_play_df$nflId==-1,]
          
          qb_id_df <- full_play_df[full_play_df$nflId %in% qb_df$nflId & full_play_df$frameId==ball_snap_frame,]
          if (nrow(qb_id_df)>1) {
            qb_id_df$ball_dist <- sqrt(((ball_snap_configuration_df$x_coords-qb_id_df$x_coords)^2)+((ball_snap_configuration_df$y_coords-qb_id_df$y_coords)^2))
            qb_id_df <- qb_id_df[qb_id_df$ball_dist==min(qb_id_df$ball_dist),]
          }
          time_of_pass_qb_df <- full_play_df[full_play_df$frameId==spec_frame_id & full_play_df$nflId==qb_id_df$nflId,]
          
          qb_tot_df <- full_play_df[full_play_df$nflId %in% qb_id_df$nflId & full_play_df$frameId==spec_frame_id,]  #isolating the voronoi coordinates for the quarterback
          LOS_x <- initial_configuration_df$x_coords
          LOS_y <- initial_configuration_df$y_coords
          
          if (LOS_x>time_of_pass_qb_df$x_coords) {
            field_progress_indicator <- -1 
            
          } else {
            field_progress_indicator <- 1
          }
          
          norm_full_play_df <- full_play_df
          norm_full_play_df$x_coords <- (full_play_df$x_coords-LOS_x)*-1*field_progress_indicator
          norm_full_play_df$y_coords <- (full_play_df$y_coords-(160/6))*field_progress_indicator
          offensive_team <- unique(norm_full_play_df[norm_full_play_df$nflId %in% qb_df$nflId,]$team)
          
          time_of_pass_norm_df <- norm_full_play_df[norm_full_play_df$frameId==(spec_frame_id) & norm_full_play_df$nflId==-1,]
          time_before_pass_norm_df <- norm_full_play_df[norm_full_play_df$frameId==(spec_frame_id-1) & norm_full_play_df$nflId==-1,]
          
          pass_angle <- (atan((time_of_pass_norm_df$x_coords-time_before_pass_norm_df$x_coords)/(time_of_pass_norm_df$y_coords-time_before_pass_norm_df$y_coords)))*(180/pi)
          if (is.na(pass_angle)==TRUE) {
            pass_angle <- NA
          } else if (pass_angle<0) {
            pass_angle <- pass_angle+180
          }
          
          xbound1 <- -10
          ybound1 <- ((LOS_y-(160/6))*field_progress_indicator)+6
          ybound2 <- ((LOS_y-(160/6))*field_progress_indicator)-6
          
          play_frame_duration <- (pass_forward_frame-ball_snap_frame)+1
          
          norm_full_play_in_pocket_df <- norm_full_play_df[norm_full_play_df$nflId!=-1,] #omitting ball from dataframe
          norm_full_play_in_pocket_df <- norm_full_play_in_pocket_df[norm_full_play_in_pocket_df$frameId>=ball_snap_frame & norm_full_play_in_pocket_df$frameId<=pass_forward_frame,]
          norm_full_play_in_pocket_df <- norm_full_play_in_pocket_df[norm_full_play_in_pocket_df$y_coords<ybound1 & norm_full_play_in_pocket_df$y_coords>ybound2,]
          norm_full_play_in_pocket_df <- norm_full_play_in_pocket_df[rowSums(is.na(norm_full_play_in_pocket_df)) != ncol(norm_full_play_in_pocket_df), ]
          full_play_pocket_df <- norm_full_play_in_pocket_df[(norm_full_play_in_pocket_df$x_coords>xbound1 & norm_full_play_in_pocket_df$x_coords<0),]
          
          y_coord_adj <- (norm_full_play_df[norm_full_play_df$frameId==1 & norm_full_play_df$nflId==-1,]$y_coords)
          
          player_in_pocket_frame_count_df<- (aggregate(frameId ~ nflId, data = full_play_pocket_df , length))
          player_in_pocket_frame_count_df$freq <- player_in_pocket_frame_count_df$frameId/play_frame_duration
          adj_player_in_pocket_df <- player_in_pocket_frame_count_df[player_in_pocket_frame_count_df$freq>=0.5,]
          
          
          pocket_players_at_snap_df <- norm_full_play_df[norm_full_play_df$frameId==ball_snap_frame & norm_full_play_df$nflId %in% adj_player_in_pocket_df$nflId,]
          rel_pos_df <- merge(pocket_players_at_snap_df, pos_df, by="nflId")
          rel_pos_df <- data.frame(nflId=rel_pos_df$nflId, "PositionAbbr"=rel_pos_df$PositionAbbr)
          pocket_players_at_snap_df <- merge(rel_pos_df,pocket_players_at_snap_df, by="nflId", all=T)
          pocket_players_at_snap_df$PositionAbbr[is.na(pocket_players_at_snap_df$PositionAbbr)] <- "UN"
          oline_players_at_snap_df <- pocket_players_at_snap_df[pocket_players_at_snap_df$team==offensive_team & pocket_players_at_snap_df$PositionAbbr!="QB" & pocket_players_at_snap_df$PositionAbbr!="RB",]
          qb_at_snap_df <- pocket_players_at_snap_df[pocket_players_at_snap_df$PositionAbbr=="QB",]
          if (nrow(qb_at_snap_df)==0) {
            next
          }
          qb_in_pocket_df <- adj_player_in_pocket_df[adj_player_in_pocket_df$nflId==qb_at_snap_df$nflId[1],]
          if (qb_in_pocket_df$freq<1) {
            next
          }
          
          oline_players_at_snap_df <- oline_players_at_snap_df[order(oline_players_at_snap_df$y_coords),]
          if (nrow(oline_players_at_snap_df)>0) {
            oline_players_at_snap_df$ordering <- 1:nrow(oline_players_at_snap_df)
          } else {
            oline_players_at_snap_df$ordering <- numeric(nrow(oline_players_at_snap_df))
          }
          oline_ordering_df <- data.frame(nflId=oline_players_at_snap_df$nflId, ordering=oline_players_at_snap_df$ordering)
          
          voronoi_x_list <- list()
          voronoi_y_list <- list()
          voronoi_nflId_list <- list()
          voronoi_frameid_list <- list()
          
          voronoi_area_vec <- rep(NA, times=length(ball_snap_frame:max(full_play_df$frameId)))
          rect <- data.frame(x=c(xbound1, 0, 0, xbound1, xbound1), y=c(ybound1, ybound1, ybound2, ybound2, ybound1))
          
          oline_ordering_wQB_df <- oline_ordering_df
          oline_ordering_wQB_df[nrow(oline_ordering_wQB_df)+1,] <- c(qb_at_snap_df$nflId, 0)
          
          
          vor_plot_frame_list <- list()
          vor_info_frame_list <- list()
          i <- 1
          for (frame_id in c(ball_snap_frame:pass_forward_frame)) {
            # print("frame")
            # print(i)
            
            spec_frame_df <- norm_full_play_df[norm_full_play_df$frameId==frame_id,]
            frame_omit_ball_df <- spec_frame_df[spec_frame_df$nflId!=-1,] #omitting ball from dataframe
            spec_frame_wo_ball_df <- frame_omit_ball_df[frame_omit_ball_df$y_coords<ybound1 & frame_omit_ball_df$y_coords>ybound2,]
            spec_frame_wo_ball_df <- spec_frame_wo_ball_df[rowSums(is.na(spec_frame_wo_ball_df)) != ncol(spec_frame_wo_ball_df), ]
            
            spec_players_df <- spec_frame_df[spec_frame_df$nflId %in% adj_player_in_pocket_df$nflId,]
            spec_players_df <- (merge(spec_players_df, rel_pos_df, by="nflId", all=T))
            spec_players_df$PositionAbbr[is.na(spec_players_df$PositionAbbr)] <- "UN"
            
            player_team_df <- data.frame(nflId=spec_players_df$nflId, team=spec_players_df$team)
            
            off_players_df <- spec_players_df[spec_players_df$team==offensive_team,]
            def_players_df <- frame_omit_ball_df[frame_omit_ball_df$team!=offensive_team,]
            oline_df <- off_players_df[off_players_df$PositionAbbr!="QB" & off_players_df$PositionAbbr!="RB",]
            oline_wQB_df <- off_players_df[off_players_df$PositionAbbr!="RB",]
            oline_df <- merge(oline_df, oline_ordering_df, by="nflId")
            oline_wQB_df <- merge(oline_wQB_df, oline_ordering_wQB_df, by="nflId")
            qb_frame_df <- off_players_df[off_players_df$PositionAbbr=="QB",]
            
            if (nrow(oline_df)>0) {
              oline_df$relevant_frame <- (oline_df$frameId-ball_snap_frame)+1
              oline_df$players_on_line <- nrow(oline_df)
              
              oline_wQB_df$relevant_frame <- (oline_wQB_df$frameId-ball_snap_frame)+1
              oline_wQB_df$players_on_line <- nrow(oline_df)
            } else {
              oline_df$relevant_frame = numeric(nrow(oline_df))
              oline_df$players_on_line = numeric(nrow(oline_df))
              
              oline_wQB_df$relevant_frame = numeric(nrow(oline_wQB_df))
              oline_wQB_df$players_on_line = numeric(nrow(oline_wQB_df))
            }
            
            pocket_df <- spec_frame_wo_ball_df[(spec_frame_wo_ball_df$x_coords>xbound1 & spec_frame_wo_ball_df$x_coords<0),]
            def_players_df$PositionAbbr <- NULL
            off_vor_calc_df <- spec_frame_wo_ball_df[spec_frame_wo_ball_df$nflId %in% oline_wQB_df$nflId,]

            vor_calc_df <- rbind(off_vor_calc_df, def_players_df)
            vor_calc_df$duplicated <- duplicated(vor_calc_df[,3:4])
            vor_calc_df <- vor_calc_df[vor_calc_df$duplicated==FALSE,]
            vor_calc_df$duplicated <- NULL
            
            vor_calc_df <- as.data.frame(vor_calc_df)
            vor_spdf <- voronoi_polygon(data=vor_calc_df,x="x_coords",y="y_coords", outline=rect)
            vor_df <- fortify_voronoi(vor_spdf)
            # vor_df <- merge(vor_df, player_guide_df, by=c("nflId"))
            mp_sf <- st_as_sf(vor_spdf)
            p_sf <- st_cast(mp_sf, "POLYGON")
            p_qb_sf <- p_sf[p_sf$nflId==qb_frame_df$nflId,]
            if (nrow(p_qb_sf)==0) {
              next
            }
            p_qb_sf_line <- p_sf[p_sf$team==offensive_team,]
            area <- st_area(p_qb_sf)
            area_line <- sum(st_area(p_qb_sf_line))
            
            # p_sf <- merge(p_sf, player_guide_df, by=c("nflId"))
            
            area_info_df <- data.frame(nflId=p_qb_sf$nflId, qb_tot_area=area, line_tot_area=area_line)
            
            
            area_info_df$frameId <- frame_id
            area_info_df$playId <- play_id
            area_info_df$gameId <- game_id
            area_info_df$relevant_frame <- (area_info_df$frameId-ball_snap_frame)+1
            
            
            
            p1 <- ggplot() + geom_polygon(data=vor_df, aes(x=y, y=x,group=group,fill=team), color="white")+
              # scale_fill_manual(values = c("firebrick1","purple3"))+
              geom_path(data=rect, aes(x=y, y=x))+
              # geom_point(aes(x=qb_frame_df$y_coords, y=qb_frame_df$x_coords))+
              geom_label_repel(data=p_sf, aes(x=y_coords, y=x_coords, label=nflId, color=team),size=3, fontface = "bold")+
              # theme(legend.position = "none")+
              # scale_color_manual(values = c("black","gold3"))
              guides(color=FALSE, fill=guide_legend(title="Team"))
            
            
            
            vor_plot_frame_list[[i]] <- p1
            vor_info_frame_list[[i]] <- area_info_df
            
            
            i <- i+1 
          }
          vor_plot_play_list[[j]] <- (vor_plot_frame_list)
          vor_info_play_list[[j]] <- rbindlist(vor_info_frame_list)
          j <- j+1
        }
        vor_plot_list[[k]] <- (vor_plot_play_list)
        vor_info_list[[k]] <- rbindlist(vor_info_play_list)
        k <- k+1
      }
      return_list[[1]] <- vor_plot_list
      return_list[[2]] <- vor_info_list
      return(return_list)
    }

<br /> The `base_vor_fun` function returns a four-dimensional list,
where the first inputted dimension of the list dictates whether to
return a plot or a data frame (1 returning a plot, 2 returning a data
frame). If you are attempting to return a data frame, only return one
dimension, with the command `base_vor_fun()[2]`. If you are attempting
to return a plot, use all four dimensions, with the second inputted
dimension dictating which game to return information from (1 being the
first game listed above in `file_vec`, 2 being the second, etc.), the
third inputted dimension dictating which play to return information from
(1 being the first passing play in the selected game, 2 being the
second, etc.), and the fourth inputted dimension dictating which frame
to return information from (1 being the first frame after the snap in
the selected play, 2 being the second, etc.). In the example below, we
produce a Voronoi plot of the pocket 15 frames after the snap in the
first passing play of the 1st game of our sample and the play’s
corresponding data frame.

    print(base_vor_fun(file_vec[1],gameId_vec[1], 76)[[1]][[1]][[1]][[15]])

![](readme_files/figure-markdown_strict/vor_plot-1.png)

    print(base_vor_fun(file_vec[1],gameId_vec[1], 76)[[2]])

    ## [[1]]
    ##       nflId qb_tot_area line_tot_area frameId playId          gameId
    ##  1: 2561036    68.50576     117.99614      64     76 2020_04_DEN_NYJ
    ##  2: 2561036    66.99302     117.80647      65     76 2020_04_DEN_NYJ
    ##  3: 2561036    65.20761     117.08635      66     76 2020_04_DEN_NYJ
    ##  4: 2561036    63.46892     116.25668      67     76 2020_04_DEN_NYJ
    ##  5: 2561036    61.71126     115.27737      68     76 2020_04_DEN_NYJ
    ##  6: 2561036    59.95518     114.15205      69     76 2020_04_DEN_NYJ
    ##  7: 2561036    58.22037     112.86017      70     76 2020_04_DEN_NYJ
    ##  8: 2561036    56.45600     111.17128      71     76 2020_04_DEN_NYJ
    ##  9: 2561036    54.71630     109.36470      72     76 2020_04_DEN_NYJ
    ## 10: 2561036    52.98615     107.51260      73     76 2020_04_DEN_NYJ
    ## 11: 2561036    50.69294     105.14756      74     76 2020_04_DEN_NYJ
    ## 12: 2561036    48.21517     102.20283      75     76 2020_04_DEN_NYJ
    ## 13: 2561036    45.94444      98.92891      76     76 2020_04_DEN_NYJ
    ## 14: 2561036    43.85743      95.47251      77     76 2020_04_DEN_NYJ
    ## 15: 2561036    41.93494      91.85902      78     76 2020_04_DEN_NYJ
    ## 16: 2561036    40.14245      88.05711      79     76 2020_04_DEN_NYJ
    ## 17: 2561036    38.47742      84.26443      80     76 2020_04_DEN_NYJ
    ## 18: 2561036    36.75487      80.71076      81     76 2020_04_DEN_NYJ
    ## 19: 2561036    35.01420      77.68055      82     76 2020_04_DEN_NYJ
    ## 20: 2561036    33.25234      75.35112      83     76 2020_04_DEN_NYJ
    ## 21: 2561036    31.76385      75.08665      84     76 2020_04_DEN_NYJ
    ## 22: 2561036    30.08902      73.93833      85     76 2020_04_DEN_NYJ
    ##       nflId qb_tot_area line_tot_area frameId playId          gameId
    ##     relevant_frame
    ##  1:              1
    ##  2:              2
    ##  3:              3
    ##  4:              4
    ##  5:              5
    ##  6:              6
    ##  7:              7
    ##  8:              8
    ##  9:              9
    ## 10:             10
    ## 11:             11
    ## 12:             12
    ## 13:             13
    ## 14:             14
    ## 15:             15
    ## 16:             16
    ## 17:             17
    ## 18:             18
    ## 19:             19
    ## 20:             20
    ## 21:             21
    ## 22:             22
    ##     relevant_frame

<br /> The `bin_fun` function below returns information concerning bin
area for each unique inputted frame. The function calls upon the
`game_conv_fun` function, and thus inputs `file_vec` and `gameId_vec`,
as well as `playId` for our demonstrative analysis (for the sake of
speed, we’ll only run the function on one play), though this can be
removed and the function can be easily altered to iterate through all
plays within the selected sample of games.

    bin_fun <- function(file_vec, gameId_vec, playId){
      # week_games_df <- game_conv_fun(file_vec, gameId_vec)
      week_games_df$event[week_games_df$event == "Pass Attempt"] <- "pass_forward"
      week_games_df$event[week_games_df$event == "Snap"] <- "ball_snap"
      
      return_list <- list()
      bin_info_game_list <- list()

      k <- 1
      for (game_id in unique(week_games_df$gameId)[1]) {
        # print("game")
        # print(k)
        ind_game_df <- week_games_df[week_games_df$gameId==game_id,]
        ind_game_df <- merge(ind_game_df, game_play_df, by=c("gameId", "playId"))
        
        ball_df <- ind_game_df[ind_game_df$nflId==-1,]
        
        pass_forward_events <- (unique((ind_game_df[ind_game_df$play_type=="pass",])$playId))
        pass_forward_events <- pass_forward_events[!is.na(pass_forward_events)]
        
        
        pass_forward_info_df <- ind_game_df[ind_game_df$event=="pass_forward" & ind_game_df$playId %in% pass_forward_events,]
        pass_forward_events_df <- aggregate(frameId ~ playId, data = pass_forward_info_df , min)
        
        qb_vor_area_info_play_list <- list()
        qb_det_vor_area_play_list <- list()
        procrustes_info_play_list <- list()
        bin_info_play_list <- list()
        
        
        j <- 1
        for (play_id in unique(pass_forward_events_df$playId)[1]) {
          # print("play")
          # print(j)
          full_play_df <- ind_game_df[ind_game_df$playId==play_id,]
          line_set_frame <- min(unique(full_play_df[full_play_df$event=="Line Set",]$frameId))
          if (line_set_frame==Inf) {
            line_set_frame <- min(unique(na.omit(full_play_df[full_play_df$event=="ball_snap",]$frameId)))-1
            if (line_set_frame<1) {
              next
            }
          }
          initial_configuration_df <- full_play_df[full_play_df$frameId==line_set_frame & full_play_df$nflId==-1,]
          spec_frame_id <- pass_forward_events_df[pass_forward_events_df$playId==play_id,]$frameId
          
          ball_snap_frame <- min(unique(na.omit(full_play_df[full_play_df$event=="ball_snap",]$frameId)))
          pass_forward_frame <- min(unique(na.omit(full_play_df[full_play_df$event=="pass_forward",]$frameId)))
          
          if (ball_snap_frame==Inf | pass_forward_frame==Inf | line_set_frame==Inf) {
            next
          }
          
          ball_snap_configuration_df <- full_play_df[full_play_df$frameId==ball_snap_frame & full_play_df$nflId==-1,]
          
          time_of_pass_df <- full_play_df[full_play_df$frameId==spec_frame_id & full_play_df$nflId==-1,]
          
          qb_id_df <- full_play_df[full_play_df$nflId %in% qb_df$nflId & full_play_df$frameId==ball_snap_frame,]
          if (nrow(qb_id_df)>1) {
            qb_id_df$ball_dist <- sqrt(((ball_snap_configuration_df$x_coords-qb_id_df$x_coords)^2)+((ball_snap_configuration_df$y_coords-qb_id_df$y_coords)^2))
            qb_id_df <- qb_id_df[qb_id_df$ball_dist==min(qb_id_df$ball_dist),]
          }
          time_of_pass_qb_df <- full_play_df[full_play_df$frameId==spec_frame_id & full_play_df$nflId==qb_id_df$nflId,]
          
          qb_tot_df <- full_play_df[full_play_df$nflId %in% qb_id_df$nflId & full_play_df$frameId==spec_frame_id,]  #isolating the voronoi coordinates for the quarterback
          LOS_x <- initial_configuration_df$x_coords
          LOS_y <- initial_configuration_df$y_coords
          
          if (LOS_x>time_of_pass_qb_df$x_coords) {
            field_progress_indicator <- -1 ####make this value 1 or -1, calculate in play_id for loop. -1 means LOS_x>qb initial position, 1 means LOS_x<qb initial position
            
          } else {
            field_progress_indicator <- 1
          }
          
          norm_full_play_df <- full_play_df
          norm_full_play_df$x_coords <- (full_play_df$x_coords-LOS_x)*-1*field_progress_indicator
          norm_full_play_df$y_coords <- (full_play_df$y_coords-(160/6))*field_progress_indicator
          offensive_team <- unique(norm_full_play_df[norm_full_play_df$nflId %in% qb_df$nflId,]$team)
          
          time_of_pass_norm_df <- norm_full_play_df[norm_full_play_df$frameId==(spec_frame_id) & norm_full_play_df$nflId==-1,]
          time_before_pass_norm_df <- norm_full_play_df[norm_full_play_df$frameId==(spec_frame_id-10) & norm_full_play_df$nflId==-1,]
          time_after_pass_norm_df <- norm_full_play_df[norm_full_play_df$frameId==(spec_frame_id+10) & norm_full_play_df$nflId==-1,]
          
          if (nrow(time_of_pass_norm_df)==0 | nrow(time_after_pass_norm_df)==0) {
            pass_angle <- NA
          } else {
            pass_angle <- (atan((time_of_pass_norm_df$x_coords-time_after_pass_norm_df$x_coords)/(time_of_pass_norm_df$y_coords-time_after_pass_norm_df$y_coords)))*(180/pi)
            if (is.na(pass_angle)==TRUE) {
              pass_angle <- NA
            } else if (pass_angle<0) {
              pass_angle <- pass_angle+180
            }
          }
          
          
          
          xbound1 <- -10
          ybound1 <- ((LOS_y-(160/6))*field_progress_indicator)+6
          ybound2 <- ((LOS_y-(160/6))*field_progress_indicator)-6
          
          play_frame_duration <- (pass_forward_frame-ball_snap_frame)+1
          
          norm_full_play_in_pocket_df <- norm_full_play_df[norm_full_play_df$nflId!=-1,] #omitting ball from dataframe
          norm_full_play_in_pocket_df <- norm_full_play_in_pocket_df[norm_full_play_in_pocket_df$frameId>=ball_snap_frame & norm_full_play_in_pocket_df$frameId<=pass_forward_frame,]
          norm_full_play_in_pocket_df <- norm_full_play_in_pocket_df[norm_full_play_in_pocket_df$y_coords<ybound1 & norm_full_play_in_pocket_df$y_coords>ybound2,]
          norm_full_play_in_pocket_df <- norm_full_play_in_pocket_df[rowSums(is.na(norm_full_play_in_pocket_df)) != ncol(norm_full_play_in_pocket_df), ]
          full_play_pocket_df <- norm_full_play_in_pocket_df[(norm_full_play_in_pocket_df$x_coords>xbound1 & norm_full_play_in_pocket_df$x_coords<0),]
          
          y_coord_adj <- (norm_full_play_df[norm_full_play_df$frameId==1 & norm_full_play_df$nflId==-1,]$y_coords)
          
          player_in_pocket_frame_count_df<- (aggregate(frameId ~ nflId, data = full_play_pocket_df , length))
          player_in_pocket_frame_count_df$freq <- player_in_pocket_frame_count_df$frameId/play_frame_duration
          adj_player_in_pocket_df <- player_in_pocket_frame_count_df[player_in_pocket_frame_count_df$freq>=0.5,]
          
          
          pocket_players_at_snap_df <- norm_full_play_df[norm_full_play_df$frameId==ball_snap_frame & norm_full_play_df$nflId %in% adj_player_in_pocket_df$nflId,]
          rel_pos_df <- merge(pocket_players_at_snap_df, pos_df, by="nflId")
          rel_pos_df <- data.frame(nflId=rel_pos_df$nflId, "PositionAbbr"=rel_pos_df$PositionAbbr)
          pocket_players_at_snap_df <- merge(rel_pos_df,pocket_players_at_snap_df, by="nflId", all=T)
          pocket_players_at_snap_df$PositionAbbr[is.na(pocket_players_at_snap_df$PositionAbbr)] <- "UN"
          oline_players_at_snap_df <- pocket_players_at_snap_df[pocket_players_at_snap_df$team==offensive_team & pocket_players_at_snap_df$PositionAbbr!="QB" & pocket_players_at_snap_df$PositionAbbr!="RB",]
          qb_at_snap_df <- pocket_players_at_snap_df[pocket_players_at_snap_df$PositionAbbr=="QB",]
          if (nrow(qb_at_snap_df)==0) {
            next
          }
          qb_in_pocket_df <- adj_player_in_pocket_df[adj_player_in_pocket_df$nflId==qb_at_snap_df$nflId[1],]
          if (qb_in_pocket_df$freq<1) {
            next
          }
          
          oline_players_at_snap_df <- oline_players_at_snap_df[order(oline_players_at_snap_df$y_coords),]
          if (nrow(oline_players_at_snap_df)>0) {
            oline_players_at_snap_df$ordering <- 1:nrow(oline_players_at_snap_df)
          } else {
            oline_players_at_snap_df$ordering <- numeric(nrow(oline_players_at_snap_df))
          }
          oline_ordering_df <- data.frame(nflId=oline_players_at_snap_df$nflId, ordering=oline_players_at_snap_df$ordering)
          
          voronoi_x_list <- list()
          voronoi_y_list <- list()
          voronoi_nflId_list <- list()
          voronoi_frameid_list <- list()
          
          voronoi_area_vec <- rep(NA, times=length(ball_snap_frame:max(full_play_df$frameId)))
          rect = data.frame(x=c(xbound1, 0, 0, xbound1, xbound1), y=c(ybound1, ybound1, ybound2, ybound2, ybound1))
          
          oline_ordering_wQB_df <- oline_ordering_df
          oline_ordering_wQB_df[nrow(oline_ordering_wQB_df)+1,] <- c(qb_at_snap_df$nflId, 0)
          
          bin_info_frame_list <- list()
          
          i <- 1
          for (frame_id in c(ball_snap_frame:pass_forward_frame)) {
            # print("frame")
            # print(i)
            
            spec_frame_df <- norm_full_play_df[norm_full_play_df$frameId==frame_id,]
            frame_omit_ball_df <- spec_frame_df[spec_frame_df$nflId!=-1,] #omitting ball from dataframe
            spec_frame_wo_ball_df <- frame_omit_ball_df[frame_omit_ball_df$y_coords<ybound1 & frame_omit_ball_df$y_coords>ybound2,]
            spec_frame_wo_ball_df <- spec_frame_wo_ball_df[rowSums(is.na(spec_frame_wo_ball_df)) != ncol(spec_frame_wo_ball_df), ]

            spec_players_df <- spec_frame_df[spec_frame_df$nflId %in% adj_player_in_pocket_df$nflId,]
            spec_players_df <- (merge(spec_players_df, rel_pos_df, by="nflId", all=T))
            spec_players_df$PositionAbbr[is.na(spec_players_df$PositionAbbr)] <- "UN"
            
            player_team_df <- data.frame(nflId=spec_players_df$nflId, team=spec_players_df$team)
            
            off_players_df <- spec_players_df[spec_players_df$team==offensive_team,]
            def_players_df <- frame_omit_ball_df[frame_omit_ball_df$team!=offensive_team,]
            oline_df <- off_players_df[off_players_df$PositionAbbr!="QB" & off_players_df$PositionAbbr!="RB",]
            oline_wQB_df <- off_players_df[off_players_df$PositionAbbr!="RB",]
            oline_df <- merge(oline_df, oline_ordering_df, by="nflId")
            oline_wQB_df <- merge(oline_wQB_df, oline_ordering_wQB_df, by="nflId")
            qb_frame_df <- off_players_df[off_players_df$PositionAbbr=="QB",]
            
            if (nrow(oline_df)>0) {
              oline_df$relevant_frame <- (oline_df$frameId-ball_snap_frame)+1
              oline_df$players_on_line <- nrow(oline_df)
              
              oline_wQB_df$relevant_frame <- (oline_wQB_df$frameId-ball_snap_frame)+1
              oline_wQB_df$players_on_line <- nrow(oline_df)
            } else {
              oline_df$relevant_frame = numeric(nrow(oline_df))
              oline_df$players_on_line = numeric(nrow(oline_df))
              
              oline_wQB_df$relevant_frame = numeric(nrow(oline_wQB_df))
              oline_wQB_df$players_on_line = numeric(nrow(oline_wQB_df))
            }
            
            pocket_df <- spec_frame_wo_ball_df[(spec_frame_wo_ball_df$x_coords>xbound1 & spec_frame_wo_ball_df$x_coords<0),]
            
            def_players_df$PositionAbbr <- NULL
            off_vor_calc_df <- spec_frame_wo_ball_df[spec_frame_wo_ball_df$nflId %in% oline_wQB_df$nflId,]
            vor_calc_df <- rbind(off_vor_calc_df, def_players_df)
            vor_calc_df$duplicated <- duplicated(vor_calc_df[,3:4])
            vor_calc_df <- vor_calc_df[vor_calc_df$duplicated==FALSE,]
            vor_calc_df$duplicated <- NULL
            vor_calc_df <- as.data.frame(vor_calc_df)

            vor_spdf <- voronoi_polygon(data=vor_calc_df,x="x_coords",y="y_coords", outline=rect)
            vor_df <- fortify_voronoi(vor_spdf)
            mp_sf <- st_as_sf(vor_spdf)
            p_sf <- st_cast(mp_sf, "POLYGON")
            p_qb_sf <- p_sf[p_sf$nflId==qb_frame_df$nflId,]
            if (nrow(p_qb_sf)==0) {
              next
            }
            p_qb_sf_line <- p_sf[p_sf$team==offensive_team,]
            area <- st_area(p_qb_sf)
            area_line <- sum(st_area(p_qb_sf_line))
            
            line_area_df <- data.frame(area=st_area(p_qb_sf_line), nflId=p_qb_sf_line$nflId)
            line_red_info_df <- data.frame(nflId=oline_wQB_df$nflId, ordering=oline_wQB_df$ordering, 
                                           players_on_line=oline_wQB_df$players_on_line, PositionAbbr=oline_wQB_df$PositionAbbr,
                                           x_coords=oline_wQB_df$x_coords, y_coords=oline_wQB_df$y_coords)
            line_area_df <- merge(line_area_df, line_red_info_df, by="nflId")
            line_area_df$gameId <- game_id
            line_area_df$playId <- play_id
            line_area_df$frameId <- frame_id
            line_area_df$passing_angle <- pass_angle
            line_area_df$relevant_frame <- (frame_id-ball_snap_frame)+1
            
            area_info_df <- data.frame(nflId=p_qb_sf$nflId, qb_tot_area=area, line_tot_area=area_line)
            area_info_df$frameId <- frame_id
            area_info_df$playId <- play_id
            area_info_df$gameId <- game_id
            area_info_df$relevant_frame <- (area_info_df$frameId-ball_snap_frame)+1
            
            bin_bound_df <- data.frame(start_y_coord=rep(c(2*(0:5)+ybound2), 5), end_y_coord=rep(c(2*(1:6)+ybound2), 5))
            bin_bound_df <- bin_bound_df[order(bin_bound_df$start_y_coord),]
            bin_bound_df$start_x_coord <- rep(c(0,-2,-4,-6,-8),6)
            bin_bound_df$end_x_coord <- rep(c(-2,-4,-6,-8,-10),6)
            row.names(bin_bound_df) <- NULL
      
            bin_info_list <- list()
            for (row_indexer in c(1:nrow(bin_bound_df))) {
              spec_bin_df <- bin_bound_df[row_indexer,]
              bin_rect <- data.frame(x=c(spec_bin_df$start_x_coord, spec_bin_df$start_x_coord, spec_bin_df$end_x_coord, spec_bin_df$end_x_coord, spec_bin_df$start_x_coord),
                                     y=c(spec_bin_df$start_y_coord, spec_bin_df$end_y_coord, spec_bin_df$end_y_coord, spec_bin_df$start_y_coord, spec_bin_df$start_y_coord))
      
              bin_vor_spdf <- voronoi_polygon(data=vor_calc_df,x="x_coords",y="y_coords", outline=bin_rect)
              bin_vor_df <- fortify_voronoi(bin_vor_spdf)
              bin_mp_sf <- st_as_sf(bin_vor_spdf)
              bin_p_sf <- st_cast(bin_mp_sf, "POLYGON")

              bin_p_qb_sf_line <- bin_p_sf[bin_p_sf$team==offensive_team,]
              bin_area_line <- sum(st_area(bin_p_qb_sf_line))
      
      
      
              ind_bin_info_df <- data.frame(bin_id=row_indexer, bin_area=bin_area_line)
              bin_info_list[[row_indexer]] <- ind_bin_info_df
            }
      
            bin_info_df <- rbindlist(bin_info_list)
            bin_info_df <- bin_info_df[ , `:=` (gameId = game_id, playId = play_id, frameId=frame_id, relevant_frame=(area_info_df$frameId-ball_snap_frame)+1)]
            bin_info_frame_list[[i]] <- bin_info_df
            
            
            i <- i+1 
          }

          bin_info_play_list[[j]] <- rbindlist(bin_info_frame_list)
          j <- j+1
        }
        bin_info_game_list[[k]] <- rbindlist(bin_info_play_list)
        k <- k+1
      }
      
      bin_info_df <- rbindlist(bin_info_game_list)
      
      bin_agg_df <- aggregate(bin_area~bin_id+relevant_frame, bin_info_df, mean)
      bin_agg_df$bin_x <- rep(c(-1,-3,-5,-7,-9),6)
      bin_agg_df$bin_y <- sort(rep(c(-5,-3,-1,1,3,5),5))
      
      bin_agg_df <- bin_agg_df[bin_agg_df$relevant_frame<36,]
      
      plot_list <- list()
      for (rel_frame in c(sort(bin_agg_df$relevant_frame))) {
        plot_df <- bin_agg_df[bin_agg_df$relevant_frame==rel_frame,]
        p <- ggplot(plot_df, aes(x = bin_y, y = bin_x)) + 
          geom_raster(aes(fill=bin_area))+scale_fill_viridis()
        plot_list[[rel_frame]] <- p
      }
      return_list[[1]] <- plot_list
      return_list[[2]] <- bin_info_df
      return(return_list)
    }

<br /> Similarly to the list returned by the `base_vor_fun` function,
`bin_fun` returns a list where the first inputted dimension of the list
dictates whether to return a plot or a data frame (1 returning a plot, 2
returning a data frame). If you are attempting to return a data frame,
only return one dimension, with the command `bin_fun()[2]`. If you are
attempting to return a plot, the return list is two-dimensional, where
the second dimension dictates which frame in the play to return a plot
for (1 being the first frame after the snap, 2 being the second, etc.).
In the example below, we produce a bin plot of the pocket 15 frames
after the snap in the first passing play of the 1st game of our sample
and the play’s corresponding data frame.

    print(bin_fun(file_vec[1],gameId_vec[1], 76)[[1]][[15]])

![](readme_files/figure-markdown_strict/bin_plot-1.png)

    print(bin_fun(file_vec[1],gameId_vec[1], 76)[[2]])

    ##      bin_id bin_area          gameId playId frameId relevant_frame
    ##   1:      1 3.721732 2020_04_DEN_NYJ     76      64              1
    ##   2:      2 4.000000 2020_04_DEN_NYJ     76      64              1
    ##   3:      3 4.000000 2020_04_DEN_NYJ     76      64              1
    ##   4:      4 4.000000 2020_04_DEN_NYJ     76      64              1
    ##   5:      5 4.000000 2020_04_DEN_NYJ     76      64              1
    ##  ---                                                              
    ## 656:     26 0.000000 2020_04_DEN_NYJ     76      85             22
    ## 657:     27 0.000000 2020_04_DEN_NYJ     76      85             22
    ## 658:     28 0.000000 2020_04_DEN_NYJ     76      85             22
    ## 659:     29 1.307736 2020_04_DEN_NYJ     76      85             22
    ## 660:     30 3.926750 2020_04_DEN_NYJ     76      85             22

<br /> The `ang_info_fun` function below is used to calculate the
offense’s and the quarterback’s area in specific regions (ie, blindside
and center). This function iterates through 360 angles of space around
the quarterback per frame, and consequently takes a long time to run and
produces data too large to be uploaded to GitHub, so we won’t provide an
example of its output here.

    ang_info_fun <- function(file_vec, gameId_vec, playId, frameId){
      # week_games_df <- game_conv_fun(file_vec, gameId_vec)
      week_games_df$event[week_games_df$event == "Pass Attempt"] <- "pass_forward"
      week_games_df$event[week_games_df$event == "Snap"] <- "ball_snap"
      
      return_list <- list()
      area_info_game_list <- list()
      angle_game_list <- list()
      k <- 1
      for (game_id in gameId_vec) {
        
        # print("game")
        # print(k)
        ind_game_df <- week_games_df[week_games_df$gameId==game_id,]
        ind_game_df <- merge(ind_game_df, game_play_df, by=c("gameId", "playId"))
        
        ball_df <- ind_game_df[ind_game_df$nflId==-1,]
        
        pass_forward_events <- (unique((ind_game_df[ind_game_df$play_type=="pass",])$playId))
        pass_forward_events <- pass_forward_events[!is.na(pass_forward_events)]
        
        
        pass_forward_info_df <- ind_game_df[ind_game_df$event=="pass_forward" & ind_game_df$playId %in% pass_forward_events,]
        pass_forward_events_df <- aggregate(frameId ~ playId, data = pass_forward_info_df , min)
        
        area_info_play_list <- list()
        angle_play_list <- list()
        j <- 1
        for (play_id in playId) {
          # print("play")
          # print(j)
          full_play_df <- ind_game_df[ind_game_df$playId==play_id,]
          line_set_frame <- min(unique(full_play_df[full_play_df$event=="Line Set",]$frameId))
          if (line_set_frame==Inf) {
            line_set_frame <- min(unique(na.omit(full_play_df[full_play_df$event=="ball_snap",]$frameId)))-1
            if (line_set_frame<1) {
              next
            }
          }
          initial_configuration_df <- full_play_df[full_play_df$frameId==line_set_frame & full_play_df$nflId==-1,]
          spec_frame_id <- pass_forward_events_df[pass_forward_events_df$playId==play_id,]$frameId
          
          ball_snap_frame <- min(unique(na.omit(full_play_df[full_play_df$event=="ball_snap",]$frameId)))
          pass_forward_frame <- min(unique(na.omit(full_play_df[full_play_df$event=="pass_forward",]$frameId)))
          
          if (ball_snap_frame==Inf | pass_forward_frame==Inf | line_set_frame==Inf) {
            next
          }
          
          ball_snap_configuration_df <- full_play_df[full_play_df$frameId==ball_snap_frame & full_play_df$nflId==-1,]
          
          time_of_pass_df <- full_play_df[full_play_df$frameId==spec_frame_id & full_play_df$nflId==-1,]
          
          qb_id_df <- full_play_df[full_play_df$nflId %in% qb_df$nflId & full_play_df$frameId==ball_snap_frame,]
          if (nrow(qb_id_df)>1) {
            qb_id_df$ball_dist <- sqrt(((ball_snap_configuration_df$x_coords-qb_id_df$x_coords)^2)+((ball_snap_configuration_df$y_coords-qb_id_df$y_coords)^2))
            qb_id_df <- qb_id_df[qb_id_df$ball_dist==min(qb_id_df$ball_dist),]
          }
          time_of_pass_qb_df <- full_play_df[full_play_df$frameId==spec_frame_id & full_play_df$nflId==qb_id_df$nflId,]
          
          qb_tot_df <- full_play_df[full_play_df$nflId %in% qb_id_df$nflId & full_play_df$frameId==spec_frame_id,]  #isolating the voronoi coordinates for the quarterback
          LOS_x <- initial_configuration_df$x_coords
          LOS_y <- initial_configuration_df$y_coords
          
          if (LOS_x>time_of_pass_qb_df$x_coords) {
            field_progress_indicator <- -1 ####make this value 1 or -1, calculate in play_id for loop. -1 means LOS_x>qb initial position, 1 means LOS_x<qb initial position
            
          } else {
            field_progress_indicator <- 1
          }
          
          norm_full_play_df <- full_play_df
          norm_full_play_df$x_coords <- (full_play_df$x_coords-LOS_x)*-1*field_progress_indicator
          norm_full_play_df$y_coords <- (full_play_df$y_coords-(160/6))*field_progress_indicator
          offensive_team <- unique(norm_full_play_df[norm_full_play_df$nflId %in% qb_df$nflId,]$team)
          
          time_of_pass_norm_df <- norm_full_play_df[norm_full_play_df$frameId==(spec_frame_id) & norm_full_play_df$nflId==-1,]
          time_before_pass_norm_df <- norm_full_play_df[norm_full_play_df$frameId==(spec_frame_id-1) & norm_full_play_df$nflId==-1,]
          
          pass_angle <- (atan((time_of_pass_norm_df$x_coords-time_before_pass_norm_df$x_coords)/(time_of_pass_norm_df$y_coords-time_before_pass_norm_df$y_coords)))*(180/pi)
          if (is.na(pass_angle)==TRUE) {
            pass_angle <- NA
          } else if (pass_angle<0) {
            pass_angle <- pass_angle+180
          }
          
          xbound1 <- -10
          ybound1 <- ((LOS_y-(160/6))*field_progress_indicator)+6
          ybound2 <- ((LOS_y-(160/6))*field_progress_indicator)-6
          
          play_frame_duration <- (pass_forward_frame-ball_snap_frame)+1
          
          norm_full_play_in_pocket_df <- norm_full_play_df[norm_full_play_df$nflId!=-1,] #omitting ball from dataframe
          norm_full_play_in_pocket_df <- norm_full_play_in_pocket_df[norm_full_play_in_pocket_df$frameId>=ball_snap_frame & norm_full_play_in_pocket_df$frameId<=pass_forward_frame,]
          norm_full_play_in_pocket_df <- norm_full_play_in_pocket_df[norm_full_play_in_pocket_df$y_coords<ybound1 & norm_full_play_in_pocket_df$y_coords>ybound2,]
          norm_full_play_in_pocket_df <- norm_full_play_in_pocket_df[rowSums(is.na(norm_full_play_in_pocket_df)) != ncol(norm_full_play_in_pocket_df), ]
          full_play_pocket_df <- norm_full_play_in_pocket_df[(norm_full_play_in_pocket_df$x_coords>xbound1 & norm_full_play_in_pocket_df$x_coords<0),]
          
          y_coord_adj <- (norm_full_play_df[norm_full_play_df$frameId==1 & norm_full_play_df$nflId==-1,]$y_coords)
          
          player_in_pocket_frame_count_df<- (aggregate(frameId ~ nflId, data = full_play_pocket_df , length))
          player_in_pocket_frame_count_df$freq <- player_in_pocket_frame_count_df$frameId/play_frame_duration
          adj_player_in_pocket_df <- player_in_pocket_frame_count_df[player_in_pocket_frame_count_df$freq>=0.5,]
          
          
          pocket_players_at_snap_df <- norm_full_play_df[norm_full_play_df$frameId==ball_snap_frame & norm_full_play_df$nflId %in% adj_player_in_pocket_df$nflId,]
          rel_pos_df <- merge(pocket_players_at_snap_df, pos_df, by="nflId")
          rel_pos_df <- data.frame(nflId=rel_pos_df$nflId, "PositionAbbr"=rel_pos_df$PositionAbbr)
          pocket_players_at_snap_df <- merge(rel_pos_df,pocket_players_at_snap_df, by="nflId", all=T)
          pocket_players_at_snap_df$PositionAbbr[is.na(pocket_players_at_snap_df$PositionAbbr)] <- "UN"
          oline_players_at_snap_df <- pocket_players_at_snap_df[pocket_players_at_snap_df$team==offensive_team & pocket_players_at_snap_df$PositionAbbr!="QB" & pocket_players_at_snap_df$PositionAbbr!="RB",]
          qb_at_snap_df <- pocket_players_at_snap_df[pocket_players_at_snap_df$PositionAbbr=="QB",]
          if (nrow(qb_at_snap_df)==0) {
            next
          }
          qb_in_pocket_df <- adj_player_in_pocket_df[adj_player_in_pocket_df$nflId==qb_at_snap_df$nflId[1],]
          if (qb_in_pocket_df$freq<1) {
            next
          }
          
          oline_players_at_snap_df <- oline_players_at_snap_df[order(oline_players_at_snap_df$y_coords),]
          if (nrow(oline_players_at_snap_df)>0) {
            oline_players_at_snap_df$ordering <- 1:nrow(oline_players_at_snap_df)
          } else {
            oline_players_at_snap_df$ordering <- numeric(nrow(oline_players_at_snap_df))
          }
          oline_ordering_df <- data.frame(nflId=oline_players_at_snap_df$nflId, ordering=oline_players_at_snap_df$ordering)
          
          voronoi_x_list <- list()
          voronoi_y_list <- list()
          voronoi_nflId_list <- list()
          voronoi_frameid_list <- list()
          
          voronoi_area_vec <- rep(NA, times=length(ball_snap_frame:max(full_play_df$frameId)))
          rect <- data.frame(x=c(xbound1, 0, 0, xbound1, xbound1), y=c(ybound1, ybound1, ybound2, ybound2, ybound1))
          
          oline_ordering_wQB_df <- oline_ordering_df
          oline_ordering_wQB_df[nrow(oline_ordering_wQB_df)+1,] <- c(qb_at_snap_df$nflId, 0)
          
          area_info_frame_list <- list()
          angle_frame_list <- list()
          
          i <- 1
          for (frame_id in frameId) {
            # print("frame")
            # print(i)
            
            spec_frame_df <- norm_full_play_df[norm_full_play_df$frameId==frame_id,]
            frame_omit_ball_df <- spec_frame_df[spec_frame_df$nflId!=-1,] #omitting ball from dataframe
            spec_frame_wo_ball_df <- frame_omit_ball_df[frame_omit_ball_df$y_coords<ybound1 & frame_omit_ball_df$y_coords>ybound2,]
            spec_frame_wo_ball_df <- spec_frame_wo_ball_df[rowSums(is.na(spec_frame_wo_ball_df)) != ncol(spec_frame_wo_ball_df), ]
            
            spec_players_df <- spec_frame_df[spec_frame_df$nflId %in% adj_player_in_pocket_df$nflId,]
            spec_players_df <- (merge(spec_players_df, rel_pos_df, by="nflId", all=T))
            spec_players_df$PositionAbbr[is.na(spec_players_df$PositionAbbr)] <- "UN"
            
            player_team_df <- data.frame(nflId=spec_players_df$nflId, team=spec_players_df$team)
            
            off_players_df <- spec_players_df[spec_players_df$team==offensive_team,]
            def_players_df <- frame_omit_ball_df[frame_omit_ball_df$team!=offensive_team,]
            oline_df <- off_players_df[off_players_df$PositionAbbr!="QB" & off_players_df$PositionAbbr!="RB",]
            oline_wQB_df <- off_players_df[off_players_df$PositionAbbr!="RB",]
            oline_df <- merge(oline_df, oline_ordering_df, by="nflId")
            oline_wQB_df <- merge(oline_wQB_df, oline_ordering_wQB_df, by="nflId")
            qb_frame_df <- off_players_df[off_players_df$PositionAbbr=="QB",]
            
            if (nrow(oline_df)>0) {
              oline_df$relevant_frame <- (oline_df$frameId-ball_snap_frame)+1
              oline_df$players_on_line <- nrow(oline_df)
              
              oline_wQB_df$relevant_frame <- (oline_wQB_df$frameId-ball_snap_frame)+1
              oline_wQB_df$players_on_line <- nrow(oline_df)
            } else {
              oline_df$relevant_frame = numeric(nrow(oline_df))
              oline_df$players_on_line = numeric(nrow(oline_df))
              
              oline_wQB_df$relevant_frame = numeric(nrow(oline_wQB_df))
              oline_wQB_df$players_on_line = numeric(nrow(oline_wQB_df))
            }
            
            pocket_df <- spec_frame_wo_ball_df[(spec_frame_wo_ball_df$x_coords>xbound1 & spec_frame_wo_ball_df$x_coords<0),]
            
            def_players_df$PositionAbbr <- NULL
            off_vor_calc_df <- spec_frame_wo_ball_df[spec_frame_wo_ball_df$nflId %in% oline_wQB_df$nflId,]
            vor_calc_df <- rbind(off_vor_calc_df, def_players_df)
            vor_calc_df$duplicated <- duplicated(vor_calc_df[,3:4])
            vor_calc_df <- vor_calc_df[vor_calc_df$duplicated==FALSE,]
            vor_calc_df$duplicated <- NULL
            vor_calc_df <- as.data.frame(vor_calc_df)
            
            vor_spdf <- voronoi_polygon(data=vor_calc_df,x="x_coords",y="y_coords", outline=rect)
            vor_df <- fortify_voronoi(vor_spdf)
            mp_sf <- st_as_sf(vor_spdf)
            p_sf <- st_cast(mp_sf, "POLYGON")
            p_qb_sf <- p_sf[p_sf$nflId==qb_frame_df$nflId,]
            if (nrow(p_qb_sf)==0) {
              next
            }
            p_qb_sf_line <- p_sf[p_sf$team==offensive_team,]
            area <- st_area(p_qb_sf)
            area_line <- sum(st_area(p_qb_sf_line))
            
            area_info_df <- data.frame(nflId=p_qb_sf$nflId, qb_tot_area=area, line_tot_area=area_line)
            
            
            rect_qbf = data.frame(x=c(qb_frame_df$x_coords, 0, 0, qb_frame_df$x_coords, qb_frame_df$x_coords), y=c(ybound1, ybound1, ybound2, ybound2, ybound1))
            
            
            vor_spdf_qbf <- voronoi_polygon(data=vor_calc_df ,x="x_coords",y="y_coords", outline=rect_qbf)
            vor_df_qbf <- fortify_voronoi(vor_spdf_qbf)
            mp_sf_qbf <- st_as_sf(vor_spdf_qbf)
            p_sf_qbf <- st_cast(mp_sf_qbf, "POLYGON")
            p_sf_qbf_pres <- p_sf_qbf
            p_sf_qbf <- p_sf_qbf[p_sf_qbf$nflId==qb_frame_df$nflId,]
            p_sf_qbf_line <- p_sf_qbf_pres[p_sf_qbf_pres$team==offensive_team,]
            area_qbf <- st_area(p_sf_qbf)
            area_qbf_line <- sum(st_area(p_sf_qbf_line))
            
            
            area_info_df$qb_front_area <- area_qbf
            area_info_df$line_front_area <- area_qbf_line 
            area_info_df$frameId <- frame_id
            area_info_df$playId <- play_id
            area_info_df$gameId <- game_id
            area_info_df$relevant_frame <- (area_info_df$frameId-ball_snap_frame)+1
            
            angle_info_list <- list()
            a <- 1
            for (angle_deg in c(1:360)) {
              angle <- angle_deg*(pi/180)
              m <- tan(angle)
              
              m_prev <- tan(angle-(pi/180))
              
              
              ybound1_rise <- ((ybound1-qb_frame_df$y_coords)*m)+qb_frame_df$x_coords
              ybound2_rise <- ((ybound2-qb_frame_df$y_coords)*m)+qb_frame_df$x_coords
              
              ybound1_rise_prev <- ((ybound1-qb_frame_df$y_coords)*m_prev)+qb_frame_df$x_coords
              ybound2_rise_prev <- ((ybound2-qb_frame_df$y_coords)*m_prev)+qb_frame_df$x_coords
              
              if (ybound1_rise>0) {
                x1_end <- 0
                y1_end <- ((-qb_frame_df$x_coords)/m)+qb_frame_df$y_coords
              } else if (ybound1_rise<(-10)) {
                x1_end <- -10
                y1_end <- ((-10-qb_frame_df$x_coords)/m)+qb_frame_df$y_coords
              } else {
                x1_end <- ybound1_rise
                y1_end <- ybound1
              }
              
              if (ybound1_rise_prev>0) {
                x1_end_prev <- 0
                y1_end_prev <- ((-qb_frame_df$x_coords)/m_prev)+qb_frame_df$y_coords
              } else if (ybound1_rise_prev<(-10)) {
                x1_end_prev <- -10
                y1_end_prev <- ((-10-qb_frame_df$x_coords)/m_prev)+qb_frame_df$y_coords
              } else {
                x1_end_prev <- ybound1_rise_prev
                y1_end_prev <- ybound1
              }
              
              if (ybound2_rise>0) {
                x2_end <- 0
                y2_end <- ((-qb_frame_df$x_coords)/m)+qb_frame_df$y_coords
              } else if (ybound2_rise<(-10)) {
                x2_end <- -10
                y2_end <- ((-10-qb_frame_df$x_coords)/m)+qb_frame_df$y_coords
              } else {
                x2_end <- ybound2_rise
                y2_end <- ybound2
              }
              
              if (ybound2_rise_prev>0) {
                x2_end_prev <- 0
                y2_end_prev <- ((-qb_frame_df$x_coords)/m_prev)+qb_frame_df$y_coords
              } else if (ybound2_rise_prev<(-10)) {
                x2_end_prev <- -10
                y2_end_prev <- ((-10-qb_frame_df$x_coords)/m_prev)+qb_frame_df$y_coords
              } else {
                x2_end_prev <- ybound2_rise_prev
                y2_end_prev <- ybound2
              }
              
              if (m>=0) {
                corner1_x <- -10
                corner1_y <- ybound2
                
                corner2_x <- 0
                corner2_y <- ybound2
              } else {
                corner1_x <- 0
                corner1_y <- ybound1
                
                corner2_x <- -10
                corner2_y <- ybound1
              }
              
              if (m_prev>=0) {
                corner1_x_prev <- -10
                corner1_y_prev <- ybound2
                
                corner2_x_prev <- 0
                corner2_y_prev <- ybound2
              } else {
                corner1_x_prev <- 0
                corner1_y_prev <- ybound1
                
                corner2_x_prev <- -10
                corner2_y_prev <- ybound1
              }
              
              
              if (m>=0 & ybound1_rise<0 & ybound2_rise>-10) {
                corner2_x <- 0
                corner2_y <- ybound1
                
                corner1_x <- 0
                corner1_y <- ybound2
              } 
              
              if (m>=0 & ybound1_rise_prev<0 & ybound2_rise_prev>-10) {
                corner2_x_prev <- 0
                corner2_y_prev <- ybound1
                
                corner1_x_prev <- 0
                corner1_y_prev <- ybound2
              } 
              
              
              if (angle_deg<=(90)) {
                rect_qbdirf <- data.frame(x=c(qb_frame_df$x_coords,x1_end,x1_end_prev,qb_frame_df$x_coords),
                                          y=c(qb_frame_df$y_coords,y1_end,y1_end_prev,qb_frame_df$y_coords))
              } else if (angle_deg==91) {
                rect_qbdirf <- data.frame(x=c(qb_frame_df$x_coords,x2_end,x1_end_prev,qb_frame_df$x_coords),
                                          y=c(qb_frame_df$y_coords,y2_end,y1_end_prev,qb_frame_df$y_coords))
              }else if (angle_deg <=(180) & angle_deg>90) {
                rect_qbdirf <- data.frame(x=c(qb_frame_df$x_coords,x2_end,x2_end_prev,qb_frame_df$x_coords),
                                          y=c(qb_frame_df$y_coords,y2_end,y2_end_prev,qb_frame_df$y_coords))
              } else if (angle_deg <=(270) & angle_deg>180) {
                rect_qbdirf <- data.frame(x=c(qb_frame_df$x_coords,x2_end_prev,x2_end,qb_frame_df$x_coords),
                                          y=c(qb_frame_df$y_coords,y2_end_prev,y2_end,qb_frame_df$y_coords))
              } else if (angle_deg == 271) {
                rect_qbdirf <- data.frame(x=c(qb_frame_df$x_coords,x2_end_prev,x1_end,qb_frame_df$x_coords),
                                          y=c(qb_frame_df$y_coords,y2_end_prev,y1_end,qb_frame_df$y_coords))
              } else if (angle_deg <=(360) & angle_deg>271) {
                rect_qbdirf <- data.frame(x=c(qb_frame_df$x_coords,x1_end_prev,x1_end,qb_frame_df$x_coords),
                                          y=c(qb_frame_df$y_coords,y1_end_prev,y1_end,qb_frame_df$y_coords))
              }
              
              
              vor_spdf_qbdirf <- voronoi_polygon(data=vor_calc_df ,x="x_coords",y="y_coords", outline=rect_qbdirf)
              vor_df_qbdirf <- fortify_voronoi(vor_spdf_qbdirf)
              mp_sf_qbdirf <- st_as_sf(vor_spdf_qbdirf)
              p_sf_qbdirf <- st_cast(mp_sf_qbdirf, "POLYGON")
              p_sf_qbdirf_pres <- p_sf_qbdirf
              p_sf_qbdirf <- p_sf_qbdirf[p_sf_qbdirf$nflId==qb_frame_df$nflId,]
              p_sf_qbdirf_line <- p_sf_qbdirf_pres[p_sf_qbdirf_pres$team==offensive_team,]
              area_qbdirf <- st_area(p_sf_qbdirf)
              area_qbdirf_line <- sum(st_area(p_sf_qbdirf_line))
              
              angle_area_info_df <- data.frame(gameId=game_id, playId=play_id, frameId=frame_id, angle=angle_deg, qb_front_angle_area=area_qbdirf, line_front_angle_area=area_qbdirf_line)
              angle_info_list[[a]] <- angle_area_info_df
              a <- a+1
              # print(a)
            }
            
            
            angle_info_frame <- rbindlist(angle_info_list)
            
            angle_frame_list[[i]] <- angle_info_frame
            area_info_frame_list[[i]] <- area_info_df
            
            i <- i+1 
          }
          angle_play_list[[j]] <- rbindlist(angle_frame_list)
          area_info_play_list[[j]] <- rbindlist(area_info_frame_list)
          j <- j+1
        }
        angle_game_list[[k]] <- rbindlist(angle_play_list)
        area_info_game_list[[k]] <- rbindlist(area_info_play_list)
        k <- k+1
      }
      
      return_list[[1]] <- rbindlist(angle_game_list)
      return_list[[2]] <- rbindlist(area_info_game_list)
      return(return_list)
    }

<br /> The `surv_fun` function below performs a survival analysis in the
form of a Kaplan-Meier estimator on the data gleaned above. The code
commented out in this function is the code that retrieves the survival
data/graphs for the blindside/center. This analysis is dependent on data
that takes too long to process for this example and likewise is too
large to upload to GitHub. The necessary data for this analysis can be
retrieved using the `angle_info_fun` function.

    surv_fun <- function(summary_info_df, angle_info_df, play_info_df,bin_info_df){
      total_plays <- nrow(aggregate(frameId~gameId+playId, summary_info_df, length))
      plot_list <- list()
      survive_df_list <- list()
      return_list <- list()
      
      # analysis_df <- merge(angle_info_df, summary_info_df)
      # analysis_df$nflId <- NULL
      # play_info_df$nflId <- NULL
      # det_analysis_df <- merge(play_info_df, analysis_df, by=c("gameId", "playId"))
      # det_analysis_df <- det_analysis_df[order(det_analysis_df$gameId,det_analysis_df$playId,det_analysis_df$relevant_frame,det_analysis_df$angle),]
      surv_analysis_df <- merge(summary_info_df, play_info_df, by=c("gameId", "playId"))
      surv_analysis_df <- surv_analysis_df[order(surv_analysis_df$gameId, surv_analysis_df$playId, surv_analysis_df$relevant_frame),]
      surv_analysis_atpass_df <- surv_analysis_df[surv_analysis_df$relevant_frame==surv_analysis_df$pass_forward_relevant_frame,]
      at_pass_info_df <- data.frame(gameId=surv_analysis_atpass_df$gameId, playId=surv_analysis_atpass_df$playId, 
                                    relevant_frame=surv_analysis_atpass_df$relevant_frame, pass_forward_relevant_frame=surv_analysis_atpass_df$pass_forward_relevant_frame)
      row.names(surv_analysis_atpass_df) <- NULL
      
      surv_analysis_df$line_area_survive <- 1
      surv_analysis_df$line_area_survive[surv_analysis_df$line_tot_area<65.214] <- 0 #This sets the failure threshold for total offensive line area. The other thresholds are below.
      
      surv_analysis_df$qb_area_survive <- 1
      surv_analysis_df$qb_area_survive[surv_analysis_df$qb_tot_area<15.4882] <- 0
      
      bin_info_df$bin_area_survive <- 1
      bin_info_df$bin_area_survive[bin_info_df$bin_area<1.51018] <- 0
      
      # line_blindside_analysis_df <- det_analysis_df[det_analysis_df$angle>150 & det_analysis_df$angle<300,]
      # line_blindside_analysis_agg<- aggregate(line_front_angle_area~gameId+playId+relevant_frame, line_blindside_analysis_df, sum)
      # line_blindside_analysis_agg$angle_area_survive <- 1
      # line_blindside_analysis_agg$angle_area_survive[line_blindside_analysis_agg$line_front_angle_area<14.52] <- 0
      # 
      # qb_blindside_analysis_agg<- aggregate(qb_front_angle_area~gameId+playId+relevant_frame, line_blindside_analysis_df, sum)
      # qb_blindside_analysis_agg$angle_area_survive <- 1
      # qb_blindside_analysis_agg$angle_area_survive[qb_blindside_analysis_agg$qb_front_angle_area<6.373] <- 0
      # 
      # line_center_analysis_df <- det_analysis_df[det_analysis_df$angle>60 & det_analysis_df$angle<120,]
      # line_center_analysis_agg<- aggregate(line_front_angle_area~gameId+playId+relevant_frame, line_center_analysis_df, sum)
      # line_center_analysis_agg$angle_area_survive <- 1
      # line_center_analysis_agg$angle_area_survive[line_center_analysis_agg$line_front_angle_area<11.14] <- 0
      # 
      # qb_center_analysis_agg<- aggregate(qb_front_angle_area~gameId+playId+relevant_frame, line_center_analysis_df, sum)
      # qb_center_analysis_agg$angle_area_survive <- 1
      # qb_center_analysis_agg$angle_area_survive[qb_center_analysis_agg$qb_front_angle_area<1.153] <- 0
      
      surv_game_list <- list()
      surv_game_list_unc <- list()
      surv_game_bin_list <- list()
      m <- 1
      for (game_id in unique(surv_analysis_df$gameId)) {
        ind_game_df <- surv_analysis_df[surv_analysis_df$gameId==game_id,]
        ind_game_bin_df <- bin_info_df[bin_info_df$gameId==game_id,]
        # ind_game_line_blindside_df <- line_blindside_analysis_agg[line_blindside_analysis_agg$gameId==game_id,]
        # ind_game_qb_blindside_df <- qb_blindside_analysis_agg[qb_blindside_analysis_agg$gameId==game_id,]
        # ind_game_line_center_df <- line_center_analysis_agg[line_center_analysis_agg$gameId==game_id,]
        # ind_game_qb_center_df <- qb_center_analysis_agg[qb_center_analysis_agg$gameId==game_id,]
        # 
        frame_obs <- 35
        
        surv_play_list <- list()
        surv_play_list_unc <- list()
        surv_bin_play_list <- list()
        j <- 1
        for (play_id in unique(ind_game_df$playId)) {
          ind_play_df <- ind_game_df[ind_game_df$playId==play_id,]
          ind_play_bin_df <- ind_game_bin_df[ind_game_bin_df$playId==play_id,]
          # ind_play_line_blindside_df <- ind_game_line_blindside_df[ind_game_line_blindside_df$playId==play_id,]
          # ind_play_qb_blindside_df <- ind_game_qb_blindside_df[ind_game_qb_blindside_df$playId==play_id,]
          # ind_play_line_center_df <- ind_game_line_center_df[ind_game_line_center_df$playId==play_id,]
          # ind_play_qb_center_df <- ind_game_qb_center_df[ind_game_qb_center_df$playId==play_id,]
          
          line_survive_play_agg <- aggregate(line_area_survive~relevant_frame, ind_play_df, mean)
          qb_survive_play_agg <- aggregate(qb_area_survive~relevant_frame, ind_play_df, mean)
          # line_blindside_survive_play_agg <- aggregate(angle_area_survive~relevant_frame, ind_play_line_blindside_df, mean)
          # qb_blindside_survive_play_agg <- aggregate(angle_area_survive~relevant_frame, ind_play_qb_blindside_df, mean)
          # line_center_survive_play_agg <- aggregate(angle_area_survive~relevant_frame, ind_play_line_center_df, mean)
          # qb_center_survive_play_agg <- aggregate(angle_area_survive~relevant_frame, ind_play_qb_center_df, mean)
          
          fail_frame <- min(line_survive_play_agg[line_survive_play_agg$line_area_survive==0,]$relevant_frame) 
          line_survive_play_agg$failure <- FALSE
          line_survive_play_agg$failure[line_survive_play_agg$relevant_frame==fail_frame] <- TRUE
          line_survive_play_agg$censored <- 1
          if (fail_frame==Inf) {
            line_survive_play_agg$failure[line_survive_play_agg$relevant_frame==max(line_survive_play_agg$relevant_frame)] <- TRUE
            line_survive_play_agg$censored[line_survive_play_agg$relevant_frame==max(line_survive_play_agg$relevant_frame)] <- 0
            fail_frame <- max(line_survive_play_agg$relevant_frame)+1
          }
          line_survive_play_agg$line_area_survive[line_survive_play_agg$relevant_frame>=fail_frame] <- 0
          
          qb_fail_frame <- min(qb_survive_play_agg[qb_survive_play_agg$qb_area_survive==0,]$relevant_frame) 
          qb_survive_play_agg$failure <- FALSE
          qb_survive_play_agg$failure[qb_survive_play_agg$relevant_frame==qb_fail_frame] <- TRUE
          qb_survive_play_agg$censored <- 1
          if (qb_fail_frame==Inf) {
            qb_survive_play_agg$failure[qb_survive_play_agg$relevant_frame==max(qb_survive_play_agg$relevant_frame)] <- TRUE
            qb_survive_play_agg$censored[qb_survive_play_agg$relevant_frame==max(qb_survive_play_agg$relevant_frame)] <- 0
            qb_fail_frame <- max(qb_survive_play_agg$relevant_frame)+1
          }
          qb_survive_play_agg$qb_area_survive[qb_survive_play_agg$relevant_frame>=qb_fail_frame] <- 0
          
          
          # blindside_fail_frame <- min(line_blindside_survive_play_agg[line_blindside_survive_play_agg$angle_area_survive==0,]$relevant_frame) 
          # line_blindside_survive_play_agg$failure <- FALSE
          # line_blindside_survive_play_agg$failure[line_blindside_survive_play_agg$relevant_frame==blindside_fail_frame] <- TRUE
          # line_blindside_survive_play_agg$censored <- 1
          # if (blindside_fail_frame==Inf) {
          #   line_blindside_survive_play_agg$failure[line_blindside_survive_play_agg$relevant_frame==max(line_blindside_survive_play_agg$relevant_frame)] <- TRUE
          #   line_blindside_survive_play_agg$censored[line_blindside_survive_play_agg$relevant_frame==max(line_blindside_survive_play_agg$relevant_frame)] <- 0
          #   blindside_fail_frame <- max(line_blindside_survive_play_agg$relevant_frame)+1
          # }
          # line_blindside_survive_play_agg$angle_area_survive[line_blindside_survive_play_agg$relevant_frame>=blindside_fail_frame] <- 0
          # 
          # qb_blindside_fail_frame <- min(qb_blindside_survive_play_agg[qb_blindside_survive_play_agg$angle_area_survive==0,]$relevant_frame) 
          # qb_blindside_survive_play_agg$failure <- FALSE
          # qb_blindside_survive_play_agg$failure[qb_blindside_survive_play_agg$relevant_frame==qb_blindside_fail_frame] <- TRUE
          # qb_blindside_survive_play_agg$censored <- 1
          # if (qb_blindside_fail_frame==Inf) {
          #   qb_blindside_survive_play_agg$failure[qb_blindside_survive_play_agg$relevant_frame==max(qb_blindside_survive_play_agg$relevant_frame)] <- TRUE
          #   qb_blindside_survive_play_agg$censored[qb_blindside_survive_play_agg$relevant_frame==max(qb_blindside_survive_play_agg$relevant_frame)] <- 0
          #   qb_blindside_fail_frame <- max(qb_blindside_survive_play_agg$relevant_frame)+1
          # }
          # qb_blindside_survive_play_agg$angle_area_survive[qb_blindside_survive_play_agg$relevant_frame>=qb_blindside_fail_frame] <- 0
          # 
          # center_fail_frame <- min(line_center_survive_play_agg[line_center_survive_play_agg$angle_area_survive==0,]$relevant_frame)
          # line_center_survive_play_agg$failure <- FALSE
          # line_center_survive_play_agg$failure[line_center_survive_play_agg$relevant_frame==center_fail_frame] <- TRUE
          # line_center_survive_play_agg$censored <- 1
          # if (center_fail_frame==Inf) {
          #   line_center_survive_play_agg$failure[line_center_survive_play_agg$relevant_frame==max(line_center_survive_play_agg$relevant_frame)] <- TRUE
          #   line_center_survive_play_agg$censored[line_center_survive_play_agg$relevant_frame==max(line_center_survive_play_agg$relevant_frame)] <- 0
          #   center_fail_frame <- max(line_center_survive_play_agg$relevant_frame)+1
          # }
          # line_center_survive_play_agg$angle_area_survive[line_center_survive_play_agg$relevant_frame>=center_fail_frame] <- 0
          # 
          # 
          # qb_center_fail_frame <- min(qb_center_survive_play_agg[qb_center_survive_play_agg$angle_area_survive==0,]$relevant_frame) 
          # qb_center_survive_play_agg$failure <- FALSE
          # qb_center_survive_play_agg$failure[qb_center_survive_play_agg$relevant_frame==qb_center_fail_frame] <- TRUE
          # qb_center_survive_play_agg$censored <- 1
          # if (qb_center_fail_frame==Inf) {
          #   qb_center_survive_play_agg$failure[qb_center_survive_play_agg$relevant_frame==max(qb_center_survive_play_agg$relevant_frame)] <- TRUE
          #   qb_center_survive_play_agg$censored[qb_center_survive_play_agg$relevant_frame==max(qb_center_survive_play_agg$relevant_frame)] <- 0
          #   qb_center_fail_frame <- max(qb_center_survive_play_agg$relevant_frame)+1
          # }
          # qb_center_survive_play_agg$angle_area_survive[qb_center_survive_play_agg$relevant_frame>=qb_center_fail_frame] <- 0
          # 
          
          names(line_survive_play_agg)[names(line_survive_play_agg) == 'line_area_survive'] <- 'Survive'
          names(qb_survive_play_agg)[names(qb_survive_play_agg) == 'qb_area_survive'] <- 'Survive'
          # names(line_blindside_survive_play_agg)[names(line_blindside_survive_play_agg) == 'angle_area_survive'] <- 'Survive'
          # names(qb_blindside_survive_play_agg)[names(qb_blindside_survive_play_agg) == 'angle_area_survive'] <- 'Survive'
          # names(line_center_survive_play_agg)[names(line_center_survive_play_agg) == 'angle_area_survive'] <- 'Survive'
          # names(qb_center_survive_play_agg)[names(qb_center_survive_play_agg) == 'angle_area_survive'] <- 'Survive'
          line_survive_play_agg$survive_id <- "line_total"
          qb_survive_play_agg$survive_id <- "qb_total"
          # line_blindside_survive_play_agg$survive_id <- "line_blindside"
          # qb_blindside_survive_play_agg$survive_id <- "qb_blindside"
          # line_center_survive_play_agg$survive_id <- "line_center"
          # qb_center_survive_play_agg$survive_id <- "qb_center"
          # 
          survive_play_agg <- bind_rows(qb_survive_play_agg, line_survive_play_agg
                                        # line_blindside_survive_play_agg, qb_blindside_survive_play_agg, line_center_survive_play_agg, qb_center_survive_play_agg
          )
          
          survive_play_agg_unc <- survive_play_agg
          survive_play_agg <- survive_play_agg[survive_play_agg$failure==TRUE,]
          
          surv_play_list[[j]] <- survive_play_agg
          surv_play_list_unc[[j]] <- survive_play_agg_unc
          
          surv_bin_list <- list()
          s <- 1
          for (bin_id in unique(ind_play_bin_df$bin_id)) {
            ind_bin_df <- ind_play_bin_df[ind_play_bin_df$bin_id==bin_id,]
            bin_survive_agg_df <- aggregate(bin_area_survive~relevant_frame, ind_bin_df, mean)
            bin_fail_frame <- min(bin_survive_agg_df[bin_survive_agg_df$bin_area_survive==0,]$relevant_frame)
            bin_survive_agg_df$failure <- FALSE
            bin_survive_agg_df$failure[bin_survive_agg_df$relevant_frame==bin_fail_frame] <- TRUE
            bin_survive_agg_df$censored <- 1
            if (bin_fail_frame==Inf) {
              bin_survive_agg_df$failure[bin_survive_agg_df$relevant_frame==max(bin_survive_agg_df$relevant_frame)] <- TRUE
              bin_survive_agg_df$censored[bin_survive_agg_df$relevant_frame==max(bin_survive_agg_df$relevant_frame)] <- 0
              bin_fail_frame <- max(bin_survive_agg_df$relevant_frame)+1
            }
            bin_survive_agg_df$bin_area_survive[bin_survive_agg_df$relevant_frame>=bin_fail_frame] <- 0
            
            bin_survive_agg_df$bin_id <- bin_id
            surv_bin_list[[s]] <- bin_survive_agg_df
            s <- s+1
            
            
          }
          
          surv_bin_play_list[[j]] <- rbindlist(surv_bin_list)
          j <- j+1
        }
        surv_game_list[[m]] <- rbindlist(surv_play_list)
        surv_game_list_unc[[m]] <- rbindlist(surv_play_list_unc)
        surv_game_bin_list[[m]] <- rbindlist(surv_bin_play_list)
        m <- m+1
      }
      
      surv_corr_df <- rbindlist(surv_game_list)
      bin_surv_corr_df <- rbindlist(surv_game_bin_list)
      surv_corr_unc_df <- rbindlist(surv_game_list_unc)
      
      
      survive_agg <- (na.omit(surv_corr_unc_df[surv_corr_unc_df$failure==TRUE,]))
      survive_agg<- survive_agg[survive_agg$relevant_frame<36,]
      survive_agg <- survive_agg[order(survive_agg$survive_id, survive_agg$relevant_frame),]
      # survive_agg <- survive_agg[survive_agg$survive_id=="line_total",]
      survive_length_agg <- aggregate(failure~survive_id+relevant_frame, survive_agg, length)
      survive_length_agg <- survive_length_agg[order(survive_length_agg$survive_id, survive_length_agg$relevant_frame),]
      names(survive_length_agg)[names(survive_length_agg) == "failure"] <- "failure_length"
      surv_agg <- merge(survive_length_agg, data.frame(relevant_frame=rep(c(1:35),length(unique(survive_agg$survive_id))),survive_id=sort(rep(c(unique(survive_agg$survive_id)),35))), by=c("relevant_frame","survive_id"), all=T)
      surv_agg <- surv_agg[order(surv_agg$survive_id, surv_agg$relevant_frame),]
      surv_agg$failure_length[is.na(surv_agg$failure_length)] <- 0
      id_length_df <- aggregate(failure_length~survive_id, surv_agg, sum)
      id_length_df$fl_adj <- cumsum(c(0, id_length_df$failure_length[-nrow(id_length_df)]))
      id_length_df$failure_length <- NULL
      surv_agg <- merge(surv_agg, id_length_df, by=c("survive_id"))
      surv_agg$survival_rate <- 1-((cumsum(surv_agg$failure_length)-surv_agg$fl_adj)/total_plays)
      
      surv_agg$hazard<- c(  c(1,((surv_agg[surv_agg$survive_id==sort(unique(surv_agg$survive_id))[1],]$survival_rate[-1]/surv_agg[surv_agg$survive_id==sort(unique(surv_agg$survive_id))[1],]$survival_rate))[-35]),
                            c(1,((surv_agg[surv_agg$survive_id==sort(unique(surv_agg$survive_id))[2],]$survival_rate[-1]/surv_agg[surv_agg$survive_id==sort(unique(surv_agg$survive_id))[2],]$survival_rate))[-35])
                            # c(1,((surv_agg[surv_agg$survive_id==sort(unique(surv_agg$survive_id))[3],]$survival_rate[-1]/surv_agg[surv_agg$survive_id==sort(unique(surv_agg$survive_id))[3],]$survival_rate))[-35]),
                            # c(1,((surv_agg[surv_agg$survive_id==sort(unique(surv_agg$survive_id))[4],]$survival_rate[-1]/surv_agg[surv_agg$survive_id==sort(unique(surv_agg$survive_id))[4],]$survival_rate))[-35]),
                            # c(1,((surv_agg[surv_agg$survive_id==sort(unique(surv_agg$survive_id))[5],]$survival_rate[-1]/surv_agg[surv_agg$survive_id==sort(unique(surv_agg$survive_id))[5],]$survival_rate))[-35]),
                            # c(1,((surv_agg[surv_agg$survive_id==sort(unique(surv_agg$survive_id))[6],]$survival_rate[-1]/surv_agg[surv_agg$survive_id==sort(unique(surv_agg$survive_id))[6],]$survival_rate))[-35])
      )
      
      surv_agg <- surv_agg[order(surv_agg$survive_id, surv_agg$relevant_frame),]
      surv_agg$survive_id[surv_agg$survive_id=="line_total"] <- "Offensive Voronoi Space"
      surv_agg$survive_id[surv_agg$survive_id=="qb_total"] <- "QB Voronoi Space"
      # surv_agg$survive_id[surv_agg$survive_id=="line_blindside"] <- "Offensive Voronoi Space, Blindside"
      # surv_agg$survive_id[surv_agg$survive_id=="qb_blindside"] <- "QB Voronoi Space, Blindside"
      # surv_agg$survive_id[surv_agg$survive_id=="line_center"] <- "Offensive Voronoi Space, Center"
      # surv_agg$survive_id[surv_agg$survive_id=="qb_center"] <- "QB Voronoi Space, Center"
      
      survive_df_list[[1]] <- surv_agg
      
      stand_features_df <- surv_agg[surv_agg$survive_id=="Offensive Voronoi Space" | surv_agg$survive_id=="QB Voronoi Space",]
      surv_p_stand <- ggplot(stand_features_df, aes(x=relevant_frame, y=survival_rate, group=survive_id, color=survive_id))+geom_smooth(method='loess', span=0.4, se=FALSE, size=1.5,size=4)+
        geom_vline(xintercept=22, size=1, linetype="dashed")+geom_vline(xintercept=35, size=1, linetype="dashed", color="red")+
        annotate("text", x=18.6, y=0, label= "Median Time to Throw", size=3)+
        annotate("text", x=33, y=0, label= "Censor Time", size=3)+xlab("Frames After Snap")+
        ylab("Survival Probability")+ labs(color='Survival ID')+ylim(0,1)+ theme_grey(base_size = 9)
      stand_features_haz_df <- na.omit(surv_agg[surv_agg$survive_id=="Offensive Voronoi Space" | surv_agg$survive_id=="QB Voronoi Space",])
      haz_p_stand <- ggplot(stand_features_haz_df, aes(x=relevant_frame, y=hazard, group=survive_id, color=survive_id))+geom_smooth(method='loess', span=0.4, se=FALSE, size=1.5,size=4)+
        # xlim(0,30)+
        geom_vline(xintercept=22, size=1, linetype="dashed")+geom_vline(xintercept=35, size=1, linetype="dashed", color="red")+
        annotate("text", x=18.6, y=0.8, label= "Median Time to Throw", size=3)+
        annotate("text", x=33, y=0.8, label= "Censor Time", size=3)+xlab("Frames After Snap")+
        ylab("Hazard Probability")+ labs(color='Survival ID')+ylim(0.8,1)+ theme_grey(base_size = 9)
      
      stand_figure <- ggarrange(surv_p_stand, haz_p_stand, common.legend = TRUE, legend = "bottom")
      
      stand_figure <-annotate_figure(stand_figure,
                                     bottom = text_grob("*Note difference in scales of y-axis", color = "black",
                                                        hjust = 1.1, x = 1, face = "italic", size = 5)
      )
      
      plot_list[[1]] <- stand_figure
      
      ###this is the code to produce and compare the survival/hazard plots of the blindside and center voronoi space
      # spec_features_df <- surv_agg[surv_agg$survive_id!="Offensive Voronoi Space" & surv_agg$survive_id!="QB Voronoi Space",]
      # surv_p_spec <- ggplot(spec_features_df, aes(x=relevant_frame, y=survival_rate, group=survive_id, color=survive_id))+geom_smooth(method='loess', span=0.4, se=FALSE, size=1.5,size=4)+
      #   # xlim(0,30)+
      #   geom_vline(xintercept=22, size=1, linetype="dashed")+geom_vline(xintercept=35, size=1, linetype="dashed", color="red")+
      #   annotate("text", x=18.6, y=0.9, label= "Median Time to Throw", size=3)+
      #   annotate("text", x=33, y=0.9, label= "Censor Time", size=3)+xlab("Frames After Snap")+
      #   ylab("Survival Probability")+ labs(color='Survival ID')+ylim(0,1)+theme_grey(base_size = 9)
      # # 
      # #   spec_fatures_haz_df <- na.omit(spec_features_df)
      # haz_p_spec <- ggplot(spec_features_df, aes(x=relevant_frame, y=hazard, group=survive_id, color=survive_id))+geom_smooth(method='loess', span=0.4, se=FALSE, size=1.5,size=4)+
      #   geom_vline(xintercept=22, size=1, linetype="dashed")+geom_vline(xintercept=35, size=1, linetype="dashed", color="red")+
      #   annotate("text", x=18.6, y=0.6, label= "Median Time to Throw", size=3)+
      #   annotate("text", x=33, y=0.6, label= "Censor Time", size=3)+xlab("Frames After Snap")+
      #   ylab("Hazard Probability")+ labs(color='Survival ID')+ylim(0.6,1)+theme_grey(base_size = 9)
      # 
      # spec_figure <- ggarrange(surv_p_spec, haz_p_spec, common.legend = TRUE, legend = "bottom")
      # 
      # spec_figure <- annotate_figure(spec_figure,
      #                                bottom = text_grob("*Note difference in scales of y-axis", color = "black",
      #                                                   hjust = 1.1, x = 1, face = "italic", size = 5)
      # )
      # plot_list[[2]] <- spec_figure
      # lg_rank_df <- na.omit(surv_corr_df)
      # row.names(lg_rank_df) <- NULL
      # lg_rank_df <- lg_rank_df[lg_rank_df$survive_i=="line_blindside"| lg_rank_df$survive_id=="line_center",]
      # attach(lg_rank_df)
      # lg_rank <- survdiff(Surv(relevant_frame, censored)~survive_id)
      # detach(lg_rank_df)
      
      bin_surv_corr_df <- rbindlist(surv_game_bin_list)
      bin_survive_agg <- (na.omit(bin_surv_corr_df[bin_surv_corr_df$failure==TRUE,]))
      bin_survive_agg<- bin_survive_agg[bin_survive_agg$relevant_frame<36,]
      bin_survive_agg <- bin_survive_agg[order(bin_survive_agg$bin_id, bin_survive_agg$relevant_frame),]
      bin_survive_length_agg <- aggregate(failure~bin_id+relevant_frame, bin_survive_agg, length)
      bin_survive_length_agg <- bin_survive_length_agg[order(bin_survive_length_agg$bin_id, bin_survive_length_agg$relevant_frame),]
      names(bin_survive_length_agg)[names(bin_survive_length_agg) == "failure"] <- "failure_length"
      
      bin_surv_agg <- merge(bin_survive_length_agg, data.frame(relevant_frame=rep(c(1:35),length(unique(bin_survive_agg$bin_id))),bin_id=sort(rep(c(unique(bin_survive_agg$bin_id)),35))), by=c("relevant_frame","bin_id"), all=T)
      bin_surv_agg <- bin_surv_agg[order(bin_surv_agg$bin_id, bin_surv_agg$relevant_frame),]
      bin_surv_agg$failure_length[is.na(bin_surv_agg$failure_length)] <- 0
      bin_id_length_df <- aggregate(failure_length~bin_id, bin_surv_agg, sum)
      bin_id_length_df$fl_adj <- cumsum(c(0, bin_id_length_df$failure_length[-nrow(bin_id_length_df)]))
      bin_id_length_df$failure_length <- NULL
      bin_surv_agg <- merge(bin_surv_agg, bin_id_length_df, by=c("bin_id"))
      bin_surv_agg$survival_rate <- 1-((cumsum(bin_surv_agg$failure_length)-bin_surv_agg$fl_adj)/total_plays)
      
      survive_df_list[[2]] <- bin_surv_agg
      
      ###Bin Survival/Hazard Plot Production and Comparison
      n <- 1
      bin_plot_list <- list()
      for (n in c(1:5)) {
        grp_bin_survive_agg <- bin_surv_agg[bin_surv_agg$bin_id==n| bin_surv_agg$bin_id==n+5| bin_surv_agg$bin_id==n+10| bin_surv_agg$bin_id==n+15| bin_surv_agg$bin_id==n+20| bin_surv_agg$bin_id==n+25,]
        bin_p <- ggplot(grp_bin_survive_agg, aes(x=relevant_frame, y=survival_rate, group=factor(bin_id), color=factor(bin_id)))+
          geom_smooth(method='loess', span=0.4, se=FALSE, size=1.5,size=4)+
          geom_vline(xintercept=22, size=1, linetype="dashed")+
          geom_vline(xintercept=35, size=1, linetype="dashed", color="red")+
          xlab("Frames After Snap")+
          ylab("Survival Probability")+ labs(color='bin_id')+ylim(0,1)+xlab("")+ylab("")
        bin_plot_list[[n]] <- bin_p
        n <- n+1
      }
      bin_figure <- ggarrange(bin_plot_list[[1]],bin_plot_list[[2]],bin_plot_list[[3]],bin_plot_list[[4]],bin_plot_list[[5]])
      bin_figure <-annotate_figure(bin_figure,
                                   bottom = text_grob("Frames After Snap", color = "black", size = 15),
                                   left = text_grob("Survival Probability", color = "black", size = 15,rot = 90)
      )
      plot_list[[2]] <- bin_figure
      
      n <- 4
      bin_lg_rank_df <- na.omit(bin_surv_corr_df)
      bin_lg_rank_df <- bin_lg_rank_df[bin_lg_rank_df$failure==TRUE,]
      row.names(bin_lg_rank_df) <- NULL
      bin_lg_rank_df <-bin_lg_rank_df[bin_lg_rank_df$bin_id==n| bin_lg_rank_df$bin_id==n+5| bin_lg_rank_df$bin_id==n+10| bin_lg_rank_df$bin_id==n+15| bin_lg_rank_df$bin_id==n+20| bin_lg_rank_df$bin_id==n+25,]
      bin_lg_rank <- survdiff(Surv(bin_lg_rank_df$relevant_frame, bin_lg_rank_df$censored)~bin_lg_rank_df$bin_id)
      
      return_list[[1]] <- plot_list
      return_list[[2]] <- survive_df_list
      
      return(return_list)
    }

<br /> The `surv_fun` returns a two-dimensional list where the first
inputted dimension of the list dictates whether to return a plot or a
data frame (1 returning a plot, 2 returning a data frame). The second
inputted dimension of the list dictates what perspective of the pocket
you wish to perform the analysis on, 1 being Voronoi features (offensive
line area, QB area, blindside, center) and 2 being bin features. For
plots, 1 returns only the survival curves for the offensive line area
and the QB area, 2 returns the survival curves for the bins, and 3 (when
uncommented) returns the survival curves for the blindside and the
center. Note that the displayed plots are not the same ones produced in
the paper, as the displayed plots are produced on a one game sample.

    print(surv_fun(summary_analysis_df,det_area_analysis_df,play_info_df,compiled_bin_info_df)[[1]][[1]])

![](readme_files/figure-markdown_strict/surv_plot-1.png)

    print(surv_fun(summary_analysis_df,det_area_analysis_df,play_info_df,compiled_bin_info_df)[[1]][[2]])

![](readme_files/figure-markdown_strict/surv_plot-2.png)
