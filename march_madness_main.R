
# general visualisation
library('ggplot2') # visualisation
library('scales') # visualisation
library('patchwork') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation
library('ggthemes') # visualisation
library('ggrepel') # visualisation

# general data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('vroom') # input/output
library('skimr') # overview
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation

# specific visualisation
library('alluvial') # visualisation
library('ggrepel') # visualisation
library('ggforce') # visualisation
library('ggridges') # visualisation
library('gganimate') # animations
library('GGally') # visualisation
library('ggExtra') # visualisation
library('viridis') # visualisation
library(gridExtra)
library('usmap') # geo
library("PerformanceAnalytics")

# specific data manipulation
library('lazyeval') # data wrangling
library('broom') # data wrangling
library('purrr') # data wrangling
library('reshape2') # data wrangling
library('rlang') # encoding
library('kableExtra') # display

# networks
library(igraph)
library(GGally)
library(ergm)
library(intergraph)

setwd("C:/Kaggle/Data/March_Madness_Analytics_2020")



################ 1. DATA LOADING

### 1.1. General dataframes

# Load Players data
df_Players <- read.csv('MPlayByPlay_Stage2/MPlayers.csv')

# Load Team data
df_Teams <- read.csv('MDataFiles_Stage2/MTeams.csv')
df_TeamCoaches <- read.csv('MDataFiles_Stage2/MTeamCoaches.csv')
df_TeamConferences <- read.csv('MDataFiles_Stage2/MTeamConferences.csv')
df_TeamSpellings <- read.csv('MDataFiles_Stage2/MTeamSpellings.csv')

# Load Cities data
df_Cities <- read.csv('MDataFiles_Stage2/Cities.csv')
df_GameCities <- read.csv('MDataFiles_Stage2/MGameCities.csv')

# Public Rankings
df_MasseyOrdinals <- read.csv('MDataFiles_Stage2/MMasseyOrdinals.csv')

# Conferences
df_Conferences <- read.csv('MDataFiles_Stage2/Conferences.csv')
df_ConferencesTourneyGames <- read.csv('MDataFiles_Stage2/MConferenceTourneyGames.csv')

# Tourney results
df_TourneyCompactResults <- read.csv('MDataFiles_Stage2/MNCAATourneyCompactResults.csv')
df_TourneyDetailedResults <- read.csv('MDataFiles_Stage2/MNCAATourneyDetailedResults.csv')
df_TourneySeedRoundSlots <- read.csv('MDataFiles_Stage2/MNCAATourneySeedRoundSlots.csv')
df_TourneySeeds <- read.csv('MDataFiles_Stage2/MNCAATourneySeeds.csv')
df_TourneySlots <- read.csv('MDataFiles_Stage2/MNCAATourneySlots.csv')

# Secondary results
df_SecondaryTourneyCompactResults <- read.csv('MDataFiles_Stage2/MSecondaryTourneyCompactResults.csv')
df_SecondaryTourneyTeams <- read.csv('MDataFiles_Stage2/MSecondaryTourneyTeams.csv')

# Regular Season results
df_RegularSeasonCompactResults <- read.csv('MDataFiles_Stage2/MRegularSeasonCompactResults.csv')
df_RegularSeasonDetailedResults <- read.csv('MDataFiles_Stage2/MRegularSeasonDetailedResults.csv')

# Seasons
df_Seasons <- read.csv('MDataFiles_Stage2/MSeasons.csv')


### 1.2. Play-by-Play Data

play_by_play <- data.frame()

# loop through each seasons PlayByPlay folders and read in in the play by play files
setwd("C:/Kaggle/Data/March_Madness_Analytics_2020/MPlayByPlay_Stage2")

#for(each in list.files()[str_detect(list.files(), "MEvents")]) {
#  
#  df <- read_csv(paste0(each))
#  
#  # Grouped shooting variables ----------------------------------------------
#  # there are some shooting variables that can probably be condensed - tip ins and dunks
##  paint_attempts_made <- c("made2_dunk", "made2_lay", "made2_tip") 
#  paint_attempts_missed <- c("miss2_dunk", "miss2_lay", "miss2_tip") 
#  paint_attempts <- c(paint_attempts_made, paint_attempts_missed)
#  # create variables for field goals made, and also field goals attempted (which includes the sum of FGs made and FGs missed)
#  FGM <- c("made2_dunk", "made2_jump", "made2_lay",  "made2_tip",  "made3_jump")
#  FGA <- c(FGM, "miss2_dunk", "miss2_jump" ,"miss2_lay",  "miss2_tip",  "miss3_jump")
#  # variable for three-pointers
#  ThreePointer <- c("made3_jump", "miss3_jump")
#  #  Two point jumper
#  TwoPointJump <- c("miss2_jump", "made2_jump")
#  # Free Throws
#  FT <- c("miss1_free", "made1_free")
#  # all shots
#  AllShots <- c(FGA, FT)
#  
#  
#  # Feature Engineering -----------------------------------------------------
#  # paste the two even variables together for FGs as this is the format for last years comp data
#  df <- df %>%
#    mutate_if(is.factor, as.character) %>% 
#    mutate(EventType = ifelse(str_detect(EventType, "miss") | str_detect(EventType, "made") | str_detect(EventType, "reb"), paste0(EventType, "_", EventSubType), EventType))
#  
#  # change the unknown for 3s to "jump" and for FTs "free"
#  df <- df %>% 
#    mutate(EventType = ifelse(str_detect(EventType, "3"), str_replace(EventType, "_unk", "_jump"), EventType),
#           EventType = ifelse(str_detect(EventType, "1"), str_replace(EventType, "_unk", "_free"), EventType))
#  
#  
#  df <- df %>% 
#    # create a variable in the df for whether the attempts was made or missed
#    mutate(shot_outcome = ifelse(grepl("made", EventType), "Made", ifelse(grepl("miss", EventType), "Missed", NA))) %>%
#    # identify if the action was a field goal, then group it into the attempt types set earlier
#    mutate(FGVariable = ifelse(EventType %in% FGA, "Yes", "No"),
#           AttemptType = ifelse(EventType %in% paint_attempts, "PaintPoints", 
#                                ifelse(EventType %in% ThreePointer, "ThreePointJumper", 
#                                       ifelse(EventType %in% TwoPointJump, "TwoPointJumper", 
#                                              ifelse(EventType %in% FT, "FreeThrow", "NoAttempt")))))
#  
#  
#  # Rework DF so only shots are included and whatever lead to the shot --------
#  df <- df %>% 
#    mutate(GameID = paste(Season, DayNum, WTeamID, LTeamID, sep = "_")) %>% 
#    group_by(GameID, ElapsedSeconds) %>% 
#    mutate(EventType2 = lead(EventType),
#           EventPlayerID2 = lead(EventPlayerID)) %>% ungroup()
#  
#  
#  df <- df %>% 
#    mutate(FGVariableAny = ifelse(EventType %in% FGA | EventType2 %in% FGA, "Yes", "No")) %>% 
#    filter(FGVariableAny == "Yes") 
#  
#  
#  # create a variable for if the shot was made, but then the second event was also a made shot
#  df <- df %>% 
#    mutate(Alert = ifelse(EventType %in% FGM & EventType2 %in% FGM, "Alert", "OK")) %>% 
#    # only keep "OK" observations
#    filter(Alert == "OK")
#  
#
#  # replace NAs with somerhing
#  df$EventType2[is.na(df$EventType2)] <- "no_second_event"
#  
#  
#  # create a variable for if there was an assist on the FGM:
#  df <- df %>% 
#    mutate(AssistedFGM = ifelse(EventType %in% FGM & EventType2 == "assist", "Assisted", 
#                                ifelse(EventType %in% FGM & EventType2 != "assist", "Solo", 
#                                       ifelse(EventType %in% FGM & EventType2 == "no_second_event", "Solo", "None"))))
#  
#  # # because the FGA culd be either in `EventType` (more likely) or `EventType2` (less likely), need
#  # # one variable to indicate the shot type
#  # df <- df %>% \
#  #   mutate(fg_type = ifelse(EventType %in% FGA, EventType, ifelse(EventType2 %in% FGA, EventType2, "Unknown")))
#  
#  # create final output
#  df <- df %>% ungroup()
#  play_by_play <- bind_rows(play_by_play, df)
#  print(each)
#  
#  rm(df); gc()
#}
#
#saveRDS(play_by_play, "play_by_play2015_19.rds")


# Load preprocessed final play-by-play data
#setwd("C:/Users/Artem/Google Drive/KAGGLE/March_Madness_Analytics_2020/MPlayByPlay_Stage2")
play_by_play <- readRDS("play_by_play2015_19.rds")

# Select Season
play_by_play <- play_by_play[play_by_play$Season == 2019,]



################ 2. DATA PREPROCESSING

### 2.1. Percentage of shots made solo in the team
# teams_solo_shots <- play_by_play %>% 
#   filter(AssistedFGM != "None") %>% 
#   group_by(EventTeamID, AssistedFGM) %>% 
#   summarise(n = n()) %>% 
#   mutate(solo_shot_perc = n / sum(n)) %>% 
#   filter(AssistedFGM == "Solo") %>% arrange(desc(solo_shot_perc)) %>% ungroup()





################################################################################################
################ NETWORK №1. NETWORK OF ASSISTS BETWEEN PLAYERS
################################################################################################


################ 1. CREATE NETWORKS OF ASSISTS

# Select only plays with assists
play_by_play <- subset(play_by_play, EventType2 == "assist")

# Create list with Team IDs
team_ids <- unique(play_by_play$EventTeamID)

# Remove missing team ids
if(0 %in% team_ids){
  team_ids <- team_ids[-which(team_ids == 0)]
}


createAssistsGraph <- function(team_id, play_by_play=play_by_play){

  # Select only one team
  play_by_play <- play_by_play[play_by_play$EventTeamID == team_id,]
  
  # Create edgeslist for one team, First column - assisted, Second column -  made a shot.
  play_by_play$EventPlayerID2 <- as.character(play_by_play$EventPlayerID2)
  play_by_play$EventPlayerID <- as.character(play_by_play$EventPlayerID)
  edgelist <- play_by_play[,c("EventPlayerID2", "EventPlayerID")]
  
  ### Remove duplicated edges and add weights instead
  # Create pair from each nodes in the edge
  edgelist$pair <- paste(edgelist$EventPlayerID2, edgelist$EventPlayerID, sep = "_")
  # Create dataframe with pairs and corresponding weights
  pair_weights <- data.frame(table(edgelist$pair))
  colnames(pair_weights) <- c("pair", 'weight')
  pair_weights$pair <- as.character(pair_weights$pair)
  # Remove duplicated edges
  edgelist <- edgelist[!duplicated(edgelist$pair),]
  # Add weights to the edgelist
  edgelist <- edgelist %>% left_join(pair_weights, 'pair')
  edgelist$pair <- NULL
  
  # Remove missing player ids
  edgelist <- edgelist[!is.na(edgelist$EventPlayerID2),]
  edgelist <- edgelist[!is.na(edgelist$EventPlayerID),]
  
  # Remove 0 player ids
  edgelist <- edgelist[edgelist$EventPlayerID2 != 0,]
  edgelist <- edgelist[edgelist$EventPlayerID != 0,]
  
  # Create graph from edgelist
  G <- igraph::graph_from_edgelist(as.matrix(edgelist[c("EventPlayerID2", "EventPlayerID")]))
  # Add weights
  E(G)$weight <- edgelist$weight
  
  # Remove self loops
  G <- igraph::simplify(G)
  
  return(G)
}


# Create assists graphs for all teams
assist_graphs_list <- list()

for(i in 1:length(team_ids)){
  assist_graphs_list[[i]] <- createAssistsGraph(team_ids[i], play_by_play)
}


################ 2. CALCULATE SET OF NETWORK STATISTICS FOR ALL TEAMS


calculateTeamNetworkStatistics <- function(g){
  
  # 1. Number of nodes
  nodes_n=length(V(g))
  
  # 2. Clustering coefficent (transitivity)
  mean_transitivity <- mean(transitivity(g, "weighted"), na.rm=T)
  
  # 3. Out-degree
  out_degree_vec <- as.numeric(strength(g, mode="out"))
  out_degree_vec <- out_degree_vec/sum(out_degree_vec)
  
  # 4. In-degree
  in_degree_vec <- as.numeric(strength(g, mode="in"))
  in_degree_vec <- in_degree_vec/sum(in_degree_vec)
  
  # 5. Betweenness
  betweenness_vec <- betweenness(g, normalized=T)
  
  # 6. Closeness
  closeness_vec <- closeness(g, normalized=T, mode="in")
  
  # 7. Page Rank
  pagerank_vec <- page.rank(g)$vector
  pagerank_vec <- pagerank_vec/sum(pagerank_vec)
  
  # 8. EigenVector centrality
  eigenvector_vec <- as.numeric(eigen_centrality(g, directed=T)$vector)
  
  # 9. Centralization metrics
  betweenness_centralization <- centralize(betweenness_vec, normalized = F)
  closeness_centralization <- centralize(closeness_vec, normalized = F)
  indegree_centralization <- centralize(in_degree_vec, normalized = F)
  outdegree_centralization <- centralize(out_degree_vec, normalized = F)
  pagerank_centralization <- centralize(pagerank_vec, normalized = F)
  eigenvector_centralization <- centralize(eigenvector_vec, normalized = F)
  
  
  return(list(nodes_n=nodes_n,
              mean_transitivity=mean_transitivity,
              betweenness_centralization=betweenness_centralization,
              closeness_centralization=closeness_centralization,
              indegree_centralization=indegree_centralization,
              outdegree_centralization=outdegree_centralization,
              pagerank_centralization=pagerank_centralization,
              eigenvector_centralization=eigenvector_centralization))
}


df_TeamsNetworkStats <- lapply(assist_graphs_list, calculateTeamNetworkStatistics)
df_TeamsNetworkStats <- do.call(rbind.data.frame, df_TeamsNetworkStats)

# Add team_id
df_TeamsNetworkStats$TeamID <- team_ids

# Add mean rank for the team in that season
df_MasseyOrdinals_pom <- subset(df_MasseyOrdinals, SystemName=="POM" & Season==2019) %>%
  select(TeamID, RankingDayNum, OrdinalRank) %>% group_by(TeamID) %>%
  summarize(median_rank=median(OrdinalRank)) %>% data.frame()

# Add ranking to the teams stats
df_TeamsNetworkStats <- left_join(df_TeamsNetworkStats, df_MasseyOrdinals_pom, by="TeamID")


### Plot Correlation matrix


# Elements of the plot
hist.panel = function (x, ...) {
  par(new = TRUE)
  hist(x,
       col = "light gray",
       probability = T,
       axes = FALSE,
       main = "",
       breaks = "FD")
}

panel.cor <- function(x, y, digits=2, prefix="", use="pairwise.complete.obs",
                      method = 'spearman', cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use=use, method=method) # MG: remove abs here
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 1/strwidth(txt)
  
  test <- cor.test(x,y, method=method)
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))
  # MG: add abs here and also include a 30% buffer for small numbers
  text(0.5, 0.5, txt, cex = cex)
  text(.8, .8, Signif, cex=cex, col=2)
}


# Plotting cor matrix
pairs(select(df_TeamsNetworkStats, -c(TeamID)),
      gap=0, lower.panel=panel.smooth,
      upper.panel=panel.cor, diag.panel=hist.panel,
      cex.labels = 1, font.labels = 2)


### 6.1. Simple regression, median_rank as DV, network stats as IVs
summary(lm(median_rank ~ mean_transitivity + betweenness_centralization + 
             closeness_centralization + pagerank_centralization,
       df_TeamsNetworkStats))


### 6.2. Plot the network examples

# 1. Example of the network with Biggest PageRank centralization
G <- assist_graphs_list[[which.max(df_TeamsNetworkStats$pagerank_centralization)]]
vertex_size <- page.rank(G)$vector * 200
plot.igraph(G, edge.arrow.size=0.1, layout=layout.kamada.kawai, edge.width=E(G)$weight*0.3,
            vertex.size=vertex_size)

# 2. Example of the network with Smallest PageRank centralization
G <- assist_graphs_list[[which.min(df_TeamsNetworkStats$pagerank_centralization)]]
vertex_size <- page.rank(G)$vector * 200
plot.igraph(G, edge.arrow.size=0.1, layout=layout.kamada.kawai, edge.width=E(G)$weight*0.3,
            vertex.size=vertex_size)

# 3. Example of the network with Biggest Betweenness centralization
G <- assist_graphs_list[[which.max(df_TeamsNetworkStats$betweenness_centralization)]]
vertex_size <- as.numeric(betweenness(G, normalized=T))*50
plot.igraph(G, edge.arrow.size=0.25, layout=layout.kamada.kawai, edge.width=E(G)$weight*0.3,
            vertex.size=vertex_size)

# 4. Example of the network with Smallest Betweenness centralization
G <- assist_graphs_list[[which.min(df_TeamsNetworkStats$betweenness_centralization)]]
vertex_size <- as.numeric(betweenness(G, normalized=T))*200
plot.igraph(G, edge.arrow.size=0.25, layout=layout.kamada.kawai, edge.width=E(G)$weight*0.3,
            vertex.size=vertex_size)


################ 6. CALCULATE SET OF NETWORK STATISTICS FOR ALL PLAYERS


calculatePlayerNetworkStatistics <- function(g){
  
  # 1. Players ids
  player_ids <- names(V(g))
  
  # 2. Betweenness
  betweenness_vec <- betweenness(g, normalized=T)
  
  # 3. Page Rank
  pagerank_vec <- page.rank(g)$vector
  pagerank_vec <- pagerank_vec/sum(pagerank_vec)
  
  
  return(list(PlayerID=player_ids,
              betweenness=betweenness_vec,
              pagerank=pagerank_vec))
}


df_PlayersNetworkStats <- lapply(assist_graphs_list, calculatePlayerNetworkStatistics)
df_PlayersNetworkStats <- do.call(rbind.data.frame, df_PlayersNetworkStats)
df_PlayersNetworkStats <- df_PlayersNetworkStats %>% mutate(PlayerID=as.character(PlayerID))

# Add full names of players to df with network stats
df_Players <- df_Players %>% mutate(FullName=paste(FirstName, LastName),
                                    PlayerID=as.character(PlayerID))
df_PlayersNetworkStats <- df_PlayersNetworkStats %>%
  left_join(df_Players %>% select(PlayerID, FullName))


### Ranking of players by centrality

# Plot 1. TOP 15 Players with highest PageRank
df_PlayersNetworkStats %>% arrange(-pagerank) %>% head(15) %>% 
  ggplot(aes(reorder(FullName, pagerank), pagerank)) +
  geom_col() + scale_y_continuous(limits = c(0.2,0.26), oob=rescale_none) +
  coord_flip() + theme_hc() 
  
# Plot 2. TOP 15 Players with highest Betweenness
df_PlayersNetworkStats %>% arrange(-betweenness) %>% head(15) %>% 
  ggplot(aes(reorder(FullName, betweenness), betweenness)) +
  geom_col() + scale_y_continuous(limits = c(0.4,0.6), oob=rescale_none) + 
  coord_flip() + theme_hc() 




################################################################################################
################ NETWORK №2. NETWORK OF TEAMS BASED ON THE PLAYED MATCHES
################################################################################################

# Select only 2010-2020 data
setwd("C:/Kaggle/Data/March_Madness_Analytics_2020")



################ 1. CONFERENCE NETWORKS BASED ON THE REGURAL SEASON RESULTS

# TODO: Split Regural season network based on the conferences
# TODO: Regular season centrality as a metric for the Tourney results

df_RegularSeasonCompactResults <- read.csv('MDataFiles_Stage2/MRegularSeasonCompactResults.csv')
df_TeamConferences <- read.csv('MDataFiles_Stage2/MTeamConferences.csv')


### Indicate Home Conference for a Team 

# Select only 2020 data and replicate TeamID column
df_TeamConferences <- df_TeamConferences %>% filter(Season == 2019) %>%
  mutate(WTeamID=TeamID, LTeamID=TeamID) %>% select(-c(Season, TeamID))

# Proportion of games played inside home confernces
df_RegularSeasonCompactResults <- read.csv('MDataFiles_Stage2/MRegularSeasonCompactResults.csv')

df_RegularSeasonCompactResults <- df_RegularSeasonCompactResults %>% filter(Season == 2019)
df_RegularSeasonCompactResults <- df_RegularSeasonCompactResults %>%
  left_join(df_TeamConferences %>% select(c(ConfAbbrev, WTeamID))) %>% rename(WConfAbbrev=ConfAbbrev) %>%
  left_join(df_TeamConferences %>% select(c(ConfAbbrev, LTeamID))) %>% rename(LConfAbbrev=ConfAbbrev)

df_RegularSeasonCompactResults <- df_RegularSeasonCompactResults %>% mutate(same_conf=WConfAbbrev==LConfAbbrev)

# Proportion
sum(df_RegularSeasonCompactResults$same_conf) / nrow(df_RegularSeasonCompactResults)

# Select only matches in the same conference
df_RegularSeasonCompactResults <- df_RegularSeasonCompactResults %>% filter(same_conf)


# Create dataframe with team-to-team wins and loses
team_to_team_results <- df_RegularSeasonCompactResults %>% 
  # filter(Season >= 2009) %>% 
  count(WTeamID, LTeamID) %>% 
  arrange(desc(n)) %>% 
  left_join(df_Teams %>% select(TeamID, TeamName), by = c("WTeamID" = "TeamID")) %>% 
  rename(WTeamName = TeamName) %>% 
  left_join(df_Teams %>% select(TeamID, TeamName), by = c("LTeamID" = "TeamID")) %>% 
  rename(LTeamName = TeamName)
team_to_team_results <- team_to_team_results %>%
  # select(-ends_with("ID")) %>% 
  rename(wins = n) %>% 
  left_join(team_to_team_results %>% select(-ends_with("ID")), by = c("WTeamName" = "LTeamName", "LTeamName" = "WTeamName")) %>% 
  rename(losses = n) %>% 
  replace_na(list(wins = 0, losses = 0)) %>% 
  mutate(win_perc = wins/(wins + losses) * 100) %>%
  # Select only teams with 10 or more games played between each other
  filter(wins+losses>=1) %>%
  mutate(WTeamID_LTeamID=paste(as.character(WTeamID), as.character(LTeamID), sep="_"))

# Create edgelist between teams on the wins agains each other
edgelist_wteam_lteam <- df_RegularSeasonCompactResults %>% 
  count(WTeamID, LTeamID) %>%
  mutate(WTeamID_LTeamID=paste(as.character(WTeamID), as.character(LTeamID), sep="_")) %>%
  filter(WTeamID_LTeamID %in% team_to_team_results$WTeamID_LTeamID) %>% 
  select(-c(WTeamID_LTeamID))
colnames(edgelist_wteam_lteam)[3] <- 'weight'

# Add conference name to edgelist
edgelist_wteam_lteam <- edgelist_wteam_lteam %>% left_join(df_TeamConferences %>% select(ConfAbbrev, WTeamID))

# Change order of WTeamID and LTeamID
edgelist_wteam_lteam <- edgelist_wteam_lteam %>% select(LTeamID, WTeamID, weight, ConfAbbrev)




### TODO: Plot on the same plot

# Create the network
g_teams <- graph.data.frame(edgelist_wteam_lteam, directed = T)

# Remove disconnected components
# g_teams <- induced_subgraph(g_teams, components(g_teams)$membership==1)

# Plot the network
vertex_size <- as.numeric(strength(g_teams, mode="in")) * 0.1
plot.igraph(g_teams, edge.arrow.size=0.05, layout=layout.kamada.kawai, edge.width=E(g_teams)$weight * 0.1,
            vertex.size=vertex_size, vertex.label=NA, vertex.color=as.factor(V(g_teams)$region_name))


(length(V(g_teams)) * length(V(g_teams))) - length(V(g_teams))


### TODO: Plot separatly

# Create separate graph of matches for each conference
conference_graphs_list <- list()
confrence_names <- as.character(unique(edgelist_wteam_lteam$ConfAbbrev))

for(i in 1:length(confrence_names)){
  
  conf_edgelist <- edgelist_wteam_lteam %>% filter(ConfAbbrev == confrence_names[i])
  conference_graphs_list[[i]] <- graph.data.frame(conf_edgelist, directed = T)
  
}


### Calculate the EigenVector centralization for each conference

calculateConferenceNetworkStatistics <- function(g){
  
  # 1. EigenVector centrality
  eigenvector_vec <- as.numeric(eigen_centrality(g, directed=T)$vector)
  
  # 2. Centralization metrics
  eigenvector_centralization <- centralize(eigenvector_vec, normalized = F)
  
  return(list(eigenvector_centralization=eigenvector_centralization))
}


df_ConferencesNetworkStats <- lapply(conference_graphs_list, calculateConferenceNetworkStatistics)
df_ConferencesNetworkStats <- do.call(rbind.data.frame, df_ConferencesNetworkStats)

# Add team_id
df_ConferencesNetworkStats$conference_name <- confrence_names
# Add graph_list number
df_ConferencesNetworkStats$id <- 1:nrow(df_ConferencesNetworkStats)


# 1. Example of the network with Biggest EigenVector centralization
G <- conference_graphs_list[[which.max(df_ConferencesNetworkStats$eigenvector_centralization)]]
vertex_size <- as.numeric(eigen_centrality(G, directed=T)$vector) * 30
plot.igraph(G, edge.arrow.size=0.25, layout=layout.kamada.kawai, edge.width=E(G)$weight*1,
            vertex.size=vertex_size)

# 2. Example of the network with Smallest EigenVector centralization
G <- conference_graphs_list[[which.min(df_ConferencesNetworkStats$eigenvector_centralization)]]
vertex_size <- as.numeric(eigen_centrality(G, directed=T)$vector) * 30
plot.igraph(G, edge.arrow.size=0.25, layout=layout.kamada.kawai, edge.width=E(G)$weight*1,
            vertex.size=vertex_size)




### Plot the grid of the conference networks

# Arrange based on the eigenvector centralization
df_ConferencesNetworkStats <- df_ConferencesNetworkStats %>% arrange(-eigenvector_centralization)


grid_list <- list()
for(i in 1:nrow(df_ConferencesNetworkStats)){
  
  temp_g <- conference_graphs_list[[df_ConferencesNetworkStats$id[i]]]
  
  grid_list[[i]] <- ggnet2(temp_g, directed = TRUE, arrow.size = 8,
                           node.size=as.numeric(eigen_centrality(temp_g, directed=T)$vector)) + 
    guides(color = FALSE, size = FALSE) + theme(panel.background = element_rect(color = "grey50"))
  
}

do.call(grid.arrange, grid_list)


### Plot the change in "competitiveness" of conferences over seasons. Time plot with lines.




################ 2. NETWORK BASED ON THE TOURNEY RESULTS


### 2.1. Add Home Region for a Team in 2019

df_TourneySeeds <- read.csv('MDataFiles_Stage2/MNCAATourneySeeds.csv')
df_Seasons <- read.csv('MDataFiles_Stage2/MSeasons.csv')

# Add letter + season to tourney seeds
df_TourneySeeds <- df_TourneySeeds %>% filter(Season >= 2015) %>%
  mutate(region_letter=substr(as.character(Seed), 1, 1)) %>%
  mutate(region_letter_season=paste(region_letter, Season, sep="_"))

# Add region name from df_Seasons
df_Seasons <- df_Seasons %>% filter(Season >= 2015 & Season != 2020) %>%
  gather(region_letter, region_name, RegionW:RegionZ, factor_key=TRUE) %>%
  mutate(region_letter=as.character(substr(region_letter, 7, 7))) %>%
  mutate(region_letter_season=paste(region_letter, Season, sep="_")) %>%
  select(region_letter_season, region_name)
df_TourneySeeds <- df_TourneySeeds %>% left_join(df_Seasons)

# TeamID and corresponding region
team_region <- df_TourneySeeds %>% distinct(TeamID, .keep_all = TRUE) %>%
  select(TeamID, region_name) %>% mutate(TeamID=as.character(TeamID))


### 2.2. Create the network

# Select only 2015+ tourney results
df_TourneyCompactResults <- read.csv('MDataFiles_Stage2/MNCAATourneyCompactResults.csv')
df_TourneyCompactResults <- df_TourneyCompactResults %>% filter(Season >= 2015)

# Create dataframe with team-to-team wins and loses
team_to_team_results <- df_TourneyCompactResults %>% 
  # filter(Season >= 2009) %>% 
  count(WTeamID, LTeamID) %>% 
  arrange(desc(n)) %>% 
  left_join(df_Teams %>% select(TeamID, TeamName), by = c("WTeamID" = "TeamID")) %>% 
  rename(WTeamName = TeamName) %>% 
  left_join(df_Teams %>% select(TeamID, TeamName), by = c("LTeamID" = "TeamID")) %>% 
  rename(LTeamName = TeamName)

team_to_team_results <- team_to_team_results %>%
  # select(-ends_with("ID")) %>% 
  rename(wins = n) %>% 
  left_join(team_to_team_results %>% select(-ends_with("ID")), by = c("WTeamName" = "LTeamName", "LTeamName" = "WTeamName")) %>% 
  rename(losses = n) %>% 
  replace_na(list(wins = 0, losses = 0)) %>% 
  mutate(win_perc = wins/(wins + losses) * 100) %>%
  # Select only teams with 10 or more games played between each other
  filter(wins+losses>=1) %>%
  mutate(WTeamID_LTeamID=paste(as.character(WTeamID), as.character(LTeamID), sep="_"))

# Create edgelist between teams on the wins agains each other
edgelist_wteam_lteam <- df_TourneyCompactResults %>% 
  count(WTeamID, LTeamID) %>%
  mutate(WTeamID_LTeamID=paste(as.character(WTeamID), as.character(LTeamID), sep="_")) %>%
  filter(WTeamID_LTeamID %in% team_to_team_results$WTeamID_LTeamID) %>% 
  select(-c(WTeamID_LTeamID))
colnames(edgelist_wteam_lteam)[3] <- 'weight'

# Drop weights
edgelist_wteam_lteam$weight <- NULL

# Change order of WTeam and LTeam
edgelist_wteam_lteam <- edgelist_wteam_lteam %>% select(LTeamID, WTeamID)

# Create the network
g_teams <- graph.data.frame(edgelist_wteam_lteam, directed = T)

# Remove disconnected components
g_teams <- induced_subgraph(g_teams, components(g_teams)$membership==1)

# Add region affilation as node attribute
temp_stats <- data.frame(TeamID=names(V(g_teams)))
temp_stats$TeamID <- as.character(temp_stats$TeamID)
temp_stats <- temp_stats %>% left_join(team_region)
g_teams <- set_vertex_attr(graph=g_teams, name="region_name", value=temp_stats$region_name)

# Add network statistics of a team based on the Network №1
temp_stats <- temp_stats %>%
  left_join(df_TeamsNetworkStats %>% mutate(TeamID=as.character(TeamID)))
g_teams <- set_vertex_attr(graph=g_teams, name="mean_transitivity", value=temp_stats$mean_transitivity)
g_teams <- set_vertex_attr(graph=g_teams, name="betweenness_centralization", value=temp_stats$betweenness_centralization)
g_teams <- set_vertex_attr(graph=g_teams, name="pagerank_centralization", value=temp_stats$pagerank_centralization)

# Plot the network

vertex_size <- as.numeric(strength(g_teams, mode="in")) * 0.5
plot.igraph(g_teams, edge.arrow.size=0.05, layout=layout.circle, edge.width=0.01,
            vertex.size=vertex_size, vertex.label=NA, vertex.color=as.factor(V(g_teams)$region_name))


### 2.3. ERGM specification

# invlogit <- function(x) {1/(1 + exp(-x))}

# Transform igraph to network class
g_teams_net <- asNetwork(g_teams)

# Model 1. Edges
ergm_model.01 <- ergm(g_teams_net ~ edges)
summary(ergm_model.01)

# Model 2. Edges + Mutality
ergm_model.02 <- ergm(g_teams_net ~ edges + mutual + nodefactor("region_name"))
summary(ergm_model.02)

# Model 3. Edges + Mutality + IVs
ergm_model.03 <- ergm(g_teams_net ~ edges + mutual +
                        nodefactor("region_name") + nodecov("mean_transitivity") +
                        nodecov("betweenness_centralization") + nodecov("pagerank_centralization"))
summary(ergm_model.03)



