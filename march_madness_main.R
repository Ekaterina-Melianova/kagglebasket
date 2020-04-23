
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

setwd("C:/Users/Artem/Google Drive/KAGGLE/March_Madness_Analytics_2020")



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
setwd("C:/Users/Artem/Google Drive/KAGGLE/March_Madness_Analytics_2020/MPlayByPlay_Stage2")

for(each in list.files()[str_detect(list.files(), "MEvents")]) {
  
  df <- read_csv(paste0(each))
  
  # Grouped shooting variables ----------------------------------------------
  # there are some shooting variables that can probably be condensed - tip ins and dunks
  paint_attempts_made <- c("made2_dunk", "made2_lay", "made2_tip") 
  paint_attempts_missed <- c("miss2_dunk", "miss2_lay", "miss2_tip") 
  paint_attempts <- c(paint_attempts_made, paint_attempts_missed)
  # create variables for field goals made, and also field goals attempted (which includes the sum of FGs made and FGs missed)
  FGM <- c("made2_dunk", "made2_jump", "made2_lay",  "made2_tip",  "made3_jump")
  FGA <- c(FGM, "miss2_dunk", "miss2_jump" ,"miss2_lay",  "miss2_tip",  "miss3_jump")
  # variable for three-pointers
  ThreePointer <- c("made3_jump", "miss3_jump")
  #  Two point jumper
  TwoPointJump <- c("miss2_jump", "made2_jump")
  # Free Throws
  FT <- c("miss1_free", "made1_free")
  # all shots
  AllShots <- c(FGA, FT)
  
  
  # Feature Engineering -----------------------------------------------------
  # paste the two even variables together for FGs as this is the format for last years comp data
  df <- df %>%
    mutate_if(is.factor, as.character) %>% 
    mutate(EventType = ifelse(str_detect(EventType, "miss") | str_detect(EventType, "made") | str_detect(EventType, "reb"), paste0(EventType, "_", EventSubType), EventType))
  
  # change the unknown for 3s to "jump" and for FTs "free"
  df <- df %>% 
    mutate(EventType = ifelse(str_detect(EventType, "3"), str_replace(EventType, "_unk", "_jump"), EventType),
           EventType = ifelse(str_detect(EventType, "1"), str_replace(EventType, "_unk", "_free"), EventType))
  
  
  df <- df %>% 
    # create a variable in the df for whether the attempts was made or missed
    mutate(shot_outcome = ifelse(grepl("made", EventType), "Made", ifelse(grepl("miss", EventType), "Missed", NA))) %>%
    # identify if the action was a field goal, then group it into the attempt types set earlier
    mutate(FGVariable = ifelse(EventType %in% FGA, "Yes", "No"),
           AttemptType = ifelse(EventType %in% paint_attempts, "PaintPoints", 
                                ifelse(EventType %in% ThreePointer, "ThreePointJumper", 
                                       ifelse(EventType %in% TwoPointJump, "TwoPointJumper", 
                                              ifelse(EventType %in% FT, "FreeThrow", "NoAttempt")))))
  
  
  # Rework DF so only shots are included and whatever lead to the shot --------
  df <- df %>% 
    mutate(GameID = paste(Season, DayNum, WTeamID, LTeamID, sep = "_")) %>% 
    group_by(GameID, ElapsedSeconds) %>% 
    mutate(EventType2 = lead(EventType),
           EventPlayerID2 = lead(EventPlayerID)) %>% ungroup()
  
  
  df <- df %>% 
    mutate(FGVariableAny = ifelse(EventType %in% FGA | EventType2 %in% FGA, "Yes", "No")) %>% 
    filter(FGVariableAny == "Yes") 
  
  
  # create a variable for if the shot was made, but then the second event was also a made shot
  df <- df %>% 
    mutate(Alert = ifelse(EventType %in% FGM & EventType2 %in% FGM, "Alert", "OK")) %>% 
    # only keep "OK" observations
    filter(Alert == "OK")
  

  # replace NAs with somerhing
  df$EventType2[is.na(df$EventType2)] <- "no_second_event"
  
  
  # create a variable for if there was an assist on the FGM:
  df <- df %>% 
    mutate(AssistedFGM = ifelse(EventType %in% FGM & EventType2 == "assist", "Assisted", 
                                ifelse(EventType %in% FGM & EventType2 != "assist", "Solo", 
                                       ifelse(EventType %in% FGM & EventType2 == "no_second_event", "Solo", "None"))))
  
  # # because the FGA culd be either in `EventType` (more likely) or `EventType2` (less likely), need
  # # one variable to indicate the shot type
  # df <- df %>% \
  #   mutate(fg_type = ifelse(EventType %in% FGA, EventType, ifelse(EventType2 %in% FGA, EventType2, "Unknown")))
  
  # create final output
  df <- df %>% ungroup()
  play_by_play <- bind_rows(play_by_play, df)
  print(each)
  
  rm(df); gc()
}

saveRDS(play_by_play, "play_by_play2015_19.rds")


# Load preprocessed final play-by-play data
setwd("C:/Users/Artem/Google Drive/KAGGLE/March_Madness_Analytics_2020/MPlayByPlay_Stage2")
play_by_play <- readRDS("play_by_play2015_19.rds")

# Select only 2016 Season for now
play_by_play <- play_by_play[play_by_play$Season == 2016,]



################ 3. DATA PREPROCESSING TEAMS

# 3.1. Percentage of shots made solo in the team
teams_solo_shots <- play_by_play %>% 
  filter(AssistedFGM != "None") %>% 
  group_by(EventTeamID, AssistedFGM) %>% 
  summarise(n = n()) %>% 
  mutate(solo_shot_perc = n / sum(n)) %>% 
  filter(AssistedFGM == "Solo") %>% arrange(desc(solo_shot_perc)) %>% ungroup()

# 3.2. Coach change during the 2015-2020 seasons
# TODO


################ 4. DATA PREPROCESSING PLAYERS

# TODO: Caclulate set of additional statistics for a player





################ 5. CREATE NETWORKS OF ASSISTS

# Select only plays with assists
play_by_play <- subset(play_by_play, EventType2 == "assist")

# Create list with Team IDs
team_ids <- unique(play_by_play$EventTeamID)

# Remove missing team ids
team_ids <- team_ids[-which(team_ids == 0)]


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


################ 6. CALCULATE SET OF NETWORK STATISTICS FOR ALL TEAMS

minmax_normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}


calculateNetworkStatistics <- function(g){
  
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
  
  # 6. Page Rank
  pagerank_vec <- page.rank(g)$vector
  pagerank_vec <- pagerank_vec/sum(pagerank_vec)
  
  # 7. EigenVector centrality
  eigenvector_vec <- as.numeric(eigen_centrality(g)$vector)
  
  # 8. Centralization metrics
  betweenness_centralization <- centralize(betweenness_vec, normalized = F)
  indegree_centralization <- centralize(in_degree_vec, normalized = F)
  outdegree_centralization <- centralize(out_degree_vec, normalized = F)
  pagerank_centralization <- centralize(pagerank_vec, normalized = F)
  eigenvector_centralization <- centralize(eigenvector_vec, normalized = F)
  
  
  return(list(nodes_n=nodes_n,
              mean_transitivity=mean_transitivity,
              betweenness_centralization=betweenness_centralization,
              indegree_centralization=indegree_centralization,
              outdegree_centralization=outdegree_centralization,
              pagerank_centralization=pagerank_centralization,
              eigenvector_centralization=eigenvector_centralization))
}


df_TeamsNetworkStats <- lapply(assist_graphs_list, calculateNetworkStatistics)
df_TeamsNetworkStats <- do.call(rbind.data.frame, df_TeamsNetworkStats)

# Add team_id
df_TeamsNetworkStats$TeamID <- team_ids

# Add mean rank for the team in that season
df_MasseyOrdinals_pom_2016 <- subset(df_MasseyOrdinals, SystemName=="POM" & Season==2016) %>%
  select(TeamID, RankingDayNum, OrdinalRank) %>% group_by(TeamID) %>%
  summarize(median_rank=median(OrdinalRank)) %>% data.frame()

# Add ranking to the teams stats
df_TeamsNetworkStats <- left_join(df_TeamsNetworkStats, df_MasseyOrdinals_pom_2016, by="TeamID")

# Add percentage of solo shots
df_TeamsNetworkStats <- left_join(df_TeamsNetworkStats,
                                  teams_solo_shots[,c("EventTeamID", "solo_shot_perc")],
                        by=c("TeamID"="EventTeamID"))


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
summary(lm(median_rank ~ mean_transitivity + betweenness_centralization + indegree_centralization + 
       outdegree_centralization + pagerank_centralization + eigenvector_centralization + solo_shot_perc,
       df_TeamsNetworkStats))


### 6.2. Plot the network examples

# 1. Example of the network with Biggest InDegree centralization
G <- assist_graphs_list[[which.max(df_TeamsNetworkStats$indegree_centralization)]]
vertex_size <- as.numeric(strength(G, mode="in")) * 0.5
plot.igraph(G, edge.arrow.size=0.25, layout=layout.kamada.kawai, edge.width=E(G)$weight*0.3,
            vertex.size=vertex_size)

# 2. Example of the network with Smallest InDegree centralization
G <- assist_graphs_list[[which.min(df_TeamsNetworkStats$indegree_centralization)]]
vertex_size <- as.numeric(strength(G, mode="in")) * 2
plot.igraph(G, edge.arrow.size=0.25, layout=layout.kamada.kawai, edge.width=E(G)$weight*0.3,
            vertex.size=vertex_size)

# 3. Example of the network with Biggest Betweenness centralization
G <- assist_graphs_list[[which.max(df_TeamsNetworkStats$betweenness_centralization)]]
vertex_size <- as.numeric(betweenness(G, normalized=T))*200
plot.igraph(G, edge.arrow.size=0.25, layout=layout.kamada.kawai, edge.width=E(G)$weight*0.3,
            vertex.size=vertex_size)

# 4. Example of the network with Smallest Betweenness centralization
G <- assist_graphs_list[[which.min(df_TeamsNetworkStats$betweenness_centralization)]]
vertex_size <- as.numeric(betweenness(G, normalized=T))*200
plot.igraph(G, edge.arrow.size=0.25, layout=layout.kamada.kawai, edge.width=E(G)$weight*0.3,
            vertex.size=vertex_size)


################ 7. CALCULATE NETWORK METRICS FOR EACH PLAYER

TODO:



################ 8. TEAMS AND COACHES, BI-PARTITE NETWORK

# Create edgelist with number of seasons together as weights
edgelist_team_coach <- as.data.frame(table(df_TeamCoaches$CoachName, df_TeamCoaches$TeamID))
edgelist_team_coach <- edgelist_team_coach[edgelist_team_coach$Freq > 0,]
colnames(edgelist_team_coach) <- c("coach_name", "team_id", "weight")

# Select only coaches with several teams experience
temp <- as.data.frame(table(edgelist_team_coach$coach_name))
temp <- temp[temp$Freq > 1,]
multi_team_coaches <- as.character(temp$Var1)
edgelist_team_coach <- edgelist_team_coach[edgelist_team_coach$coach_name %in% multi_team_coaches,]
edgelist_team_coach$coach_name <- as.character(edgelist_team_coach$coach_name)
edgelist_team_coach$team_id <- as.character(edgelist_team_coach$team_id)

# Create bi-partite network
g <- graph.data.frame(edgelist_team_coach, directed = F)
V(g)$type <- V(g)$name %in% edgelist_team_coach$coach_name #the second column of edges is TRUE type
E(g)$weight <- as.numeric(edgelist_team_coach$weight)

# Color the nodes based on the type
V(g)$color <- V(g)$type
V(g)$color=gsub("FALSE","red",V(g)$color)
V(g)$color=gsub("TRUE","blue",V(g)$color)



layout_bi <- layout_as_bipartite(g)

plot.igraph(g, edge.color=adjustcolor("darkgray", .3), 
            edge.width=1, layout=layout_bi,
            vertex.size=1, vertex.label=NA)

# E(g)$weight*0.5


################ 9. TEAMS NETWORK BASED ON THE COMMON COACHES

# TODO: Reverse network - coaches network based on the common teams

# Dataframe with team and coaches affilated with it
team_coaches <- group_by(edgelist_team_coach, team_id) %>%
  summarize(type = list(sort(unique(coach_name))))

# Create adjacency matrix between teams based on the common coaches
network_team_adjacency <- sapply(seq_len(length(team_coaches$type)), function(x) 
  sapply(seq_len(length(team_coaches$type)),
         function(y) length(intersect(unlist(team_coaches$type[x]), unlist(team_coaches$type[y])))))
network_team_adjacency <- as.data.frame(network_team_adjacency)
rownames(network_team_adjacency) <- team_coaches$team_id
colnames(network_team_adjacency) <- team_coaches$team_id

# Create graph
network_team_adjacency <- as.matrix(network_team_adjacency)
diag(network_team_adjacency) <- 0
g <- graph_from_adjacency_matrix(as.matrix(network_team_adjacency))
g <- as.undirected(g)

# Plot the graph
plot.igraph(g, edge.color=adjustcolor("darkgray", .3), 
            edge.width=1, layout=layout.fruchterman.reingold,
            vertex.size=3, vertex.label=NA)



################ 10. NETWORK OF TEAMS BASED ON THE MATCHES PLAYED
# Only based on the regular season for now
# TODO: Tourney or Regural season results?

# Create dataframe with team-to-team wins and loses
team_to_team <- df_TourneyCompactResults %>% 
  # filter(Season >= 2009) %>% 
  count(WTeamID, LTeamID) %>% 
  arrange(desc(n)) %>% 
  left_join(df_Teams %>% select(TeamID, TeamName), by = c("WTeamID" = "TeamID")) %>% 
  rename(WTeamName = TeamName) %>% 
  left_join(df_Teams %>% select(TeamID, TeamName), by = c("LTeamID" = "TeamID")) %>% 
  rename(LTeamName = TeamName) %>% 
  # select(-ends_with("ID")) %>% 
  rename(wins = n) %>% 
  left_join(foo %>% select(-ends_with("ID")), by = c("WTeamName" = "LTeamName", "LTeamName" = "WTeamName")) %>% 
  rename(losses = n) %>% 
  replace_na(list(wins = 0, losses = 0)) %>% 
  mutate(win_perc = wins/(wins + losses) * 100) %>%
  # Select only teams with 10 or more games played between each other
  filter(wins+losses>=5) %>%
  mutate(WTeamID_LTeamID=paste(as.character(WTeamID), as.character(LTeamID), sep="_"))


# Create edgelist between teams on the wins agains each other
edgelist_wteam_lteam <- df_RegularSeasonCompactResults %>% 
  count(WTeamID, LTeamID) %>%
  mutate(WTeamID_LTeamID=paste(as.character(WTeamID), as.character(LTeamID), sep="_")) %>%
  filter(WTeamID_LTeamID %in% team_to_team$WTeamID_LTeamID) %>% 
  select(-c(WTeamID_LTeamID))
colnames(edgelist_wteam_lteam)[3] <- 'weight'


# Create the network
g <- graph.data.frame(edgelist_wteam_lteam, directed = T)

# Remove disconnected components
g <- induced_subgraph(g, components(g)$membership==1)

# Plot the network
vertex_size <- as.numeric(strength(g, mode="out")) * 0.05
plot.igraph(g, edge.arrow.size=0.03, layout=layout.circle, edge.width=0.01,
            vertex.size=vertex_size, vertex.label=NA)






