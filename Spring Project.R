#SAC Final Project Code
#By: Brandon Wisniewski

install.packages("baseballr")
install.packages("cfbfastR")
install.packages("nflfastR")
install.packages("rvest")
install.packages("XML")
install.packages("reshape2")
install.packages("gt")
install.packages("formattable")
install.packages("ggimage")


library(tidyverse)
library(readr)
library(ggplot2)
library(rvest)
library(XML)
library(reshape2)
library(gt)
library(ggimage)


nfl_pbp <- load_pbp(2000:2022)
#Lamar Jackson career counting stats in regular season


# Read the HTML content from the URL
url <- "https://www.pro-football-reference.com/players/J/JackLa00.htm?sr&utm_source=direct&utm_medium=Share&utm_campaign=ShareTool#passing"
page <- read_html(url)

# Extract the passing statistics table
LJ_rs <- page %>%
  html_node("#passing") %>%
  html_table(header = TRUE)

# Clean up the column names
colnames(LJ_rs) <- gsub("[^[:alnum:]]", "", colnames(LJ_rs))
colnames(LJ_rs) <- gsub("Rate", "QBR", colnames(LJ_rs))

# Convert the data types
LJ_rs$Year <- as.integer(LJ_rs$Year)
LJ_rs$Age <- as.integer(LJ_rs$Age)
LJ_rs$G <- as.integer(LJ_rs$G)
LJ_rs$GS <- as.integer(LJ_rs$GS)
LJ_rs$Cmp <- as.integer(gsub(",", "", LJ_rs$Cmp))
LJ_rs$Att <- as.integer(gsub(",", "", LJ_rs$Att))
LJ_rs$Yds <- as.integer(gsub(",", "", LJ_rs$Yds))
LJ_rs$TD <- as.integer(LJ_rs$TD)
LJ_rs$Int <- as.integer(LJ_rs$Int)
LJ_rs$Lng <- as.character(LJ_rs$Lng)
LJ_rs$Lng <- as.integer(gsub("\\D", "", LJ_rs$Lng))
LJ_rs$QBR <- as.numeric(gsub(",", ".", LJ_rs$QBR))

# View the resulting data frame
head(LJ_rs)

#in the playoffs

url2 <- "https://www.pro-football-reference.com/players/J/JackLa00.htm?sr&utm_source=direct&utm_medium=Share&utm_campaign=ShareTool#passing_playoffs"

page2 <- read_html(url2)

# Extract the passing statistics table
LJ_ps <- page2 %>%
  html_node("#passing_playoffs") %>%
  html_table(header = TRUE)

# Clean up the column names
colnames(LJ_ps) <- gsub("[^[:alnum:]]", "", colnames(LJ_ps))
colnames(LJ_ps) <- gsub("Rate", "QBR", colnames(LJ_ps))

# Convert the data types
LJ_ps$Year <- as.integer(LJ_ps$Year)
LJ_ps$Age <- as.integer(LJ_ps$Age)
LJ_ps$G <- as.integer(LJ_ps$G)
LJ_ps$GS <- as.integer(LJ_ps$GS)
LJ_ps$Cmp <- as.integer(gsub(",", "", LJ_ps$Cmp))
LJ_ps$Att <- as.integer(gsub(",", "", LJ_ps$Att))
LJ_ps$Yds <- as.integer(gsub(",", "", LJ_ps$Yds))
LJ_ps$TD <- as.integer(LJ_ps$TD)
LJ_ps$Int <- as.integer(LJ_ps$Int)
LJ_ps$Lng <- as.character(LJ_ps$Lng)
LJ_ps$Lng <- as.integer(gsub("\\D", "", LJ_ps$Lng))
LJ_ps$QBR <- as.numeric(gsub(",", ".", LJ_ps$QBR))

#Overall Salary Cap data

salary_cap <- c(224800000, 208200000, 182500000, 198200000)

#find QB data for each current QB you want to analyze
LJ_pbp <- nfl_pbp %>% 
  filter(passer == "L.Jackson" |
         rusher == "L.Jackson") %>% 
  mutate(ID = as.numeric(old_game_id),
         year = as.factor(floor(ID / 1000000))) %>% 
  group_by(year) %>% 
  ungroup()

PM_pbp <- nfl_pbp %>% 
  filter(passer == "P.Mahomes" |
           rusher == "P.Mahomes") %>% 
  mutate(ID = as.numeric(old_game_id),
         year = as.factor(floor(ID / 1000000))) %>% 
  group_by(year) %>% 
  ungroup()

JA_pbp <- nfl_pbp %>% 
  filter(passer == "J.Allen" |
           rusher == "J.Allen") %>% 
  mutate(ID = as.numeric(old_game_id),
         year = as.factor(floor(ID / 1000000))) %>%  
  group_by(year) %>% 
  ungroup()

JB_pbp <- nfl_pbp %>% 
  filter(passer == "J.Burrow" |
           rusher == "J.Burrow") %>% 
  mutate(ID = as.numeric(old_game_id),
         year = as.factor(floor(ID / 1000000))) %>% 
  group_by(year) %>% 
  ungroup() 

JH_pbp <- nfl_pbp %>% 
  filter(passer == "J.Herbert" |
           rusher == "J.Herbert") %>% 
  mutate(ID = as.numeric(old_game_id),
         year = as.factor(floor(ID / 1000000))) %>% 
  group_by(year) %>% 
  ungroup()

DJ_pbp <- nfl_pbp %>% 
  filter(passer == "D.Jones" |
           rusher == "D.Jones") %>% 
  mutate(ID = as.numeric(old_game_id),
         year = as.factor(floor(ID / 1000000))) %>% 
  group_by(year) %>% 
  ungroup()

DW_pbp <- nfl_pbp %>% 
  filter(passer == "D.Watson" |
           rusher == "D.Watson") %>% 
  mutate(ID = as.numeric(old_game_id),
         year = as.factor(floor(ID / 1000000))) %>% 
  group_by(year) %>% 
  ungroup()

RW_pbp <- nfl_pbp %>% 
  filter(passer == "R.Wilson" |
           rusher == "R.Wilson") %>% 
  mutate(ID = as.numeric(old_game_id),
         year = as.factor(floor(ID / 1000000))) %>% 
  group_by(year) %>% 
  ungroup()

JaH_pbp <- nfl_pbp %>% 
  filter(passer == "J.Hurts" |
           rusher == "J.Hurts") %>% 
  mutate(ID = as.numeric(old_game_id),
         year = as.factor(floor(ID / 1000000))) %>% 
  group_by(year) %>% 
  ungroup()

#Historical QBs

AR_pbp <- nfl_pbp %>% 
  filter(passer == "A.Rodgers" |
           rusher == "A.Rodgers") %>% 
  mutate(ID = as.numeric(old_game_id),
         year = as.factor(floor(ID / 1000000))) %>% 
  group_by(year) %>% 
  ungroup()

CN_pbp <- nfl_pbp %>% 
  filter(passer == "C.Newton" |
           rusher == "C.Newton") %>% 
  mutate(ID = as.numeric(old_game_id),
         year = as.factor(floor(ID / 1000000))) %>% 
  group_by(year) %>% 
  ungroup()

MV_pbp <- nfl_pbp %>% 
  filter(passer == "M.Vick" |
           rusher == "M.Vick")  %>% 
  mutate(ID = as.numeric(old_game_id),
         year = as.factor(floor(ID / 1000000))) %>% 
  group_by(year) %>% 
  ungroup()

#debug
i = 1
class(colnames(LJ_pbp))


#Contract Details from Spottrac 2023

url_2023 <- "https://www.spotrac.com/nfl/positional/breakdown/"
contracts_2023 <- read_html(url_2023)

contracts_2023_df <- as.data.frame(html_table(contracts_2023))


# define a regular expression to match the value in millions
split_column <- function(column) {
  millions_regex <- "\\d+\\.\\d+M"
  column <- as.character(column)
  column_millions <- sub(millions_regex, "\\1", column)
  column_millions <- as.numeric(gsub("M", "", column_millions))
  column <- as.numeric(column)
  return(data.frame(column_millions))
}

# apply the function to each column using lapply
df_split <- lapply(contracts_2023_df[3:12], split_column)

# combine the results into a single dataframe
df_result <- do.call(cbind, df_split)

colnames(df_result) <- c("QB", "RB", "WR", "TE", 
                         "OL", "DL", "LB", "DB", "ST", "TOT")

df_result <- df_result %>%
  filter(QB >= 0) %>% 
  mutate_all(~as.numeric(as.character(.)))

col_means <- sapply(df_result, mean)

df_result <- add_row(df_result, !!!col_means)

contracts_2023_df <- contracts_2023_df %>%
  filter(Players >= 0)

new_row <- as.data.frame(list("Average", 53))
colnames(new_row) <- c("Team", "Players")

contracts_2023_df <- add_row(contracts_2023_df, new_row)

contracts_2023_df <- bind_cols(contracts_2023_df[1:2], df_result)

QB_spending_2023 <- contracts_2023_df %>% 
  select(Team, QB, TOT) %>% 
  mutate(nonQB = TOT - QB)

#Contract Details from Spottrac 2022

url_2022 <- "https://www.spotrac.com/nfl/positional/breakdown/2022/"
contracts_2022 <- read_html(url_2022)

contracts_2022_df <- as.data.frame(html_table(contracts_2022))


# define a regular expression to match the value in millions
split_column <- function(column) {
  millions_regex <- "\\d+\\.\\d+M"
  column <- as.character(column)
  column_millions <- sub(millions_regex, "\\1", column)
  column_millions <- as.numeric(gsub("M", "", column_millions))
  column <- as.numeric(column)
  return(data.frame(column_millions))
}

# apply the function to each column using lapply
df_split <- lapply(contracts_2022_df[3:12], split_column)

# combine the results into a single dataframe
df_result <- do.call(cbind, df_split)

colnames(df_result) <- c("QB", "RB", "WR", "TE", 
                         "OL", "DL", "LB", "DB", "ST", "TOT")

df_result <- df_result %>%
  filter(QB >= 0) %>% 
  mutate_all(~as.numeric(as.character(.)))

col_means <- sapply(df_result, mean)

df_result <- add_row(df_result, !!!col_means)

contracts_2022_df <- contracts_2022_df %>%
  filter(Players >= 0)

new_row <- as.data.frame(list("Average", 53))
colnames(new_row) <- c("Team", "Players")

contracts_2022_df <- add_row(contracts_2022_df, new_row)

contracts_2022_df <- bind_cols(contracts_2022_df[1:2], df_result)

QB_spending_2022 <- contracts_2022_df %>% 
  select(Team, QB, TOT) %>% 
  mutate(nonQB = TOT - QB)

#Contract Details from Spottrac 2021

url_2021 <- "https://www.spotrac.com/nfl/positional/breakdown/2021/"
contracts_2021 <- read_html(url_2021)

contracts_2021_df <- as.data.frame(html_table(contracts_2021))


# define a regular expression to match the value in millions
split_column <- function(column) {
  millions_regex <- "\\d+\\.\\d+M"
  column <- as.character(column)
  column_millions <- sub(millions_regex, "\\1", column)
  column_millions <- as.numeric(gsub("M", "", column_millions))
  column <- as.numeric(column)
  return(data.frame(column_millions))
}

# apply the function to each column using lapply
df_split <- lapply(contracts_2021_df[3:12], split_column)

# combine the results into a single dataframe
df_result <- do.call(cbind, df_split)

colnames(df_result) <- c("QB", "RB", "WR", "TE", 
                         "OL", "DL", "LB", "DB", "ST", "TOT")

df_result <- df_result %>%
  filter(QB >= 0) %>% 
  mutate_all(~as.numeric(as.character(.)))

col_means <- sapply(df_result, mean)

df_result <- add_row(df_result, !!!col_means)

contracts_2021_df <- contracts_2021_df %>%
  filter(Players >= 0)

new_row <- as.data.frame(list("Average", 53))
colnames(new_row) <- c("Team", "Players")

contracts_2021_df <- add_row(contracts_2021_df, new_row)

contracts_2021_df <- bind_cols(contracts_2021_df[1:2], df_result)

QB_spending_2021 <- contracts_2021_df %>% 
  select(Team, QB, TOT) %>% 
  mutate(nonQB = TOT - QB)

#Contract Details from Spottrac 2020

url_2020 <- "https://www.spotrac.com/nfl/positional/breakdown/2020/"
contracts_2020 <- read_html(url_2020)

contracts_2020_df <- as.data.frame(html_table(contracts_2020))


# define a regular expression to match the value in millions
split_column <- function(column) {
  millions_regex <- "\\d+\\.\\d+M"
  column <- as.character(column)
  column_millions <- sub(millions_regex, "\\1", column)
  column_millions <- as.numeric(gsub("M", "", column_millions))
  column <- as.numeric(column)
  return(data.frame(column_millions))
}

# apply the function to each column using lapply
df_split <- lapply(contracts_2020_df[3:12], split_column)

# combine the results into a single dataframe
df_result <- do.call(cbind, df_split)

colnames(df_result) <- c("QB", "RB", "WR", "TE", 
                         "OL", "DL", "LB", "DB", "ST", "TOT")

df_result <- df_result %>%
  filter(QB >= 0) %>% 
  mutate_all(~as.numeric(as.character(.)))

col_means <- sapply(df_result, mean)

df_result <- add_row(df_result, !!!col_means)

contracts_2020_df <- contracts_2020_df %>%
  filter(Players >= 0)

new_row <- as.data.frame(list("Average", 53))
colnames(new_row) <- c("Team", "Players")

contracts_2020_df <- add_row(contracts_2020_df, new_row)

contracts_2020_df <- bind_cols(contracts_2020_df[1:2], df_result)

QB_spending_2020 <- contracts_2020_df %>% 
  select(Team, QB, TOT) %>% 
  mutate(nonQB = TOT - QB)

#Combine Averages from each year into 1 small data frame to use for graph

year <- c(2020, 2021, 2022, 2023)
QB_spending <- c(16524703, 14724302, 15001230, 17861878)
nonQB_spending <- c(124844421, 116893852, 126181418, 191806187)
lamar_spending <- c(1535980, 1771588, 23016000, 32416000)
champ_QB_spending <- c(27532500, 21515000, 38618381)

df <- data.frame(year, nonQB_spending, QB_spending)
df1 <- melt(df, id.vars = "year")
df1$count_millions <- df1$value / 1000000

#Make Stacked bar chart

QB_Contract_Graph <- ggplot(df1, aes(x=year, y=value, fill=variable)) +
  geom_bar(stat='identity', position='stack') +
  geom_text(aes(x = year, y = value, label = paste0("$", round(count_millions, 2), " M")), 
            position=position_stack(vjust=0.5), size = 2.6) +
  scale_y_continuous(labels=scales::comma_format(unit="M")) +
  scale_fill_manual(values = c("QB_spending" = "gold", "nonQB_spending" = "lightblue"),
                    labels = c("Quarterback", "Non-Quarterback")) +
  labs(x = "Year", 
       y = "Money Spent", 
       title = "Average Money spent by NFL team",
       subtitle = "QBs vs Non-QBs 2020-2023",
       fill = "Position",
       caption = "Graph by Brandon Wisniewski | Data from Spottrac.com") +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5))+
  theme_dark() +
  
  #Red Lines for Salary Cap
  geom_segment(aes(x=year[1]-0.5, xend=year[1]+0.5, y=salary_cap[4], yend=salary_cap[4]), color="red", size=1.5) +
  geom_segment(aes(x=year[2]-0.5, xend=year[2]+0.5, y=salary_cap[3], yend=salary_cap[3]), color="red", size=1.5) +
  geom_segment(aes(x=year[3]-0.5, xend=year[3]+0.5, y=salary_cap[2], yend=salary_cap[2]), color="red", size=1.5) +
  geom_segment(aes(x=year[4]-0.5, xend=year[4]+0.5, y=salary_cap[1], yend=salary_cap[1]), color="red", size=1.5) +
  
  #Purple Lines for Lamar
  geom_segment(aes(x=year[1]-0.5, xend=year[1]+0.5, y=lamar_spending[1], yend=lamar_spending[1]), color="#241773", size=1.5) +
  geom_segment(aes(x=year[2]-0.5, xend=year[2]+0.5, y=lamar_spending[2], yend=lamar_spending[2]), color="#241773", size=1.5) +
  geom_segment(aes(x=year[3]-0.5, xend=year[3]+0.5, y=lamar_spending[3], yend=lamar_spending[3]), color="#241773", size=1.5) +
  geom_segment(aes(x=year[4]-0.5, xend=year[4]+0.5, y=lamar_spending[4], yend=lamar_spending[4]), color="#241773", size=1.5) + 
  
  #Team-Specific Colored Lines for SB Champ
  geom_segment(aes(x = year[1]-0.5, xend=year[1]+0.5, y=champ_QB_spending[1], yend=champ_QB_spending[1]), color="#FF7900", size=1.5)+
  geom_segment(aes(x = year[2]-0.5, xend=year[2]+0.5, y=champ_QB_spending[2], yend=champ_QB_spending[2]), color="#003594", size=1.5)+
  geom_segment(aes(x = year[3]-0.5, xend=year[3]+0.5, y=champ_QB_spending[3], yend=champ_QB_spending[3]), color="#E31837", size=1.5)


#Make Tables for Lamar (Basic) Career Stats


LJ_rs$Year <- as.factor(c(2018, 2019, 2020, 2021, 2022, "total"))

#Add Ravens Logo to dataframes

img <- rep("RAVEN.png", 6)

new_names <- c( "Year" ,   "Age"   ,  "Tm" ,     "Pos", "No",   "G"   ,    "GS"  ,    "QBrec" ,  "Cmp" ,    "Att" ,    "Cmp_PCT"   , 
            "Yds"   ,  "TD"    ,  "TD_pct" ,     "Int"    , "Int_pct"  ,   "1D"  ,    "Lng"  ,   "YA"  ,    "AYA"  ,   "YC"   ,   "YG"    , 
              "PasserRtg", "QBR"   ,  "Sk"   ,   "Sack_Yds_Lost"  ,   "Yds_per_Sk"   ,   "NYA"   ,  "ANYA"  ,  "4QC"  ,   "GWD"   ,  "AV"  ,    "Awards")

colnames(LJ_rs) <- new_names
colnames(LJ_ps) <- new_names

LJ_rs_selected <- LJ_rs %>% 
  select(Year, Age, GS, Cmp, Yds, TD, Int, QBR)

rs_table <- gt(LJ_rs_selected) %>% 
            tab_header("Lamar Jackson Regular Season Stats") %>% 
            tab_source_note("By Brandon Wisniewski | Data from SportsReference")
  
LJ_ps_selected <- LJ_ps %>% 
  select(Year, Age, GS, Cmp, Yds, TD, Int, QBR)
LJ_ps$Year <- as.factor(c(2018, 2019, 2020, "total"))  

ps_table <- gt(LJ_ps_selected) %>% 
  tab_header("Lamar Jackson Post Season Stats") %>% 
  tab_source_note("By Brandon Wisniewski | Data from SportsReference")

LJ_scatter <- ggplot(LJ_rs, aes(x = Cmp_PCT,xend = 68, y = AYA, yend = 10)) +
  geom_jitter() +
  geom_text(aes(label = Year), hjust = 0.5, vjust = -2.25, size = 4) +
  labs(x = "Completion Percentage", 
       y = "Adjusted Yards per Attempt", 
       title = "Lamar Jackson Regular Season",
       subtitle = "Completion % vs. Adj. Y/A",
       fill = "Position",
       caption = "Graph by Brandon Wisniewski | Data from SportsReference") +
  geom_image(aes(image = img), size = 0.15,
             position = position_jitter(w = -0.25, h = 0)) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0)) +
  theme_classic()

#Filter Play by Play data to find EPA for each playoff game

LJ_pbp_ps <- LJ_pbp %>% 
  filter(season_type == "POST") %>% 
  mutate(game_id = case_when(
    game_id == "2018_18_LAC_BAL" ~ "2018 WC vs LAC",
    game_id == "2019_19_TEN_BAL" ~ "2019 DIV vs TEN",
    game_id == "2020_18_BAL_TEN" ~ "2020 WC @ TEN", 
    game_id == "2020_19_BAL_BUF" ~ "2020 DIV @ BUF"), 
    Quarter = 1 + (3600 - game_seconds_remaining) / 900) %>% 
  select(play_id, game_id, home_team, away_team, game_date, qb_epa, game_seconds_remaining, Quarter) %>% 
  group_by(game_id) %>% 
  ungroup()

ggplot(LJ_pbp_ps, aes(x = Quarter, y = qb_epa)) + 
  geom_area(fill = "#241773") + 
  facet_grid(rows = vars(game_id)) +
  labs(x = "Quarter", 
       y = "Expected Points Added (EPA)", 
       title = "Lamar Jackson EPA by Playoff Game",
       caption = "Graph by Brandon Wisniewski | Data from nflfastR") +
  theme(plot.title = element_text(hjust = 0.5),  
        plot.caption = element_text(hjust = 0.5)) +
  theme_bw()
  
#Add play by play EPA for Hurts and Jones

DJ_pbp_ps <- DJ_pbp %>% 
  filter(season_type == "POST") %>% 
  mutate(game_id = case_when(
    game_id == "2022_19_NYG_MIN" ~ "2022 WC @ MIN",
    game_id == "2022_20_NYG_PHI" ~ "2022 DIV @ PHI"), 
    Quarter = 1 + (3600 - game_seconds_remaining) / 900) %>% 
  select(play_id, game_id, home_team, away_team, game_date, qb_epa, game_seconds_remaining, Quarter) %>% 
  group_by(game_id) %>% 
  ungroup()

ggplot(DJ_pbp_ps, aes(x = Quarter, y = qb_epa)) + 
  geom_area(fill = "#0B2265") + 
  facet_grid(rows = vars(game_id)) +
  labs(x = "Quarter", 
       y = "Expected Points Added (EPA)", 
       title = "Daniel Jones EPA by Playoff Game",
       caption = "Graph by Brandon Wisniewski | Data from nflfastR") +
  theme(plot.title = element_text(hjust = 0.5),  
        plot.caption = element_text(hjust = 0.5)) +
  theme_bw()



JaH_pbp_ps <- JaH_pbp %>% 
  filter(season_type == "POST") %>% 
  mutate(game_id = case_when(
    game_id == "2021_19_PHI_TB" ~ "2021 WC @ TB",
    game_id == "2022_20_NYG_PHI" ~ "2022 DIV vs NYG",
    game_id == "2022_21_SF_PHI" ~ "2022 NFCCG vs SF",
    game_id == "2022_22_KC_PHI" ~ "SB LVII vs KC"), 
    Quarter = 1 + (3600 - game_seconds_remaining) / 900) %>% 
  select(play_id, game_id, home_team, away_team, game_date, qb_epa, game_seconds_remaining, Quarter) %>% 
  group_by(game_id) %>% 
  ungroup()

ggplot(JaH_pbp_ps, aes(x = Quarter, y = qb_epa)) + 
  geom_area(fill = "#046A38") + 
  facet_grid(rows = vars(game_id)) +
  labs(x = "Quarter", 
       y = "Expected Points Added (EPA)", 
       title = "Jalen Hurts EPA by Playoff Game",
       caption = "Graph by Brandon Wisniewski | Data from nflfastR") +
  theme(plot.title = element_text(hjust = 0.5),  
        plot.caption = element_text(hjust = 0.5)) +
  theme_bw()


#Make chart graphing recent Extensions and their Guaranteed Money

signed_qbs <- c("Jalen Hurts", "Daniel Jones", "Derek Carr", "Russell Wilson",
                "Kyler Murray", "Deshaun Watson", "Josh Allen", "Dak Prescott", "Patrick Mahomes")

contract <- c(255000000, 160000000, 150000000, 242588236, 230500000, 230000000,
              258034000, 160000000, 450000000)

gtd <- c(179300000, 92000000, 100000000, 161000000, 159797000, 230000000, 150000000,
         126000000, 141481905)

qb_team_colors <- c("#004C54", "#0B2265", "#D3BC8D", "#FB4F14", "#97233F",
                    "#311D00", "#00338D", "#041E42", "#E31837")

qb_headshots <- c("JaH.png", "DJ.png", "DC.png", "RW.png", "KM.png", "DW.png", 
                  "JA.png", "DP.png", "PM.png")

qb_contracts_df <- data.frame(signed_qbs, contract, gtd, qb_team_colors, qb_headshots)

ggplot(qb_contracts_df, aes(x = signed_qbs, ymax = contract, ymin = gtd)) +
  geom_errorbar(color = qb_team_colors, size = 2.5, width = 0.1) +
  geom_image(aes(x = signed_qbs, y = (contract + gtd) / 2, image = qb_headshots), size = 0.16) +
  scale_y_continuous(labels=scales::comma_format(unit="M")) +
  labs(x = "Quarterbacks", 
       y = "Contract", 
       title = "QB Contracts vs Guaranteed Amount",
       subtitle = "Current QB contracts signed since 2020 that exceed $100 Million",
       caption = "Graph by Brandon Wisniewski | Data from spottrac.com") +
  theme(
        axis.text.x = element_blank()) 

#Compare EPA of current Top Quarterbacks

LJ_epa <- mean(LJ_pbp$qb_epa)
JaH_epa <- mean(JaH_pbp$qb_epa)
JA_epa <- mean(JA_pbp$qb_epa)
PM_epa <- mean(PM_pbp$qb_epa)
JH_epa <- mean(JH_pbp$qb_epa)
JB_epa <- mean(JB_pbp$qb_epa)
RW_epa <- mean(RW_pbp$qb_epa)

QB_epa <- c(LJ_epa, JaH_epa, JA_epa, PM_epa, JH_epa, JB_epa, RW_epa)
rush_ypg <- c(63.4, 42.2, 40.1, 19.3, 13.9, 12.3, 28.7)
qb_headshots2 <- c("LJ.png", "JaH.png", "JA.png", "PM.png", 
                   "JH.png", "JB.png", "RW.png")

epa_df <- data.frame(QB_epa, rush_ypg, qb_headshots2)

ggplot(epa_df, aes(x = QB_epa, y = rush_ypg)) +
  geom_point() +
  geom_image(aes(x = QB_epa, y = rush_ypg, image = qb_headshots2), size = 0.125) +
  labs(x = "Expected Points Added (EPA) per Play", 
       y = "Rushing Yards per Game", 
       title = "EPA/play vs Rush YPG in Quarterbacks",
       caption = "Graph by Brandon Wisniewski | Data from nflfastR and SportsReference") +
  theme_bw()
  

