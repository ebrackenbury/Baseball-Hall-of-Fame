# Load Libraries
library(Lahman)
library(tidyverse)
library(randomForest)
library(gridExtra)

# Extract Data Sets From Lahman Package
voting <- Lahman::HallOfFame
people <- Lahman::People

# Fix
voting$category[voting$playerID == "griffcl01"] <- "Pioneer/Executive"

# Get Inducted Players
inducted_players <- voting %>% 
  filter(category == "Player" & inducted == "Y")

# Create Name Variable
people$name <- paste(people$nameFirst, people$nameLast)

# Exclude Non-Players
players <- people %>%
  filter(is.na(bats) == FALSE & is.na(debut) == FALSE)

# Exclude Newly Retired Players
eligible <- players %>%
  filter(finalGame < "2013-01-01")

# Get Position
player_position <- Fielding %>%
  group_by(playerID, POS) %>% 
  summarize(games = sum(G)) %>% 
  slice(which.max(games)) %>% 
  arrange(desc(games))

# Add Position and Convert to Factor
eligible <- merge(eligible, player_position[c("playerID", "POS")], by.x = "playerID")
names(eligible)[names(eligible) == "POS"] <- "position"
eligible$position <- as.factor(eligible$position)

# Separate Hitters and Pitchers
pitchers <- eligible %>% 
  filter(position == "P")

fielders <- eligible %>% 
  filter(position != "P")

# Aggregate Batting Statistics
batting_stats <- Batting %>%
  group_by(playerID) %>% 
  summarize(batting_seasons = length(unique(yearID)),
            games = sum(G),
            at_bats = sum(AB),
            runs = sum(R),
            hits = sum(H),
            singles = sum(H-(X2B + X3B + HR)),
            doubles = sum(X2B),
            triples = sum(X3B),
            home_runs = sum(HR),
            rbis = sum(RBI, na.rm = TRUE),
            stolen_bases = sum(SB),
            caught_stealing = sum(CS),
            walks = sum(BB),
            strikeouts = sum(SO),
            int_walks = sum(IBB),
            hbp = sum(HBP),
            sac_hit = sum(SH),
            sac_fly = sum(SF),
            gidp = sum(GIDP))

# Create New Batting Variables
batting_stats <- batting_stats %>% 
  mutate(average = hits/at_bats,
         on_base = (hits + walks + hbp)/(at_bats + walks + hbp + sac_fly),
         slugging = (singles + 2*doubles + 3*triples + 4*home_runs)/at_bats,
         ops = on_base + slugging)

# Aggregate Pitching Statistics
# pitching_stats <- Pitching %>% 
#                     group_by(playerID) %>% 
#                     summarize(pitching_seasons = length(unique(yearID)),
#                     wins = sum(W),
#                     losses = sum(L),
#                     games = sum(G),
#                     complete_games = sum(CG),
#                     shutouts = sum(SO),
#                     saves = sum(SV),
#                     innings = sum(IPouts/3),
#                     hits = sum(H),
#                     earned_runs = sum(ER),
#                     home_runs = sum(HR),
#                     walks = sum(BB),
#                     strikeouts = sum(SO),
#                     int_walks = sum(IBB),
#                     wild_pitches = sum(WP),
#                     hbp = sum(HBP),
#                     balks = sum(BK),
#                     batters_faced = sum(BFP),
#                     games_finished = sum(GF),
#                     runs = sum(R),
#                     sac_hits = sum(SH),
#                     sac_fly = sum(SF),
#                     gidp = sum(GIDP))

# Create New Pitching Variables
# pitching_stats <- pitching_stats %>% 
#                     mutate(era = earned_runs*9/innings,
#                     whip = (walks + hits)/innings)

# Add Position
fielders <- merge(fielders, batting_stats, by.x = "playerID")

# Add Inducted
fielders <- merge(fielders, inducted_players[c("playerID", "inducted")], by.x = "playerID", all.x = TRUE)

# Clean Up Inducted
fielders <- fielders %>% 
  mutate(inducted = if_else(is.na(inducted), "N", "Y"))

fielders$inducted <- as.factor(fielders$inducted)