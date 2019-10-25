colnames(fielders)
colors <- c("Y" = "orange2",
            "N" = "blue2")

# Create Plot Function
plotfunc <- function(var1){
  hist <- ggplot(data = fielders, aes_string(x = var1, fill = "inducted")) +
    geom_histogram() +
    scale_fill_manual(values = colors)
  box <- ggplot(data = fielders, aes_string(x = "inducted", y = var1, fill = "inducted")) +
    geom_boxplot() +
    coord_flip() +
    scale_fill_manual(values = colors)
  grid.arrange(box, hist)
}

# ---------- Games Variable
ggplot(data = fielders, aes(x = inducted, y = games, fill = inducted)) +
  stat_summary(fun.y = "mean", geom = "bar") +
  scale_fill_manual(values = colors)

plotfunc("games")

fielders %>% 
  filter(inducted == "Y" & games < 1000) %>% 
  select(name, games) %>% 
  arrange(games)

table(voting$votedBy)

# Exclude Negro League Players
negroleague <- voting %>% 
  filter(votedBy == "Negro League")

fielders <- fielders %>% 
  filter(!(playerID %in% negroleague$playerID))

plotfunc("games")

fielders %>% 
  filter(games > 3000) %>% 
  select(playerID, name, games, inducted)

# Exclude Pete Rose - Ineligible For Hall of Fame
fielders <- fielders[fielders$playerID != "rosepe01", ]

# ---------- Hits Variable
plotfunc("hits")

fielders %>% 
  group_by(inducted) %>% 
  summarize(mean(hits))

fielders %>% 
  filter(hits > 2500 & inducted == "N") %>% 
  select(name, hits) %>% 
  arrange(desc(hits))

# Mitchell Report Players
mitchell_report <- read.csv("mitchell report players.csv")

fielders <- fielders %>%
  mutate(mitchell = if_else(name %in% mitchell_report$Player_Name, "Y", "N"))

fielders$mitchell <- as.factor(fielders$mitchell)

fielders %>% 
  filter(hits > 2500 & inducted == "N") %>% 
  select(name, hits, mitchell) %>% 
  arrange(desc(hits))

# ---------- Mitchell
fielders %>% 
  filter(mitchell == "Y") %>% 
  ggplot(aes(x = inducted, fill = inducted)) +
  geom_bar() +
  scale_fill_manual(values = colors)

# Temporarily Exclude Players w/ < 1000 Games
# Temporarily Exclude Players in Mitchell Report
fielders_full <- fielders

fielders <- fielders %>% 
  filter(games > 1000)

fielders <- fielders %>% 
  filter(mitchell == "N")

# ---------- Home Runs
plotfunc("home_runs")

fielders %>% 
  filter(home_runs > 500 & inducted == "N") %>% 
  select(name, home_runs)

# ---------- Average Variable
plotfunc("average")

fielders %>% 
  ggplot(aes(x = average, fill = inducted)) +
  geom_histogram() +
  scale_fill_manual(values = colors)

fielders %>% 
  filter(average > 0.35 & inducted == "N" & games > 1000) %>% 
  select(playerID, name)

# Exclude Shoeless Joe Jackson - Ineligible For Hall of Fame
fielders <- fielders[fielders$playerID != "jacksjo01", ]

fielders %>% 
  filter(average > 0.325 & inducted == "N") %>% 
  select(playerID, name, games, hits)

fielders %>% 
  ggplot(aes(x = games, y = average, color = inducted)) +
  geom_point(aes(color = inducted), size = 2) +
  scale_color_manual(values = colors) +
  geom_smooth()