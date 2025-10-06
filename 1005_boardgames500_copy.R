library(ggplot2)

# Load dataset ------------------------------------------------------------

## Chargement des données ----
boardgames <- read.csv(
  file = "data/bg_extract.csv",
  header = FALSE,
  col.names = c(
    "name",
    "min_age",
    "min_players",
    "max_players",
    "game_duration_minutes",
    "editor",
    "category"
  ),
  colClasses = c("character",
                 "numeric",
                 "numeric",
                 "numeric",
                 "numeric",
                 "factor",
                 "factor"),
  sep = ",",
  encoding = "UTF-8"
)

boardgames_palette <- c(
  "Jeux Experts" = "#FF3000",  
  "Jeux Initiés" = "#2FCDCD",  
  "Jeux Famille" = "#FFCD00",  
  "Jeux Enfants" = "#9ACD00"  
)

str(boardgames)
summary(boardgames)

## Transformation des variables ----

# On fixe le nombre de joueurs à 20 lorsqu'il ne sont pas renseignés
boardgames[is.na(boardgames$max_players),]
boardgames$max_players <- ifelse(is.na(boardgames$max_players), 20, boardgames$max_players)

# On retire les jeux dont la durée de jeu est 0 5extensions de jeux)
boardgames[boardgames$game_duration_minutes == 0, ]
boardgames <- boardgames[boardgames$game_duration_minutes > 0, ]

# On regroupe les catégories "Puzzle Adulte" et "Jeux Experts" car elle ne contient qu'une modalité
boardgames[boardgames$category == "Puzzle Adulte", ]$category <- "Jeux Experts"
boardgames$category <- factor(x = boardgames$category,
                              levels = c("Jeux Enfants", "Jeux Famille", "Jeux Initiés", "Jeux Experts"),
                              ordered = T)





# Plot : Temps de jeu vs age minimun --------------------------------------
selected_games <- boardgames %>%
  filter(name %in% c("Codex Naturalis") | game_duration_minutes == 180)

ggplot(boardgames,
       aes(x = game_duration_minutes, y = min_age, colour = category)) +
  geom_point(size = 4) +
  # Codex Naturalis
  annotate(
    geom = "text",
    x = 20,
    y = 5,
    size = 2,
    label = "Codex Naturalis",
    color = "black"
  ) +
  annotate(
    "segment",
    xend = 25 - 0.1,
    yend = 6 - 0.1,
    x = 20,
    y = 5.2,
    arrow = arrow(type = "closed", length = unit(0.01, "npc"))
  ) +
  #  Bureau of Investigation : Enquêtes à Arkham
  annotate(
    geom = "text",
    x = 150,
    y = 17,
    size = 2,
    label = "Bureau of Investigation : Enquêtes à Arkham",
    color = "black"
  ) +
  annotate(
    "segment",
    xend = 180 - 0.1,
    yend = 16 + 0.1,
    x = 170,
    y = 16.8,
    arrow = arrow(type = "closed", length = unit(0.01, "npc"))
  ) +
  scale_color_manual(values = boardgames_palette) +
  scale_x_continuous(
    name = "Temps de jeu (en minutes)",
    breaks = c(0, 10, 30, 45, 60, 90, 120, 180),
    minor_breaks = NULL,
    limits = c(0, 200),
    expand = c(0, 0)
  ) +
  scale_y_continuous(name = "Age minimum",
                     breaks = seq(0, 18, by = 2),
                     minor_breaks = NULL) +
  labs(title = "Durée des jeux de société par âge minimum", 
       caption = "500 popular boardgames") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank(), 
        panel.grid.major = element_line(color = "#222", linewidth = 0.5),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5))



# Tidytuesday version -----------------------------------------------------
library(stringr)
#source: https://www.kaggle.com/datasets/jvanelteren/boardgamegeek-reviews/version/3
caption_boadgmes <- "BoardgamesGeek Reviews"

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-01-25/details.csv')


str(ratings)
summary(ratings)

total_bg <- ratings %>% 
  merge(details, by = "id")



categories <- total_bg$boardgamecategory %>% 
  str_remove_all("\\[|\\]|\\'|\"") %>% 
  str_split(",") %>% 
  unlist()


ggplot(total_bg,
       aes(x = playingtime, y = minage, size = bayes_average)) +
  geom_point() +
  labs(title = "Are short games the populest?",
       caption = caption_boadgmes) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
  )

