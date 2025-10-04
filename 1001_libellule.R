library(dplyr)
library(ggplot2)
library(stringr)



# Data Prepocessing -------------------------------------------------------

# source: http://dragonfly-database.eu/

## Load data
dragonfly_data <- read.csv("data/dragonfly_database-data-2025-10-01_20-19-46.csv",
                           sep = ";",
                            dec = ",", 
                           na.strings = "-")

## Remove colnames with .source
wrong_colnames <- grepl(x = colnames(dragonfly_data), pattern = ".source")
dragonfly_data <- dragonfly_data[, !wrong_colnames] 


## Replace . characters
colnames(dragonfly_data) <- colnames(dragonfly_data) %>% 
  str_to_lower() %>% 
  str_replace_all(pattern = "\\.+", replacement = "_") %>% 
  str_remove(pattern = "_$")

str(dragonfly_data)



# Plot: Abmonen length ----------------------------------------------------

male_abdomen <- dragonfly_data %>% 
  select(family, species, abdomen_length_min_m, abdomen_length_max_m) %>% 
  rename("abdomen_length_min" = "abdomen_length_min_m",
         "abdomen_length_max" = "abdomen_length_max_m") %>% 
  mutate(sex = "male")
female_abdomen <- dragonfly_data %>% 
  select(family, species, abdomen_length_min_f, abdomen_length_max_f) %>% 
  rename("abdomen_length_min" = "abdomen_length_min_f",
         "abdomen_length_max" = "abdomen_length_max_f") %>% 
  mutate(sex = "female")

dragonflies_abdomen <- rbind(male_abdomen, female_abdomen)

dragonfly_data %>%
  filter(!is.na(abdomen_length_min_m) &
           !is.na(abdomen_length_max_m)) 

dragonflies_abdomen %>% 
  filter(!is.na(abdomen_length_min) &
           !is.na(abdomen_length_max)) %>% 
  mutate(sex = factor(sex, levels = c("female", "male"))) %>% 
  ggplot() +
  # Mal abmonen length
  geom_segment(aes(x = abdomen_length_min, xend = abdomen_length_max, y = species, yend = species, col = sex, linetype = sex), size =1) +
  geom_point(aes(x = abdomen_length_min, y = species, col = sex, shape = sex), size = 3) +
  geom_point(aes(x = abdomen_length_max, y = species, col = sex, shape = sex), size = 3) +
  facet_grid(rows = vars(family), 
             # cols = vars(sex),
             scales = "free_y",
             space = "free_y",
             switch = "y") +
  scale_x_continuous(name = "Abdomen length", breaks = seq(30, 80, by = 10), minor_breaks = seq(30, 80, by =1)) +
  scale_color_manual(name = "Sex", values = c("#14C4D8", "#D82914")) +
  scale_linetype_manual(name = "Sex", values = c("dashed", "solid")) +
  scale_shape_manual(name = "Sex", values = c(18, 16)) +
  labs(title = "Female vs Male Dragonflies Abdomen",
       caption = " Dragonfly Database") +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "#333", linewidth = 0.5, linetype = "solid"),
    panel.grid.minor.x = element_line(color = "#666", linewidth = 0.5, linetype = "dotted"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(0, 0, 10, 0)),
    axis.title = element_blank(),
    strip.placement = 'outside',
    strip.background = element_rect(
      fill = "#FDFCFA",
      color = "#1B1B1B",
      linewidth = 1
    ),
    legend.position = "bottom"
  )
