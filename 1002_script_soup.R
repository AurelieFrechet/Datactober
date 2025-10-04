# install.packages("tastyR")
library(tastyR)
library(stringr)
library(dplyr)


# Clean data --------------------------------------------------------------

soups_names <- grep(pattern = "soup", x = tolower(allrecipes$name), value = TRUE)

soups_recipes <- allrecipes[tolower(allrecipes$name) %in% soups_names, ]


ingredients_bruts <- soups_recipes$ingredients %>% 
  str_to_lower() %>% 
  paste(collapse = " ") %>% 
  str_replace_all(pattern = "\\band\\b", replacement = " , ") %>%
  str_split(pattern = ",") %>% 
  unlist() 

## Stop words
measures <- c("cup(s)?", "tablespoon(s)?", "teaspoon(s)?", "ounce(s)?", "pound(s)?", 
              "pieces", "(p)?inch(s)?", "slice(s", "d)?", "half", "allpurpose", "large", 
              "bag(s)?", "clove(s)?", "chunk(s)", "baby", "bunch", "small", "package",
              "bitesize", "cub(e)?(s)?", "lengthwise")
adjectives <- c("thinly", "finely", "fresh(ly)?", "roughly","thick(ly)?")
expressions <- c("or", "more", "to", "taste", "as", "needed", "optional", "in", "hal(f|ves)", 
                 "into", "about", "if")
verbs <- c("chopped", "diced", "shred(ded)?", "sliced", "crushed", "grated", "peeled", 
           "minced", "divided", "rinsed", "cooked", "(un)?drained", "ground", "dried", 
           "pressed", "soaked", "cut", "cubed", "seeded", "cored", "halved", "juiced", "quartered", 
           "thawed", "uncooked", "deveined", "toasted", "baked", "crumbled", "softened",
           "skinless")

stop_words <- paste0("\\b", c(measures, adjectives, expressions, verbs), "\\b")

ingredients_net <- ingredients_bruts %>% 
  str_remove_all(pattern = "[0-9]*") %>% 
  str_remove_all(pattern = "[^[:alnum:]?!\\s]") %>% 
  str_remove_all(pattern = paste(stop_words, collapse = "|")) %>% 
  str_replace_all(pattern = "\\s+", replacement = " ") %>% 
  str_trim()


ingredients <- ingredients_net %>%
  str_replace_all("ves\\b", "f") %>%
  str_replace_all("es\\b", "e") %>%
  str_replace_all("oes\\b", "o") %>%
  str_replace_all("s\\b", "") %>%
  str_replace_all("\\bchile\\b", "chili")

stop_words <- c(
  "or", "more", "to", "into", "and", ""
)

soup_ingredient <- ingredients[!(ingredients %in% c(""))]

word_soups <- data.frame(table(soup_ingredient))

color_freq <- colorRampPalette(colors = c("#FFEF00", "#8B0000"))
color_gradient <- data.frame(Freq = 1:max(word_soups$Freq),
                             color = color_freq(max(word_soups$Freq)))
word_soups %>% 
  arrange(-Freq) %>% 
  head(20)

word_soups <- word_soups %>% 
  arrange(-Freq) %>% 
  merge(color_gradient) %>% 
  filter(Freq > 5)

# Categories --------------------------------------------------
herbs <- c("basil", "chives", "marjoram", "oregano", "parsley", "rosemary",
           "tarragon", "thyme")
spices <- c("allspices", "pepper", "cinnamon", "cumin", "curry", "chili", "ginger",
            "nutmeg", "paprika", "salt", "turmeric")
meat <- c("chicken", "beef")

word_soups$color[grepl(paste(spices, collapse = "|"), word_soups$soup_ingredient)] <- "#e4a02d"
word_soups$color[grepl(paste(herbs, collapse = "|"), word_soups$soup_ingredient)]  <- "#068a69"
word_soups$color[grepl(paste(meat, collapse = "|"), word_soups$soup_ingredient)]   <- "#975d5d"
word_soups$color[grepl("mushroom", word_soups$soup_ingredient)] <- "#999"

cols <- unique(word_soups$color)
names(cols) <- cols

# Cloud word --------------------------------------------------------------
##wordcloud2 ----
library(wordcloud2) 

# have a look to the example dataset
# head(demoFreq)

# Basic plot
wordcloud2(
  data = word_soups[, c("soup_ingredient", "Freq")],
  size = 1,
  shape = 'cardiod',
  maxRotation = 0,
  minRotation = 0,
  color = word_soups$color
)

## ggwordcloud ----
library(ggwordcloud)


word_soups %>%
  arrange(-Freq) %>%
  ggplot(aes(
    label = soup_ingredient,
    size = Freq,
    color = color
  )) +
  geom_text_wordcloud(area_corr = TRUE,
                      rm_outside = TRUE,
                      mask = png::readPNG("data/icons_soup.png")) +
  scale_size_area(max_size = 40) +
  # scale_color_gradient(low = "#ff8d00", high = "#c36728") 
  scale_color_manual(name = "", values = cols) + 
  labs(title = "Soup of the Day",
       subtitle = "Most popular soup's ingredients",
       caption = "tastyR package - Allrecipes.com") +
  theme(plot.background = element_rect(fill = "#222", colour = NULL),
        panel.background = element_rect(fill = "#222"),
        plot.title = element_text(family = "Freestyle Script", size = 46, colour = "white", hjust = 0.5),
        plot.subtitle = element_text(size = 16, colour = "white", hjust = 0.5),
        plot.caption = element_text(colour = "white", face = "italic"))

