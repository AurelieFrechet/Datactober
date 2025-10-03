# install.packages("tastyR")
library(tastyR)
library(stringr)


# Clean data --------------------------------------------------------------

soups_names <- grep(pattern = "soup", x = tolower(allrecipes$name), value = TRUE)

soups_recipes <- allrecipes[tolower(allrecipes$name) %in% soups_names, ]


ingredients <- soups_recipes$ingredients %>% 
  str_to_lower() %>% 
  paste(collapse = " ") %>% 
  str_replace_all(pattern = " and ", replacement = " , ") %>%
  str_split(pattern = ",") %>% 
  unlist() %>% 
  str_remove_all(pattern = "[0-9]*") %>% 
  str_remove_all(pattern = "[^[:alnum:]?!\\s]") %>% 
  str_remove_all(pattern = "cup(s)?|tablespoon(s)?|teaspoon(s)?|ounce(s)?|pound(s)?|pieces|(p)?inch(s)?|slice(s|d)?|half|allpurpose|large|bag|clove(s)?|chunk(s)|baby|bunch") %>% 
  str_remove_all(pattern = "chopped|diced|shred(ded)?|sliced|crushed|grated|peeled|minced|divided|rinsed|cooked|(un)?drained|ground|dried|pressed|soaked| cut |cubed|seeded|cored|halved|juiced|quartered|thawed") %>% 
  str_remove_all(pattern = "thinly|finely|fresh(ly)?|or more|into ?.*|(or )?to taste|as needed|optional|in half|roughly|thick(ly)?") %>%
  str_replace_all("chile", "chili") %>% 
  str_replace_all("onion(s)?", "onions") %>% 
  str_replace_all("egg(s)?", "eggs") %>% 
  str_replace_all("potato(es)?", "potato") %>% 
  str_replace_all("tomato(es)?", "tomato") %>% 
  str_replace_all(".* ?carrot(s)?", "carrots") %>%
  str_replace_all("leek(s)?", "leeks") %>%
  str_replace_all("stalk(s)? celery", "stalks celery") %>%
  str_replace_all(".* ?water ?.*", "water") %>%
  str_replace_all(".* ?beef ?.*", "beef") %>%
  str_replace_all(".* ?chicken ?.*", "chichen") %>%
  str_replace_all(".* ?butter", "butter") %>%
  str_replace_all(pattern = "\\s+", replacement = " ") %>% 
  str_trim()

stop_words <- c(
  "or", "more", "to", "into", "and", ""
)

soup_ingredient <- soup_ingredient[!(soup_ingredient %in% stop_words)]

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

word_soups$color[grepl(paste(spices, collapse = "|"), word_soups$soup_ingredient)] <- "#d07335"
word_soups$color[grepl(paste(herbs, collapse = "|"), word_soups$soup_ingredient)] <- "#068a69"
word_soups$color[grepl("mushroom", word_soups$soup_ingredient)] <- "#999"

cols <- unique(word_soups$color)
names(cols) <- cols

# Cloud word --------------------------------------------------------------

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
  scale_size_area(max_size = 50) +
  # scale_color_gradient(low = "#ff8d00", high = "#c36728") 
  scale_color_manual(name = "", values = cols) + 
  labs(title = "Soup of the Day",
       subtitle = "Most popular soup's indregients",
       caption = "tastyR package - Allrecipes.com") +
  theme(plot.background = element_rect(fill = "#222"),
        panel.background = element_rect(fill = "#222"),
        plot.title = element_text(colour = "white", hjust = 0.5),
        plot.caption = element_text(colour = "white", face = "italic"))

