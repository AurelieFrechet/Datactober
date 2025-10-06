library(dplyr)
library(readxl)
library(ggplot2)
library(maps)
library(stringi)

#source: https://www.data.gouv.fr/datasets/interventions-realisees-par-les-services-d-incendie-et-de-secours/
years <- seq(2014, 2023)
interventions_list <- lapply(years, function(year) {
  df <- read_excel(paste0("data/interventions_pompiers/interventions", year, ".xlsx"))
  df <- df[, 1:8]
  colnames(df) <- c(
    "annee",
    "zone",
    "region",
    "numero",
    "departement",
    "categorie",
    "feux_habitations_bureaux",
    "dont_cheminees"
  )
  return(df)
})

interventions <- do.call(rbind, interventions_list)
str(interventions)

interventions <- interventions %>% 
  mutate(annee  = as.numeric(annee),
         zone   = as.factor(stri_trans_general(str = zone, id = "Latin-ASCII")),
         region = as.factor(stri_trans_general(str = region, id = "Latin-ASCII")),
         numero = as.factor(numero),
         departement = as.factor(stri_trans_general(str = departement, id = "Latin-ASCII")),
         categorie   = as.factor(categorie),
         feux_habitations_bureaux = as.numeric(feux_habitations_bureaux),
         dont_cheminees = as.numeric(dont_cheminees)) 

summary(interventions)

interventions$id <- gsub(pattern = "\\W", replacement = "", x= tolower(interventions$departement))


## Time serie par région ----------------------------------------------------------

interventions %>% 
  group_by(annee, zone) %>% 
  summarise(nb_cheminee = sum(dont_cheminees)) %>% 
ggplot(aes(x=annee, y =nb_cheminee, colour = zone)) +
  geom_line()

interventions %>% 
  filter(region == "Bretagne") %>% 
  ggplot(aes(x=annee, y =dont_cheminees, colour = departement)) +
  geom_line()

interventions %>% 
  filter(annee== 2023) %>% 
  group_by(zone, region) %>% 
  summarise(nb_cheminee = sum(dont_cheminees)) %>% 
  ggplot(aes(x = nb_cheminee, y = reorder(region, nb_cheminee), fill = zone)) +
  geom_col()


# Ajout densité par région ------------------------------------------------

population <- read_delim("data/population.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
population$region <- stri_trans_general(str = population$Géographie, id = "Latin-ASCII")
population$id <- gsub(pattern = "\\W", replacement = "", x= tolower(population$region))

interventions_2023 <- 
  interventions %>% 
  filter(annee == 2023) %>% 
  group_by(id, zone, region, departement) %>% 
  summarise(feux = sum(feux_habitations_bureaux, na.rm = T),
            cheminee = sum(dont_cheminees, na.rm=T))

setdiff(population$id, interventions$id)

population$id[population$id %in% c("seinesaintdenis", "hautsdeseine", "valdemarne")] <- "paris"

population <- population %>% 
  group_by(id) %>% 
  summarise(pop = sum(Valeur, na.rm = T))

interventions_2023 <- interventions_2023 %>% 
  left_join(population) %>% 
  mutate(ratio_cheminee = cheminee/pop)


# Map pour 2023 -------------------------------------------------------------------
carte_france <- map_data('france') 
carte_france$id <- gsub(pattern = "\\W", replacement = "", x= tolower(carte_france$region))

setdiff(unique(carte_france$id), unique(interventions_2023$id)) %>% sort()
setdiff(unique(interventions_2023$id), unique(carte_france$id)) %>% sort()

## décupler les lignes petite couronne
hautsdeseine <- interventions_2023[interventions_2023$id == "paris", ]
hautsdeseine$id <- "hautsdeseine"
seinesaintdenis <- interventions_2023[interventions_2023$id == "paris", ]
seinesaintdenis$id <- "seinesaintdenis"
valdemarne <- interventions_2023[interventions_2023$id == "paris", ]
valdemarne$id <- "valdemarne"

interventions_2023 <- rbind(interventions_2023, hautsdeseine, seinesaintdenis, valdemarne)

base_carte <- carte_france %>% 
  select("long", "lat", "group", "order", "id") %>% 
  left_join(interventions_2023, by = "id")

base_carte %>% 
ggplot(aes(x = long, y = lat, group = group, fill = ratio_cheminee)) +
  geom_polygon(color = '#555555', linewidth = .5) +
  coord_map() +
  scale_fill_distiller(name = 'Feux de cheminée\npar habitant',
                       palette = "YlOrRd", direction = 1)+
  labs(title = "Nombre d'incendies de cheminées par habitants pour l'année 2023",
       caption = "Interventions des sapeurs-pompiers - data.gouv") +
  theme_void() +
  theme(plot.caption.position = "plot")


