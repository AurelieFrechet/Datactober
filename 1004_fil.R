library(dplyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)

# Load data ---------------------------------------------------------------


yarn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-10-11/yarn.csv')
yarn_attributes <- readr::read_csv("https://raw.githubusercontent.com/awalsh17/ravelry_yarns/main/data/yarn_attributes.csv")
yarn_fibers <- readr::read_csv("https://raw.githubusercontent.com/awalsh17/ravelry_yarns/main/data/yarn_fibers.csv")

# write.csv(yarn, "data/yarn.csv")
# write.csv(yarn_attributes, "data/yarn_attributes.csv")
# write.csv(yarn_fibers, "data/yarn_fibers.csv")

str(yarn)
summary(yarn)


# Deal with strange values ------------------------------------------------

yarn <- yarn %>% 
  mutate(yarn_weight_name  = as.factor(ifelse(yarn_weight_name == "No weight specified", NA, yarn_weight_name)),
         yardage = ifelse(yardage == 0, NA, yardage),
         grams = ifelse(grams == 0, NA, grams))


# Remove 7kg yarn :
yarn <- yarn %>% 
  filter(grams != max(grams, na.rm=TRUE))


ggplot(yarn, aes(x=grams, y=yardage)) + geom_point()
## Yarn weight name per yardage
#nombre de mètre de la boule de laine par catégorie du poids de la laine
ggplot(yarn, aes(x = yardage, y = yarn_weight_name)) +
  geom_violin()

ggplot(yarn, aes(x = grams, y = yarn_weight_name)) +
  geom_boxplot()

ggplot(yarn, aes(x = min_gauge, y = yarn_weight_name)) +
  geom_boxplot()

# min gauge/ max gauge per weight name

## yarn compagny name tendances weight yarn



# Associated fibers -------------------------------------------------------
sub_yarns <- split(yarn_fibers, yarn_fibers$id)

list_df <- lapply(sub_yarns, function(df){
  if(nrow(df) == 1){
    data.frame(
      from = df$fiber_type_name,
      to = df$fiber_type_name
    )
  } else {
    data.frame(
      from = df$fiber_type_name[1:(nrow(df) -1)],
      to = df$fiber_type_name[2:nrow(df)]
    )
  }
})

links_df <- do.call(rbind, list_df)

edges <- links_df %>% 
  group_by(from, to) %>% 
  summarise(n = n())

vertices <- yarn_fibers %>% 
  group_by(name = fiber_type_name) %>% 
  summarise(value = n())

matrice_links <- edges %>% 
  pivot_wider(id_cols = from, names_from = to, values_from = n)

## Edges plot----
# source: https://r-graph-gallery.com/311-add-labels-to-hierarchical-edge-bundling.html


vertices$group  <-  edges$from[ match( vertices$name, edges$to ) ]

vertices$id <- NA
myleaves <- which(is.na( match(vertices$name, edges$from) ))
nleaves <- length(myleaves)
vertices$id[ myleaves ] <- seq(1:nleaves)
vertices$angle <- 90 - 360 * vertices$id / nleaves

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust <- ifelse( vertices$angle < -90, 1, 0)

# flip angle BY to make them readable
vertices$angle <- ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)


# Create a graph object
mygraph <- igraph::graph_from_data_frame( edges, vertices=vertices )

from  <-  match( connect$from, vertices$name)
to  <-  match( connect$to, vertices$name)

# Basic usual argument
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", width=0.9)

## Heatmap ----
ggplot(edges, aes(x=from, y=to, fill=n)) +
  geom_tile()

## Chorddiag -----
library(chorddiag)

fiber_names <- matrice_links$from
m <- as.matrix(matrice_links[, -1])

dimnames(m) <- list(have = fiber_names,
                    prefer = fiber_names)
chorddiag(m, palette = "Pastel2")
