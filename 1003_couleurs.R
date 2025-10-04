library(data.table)
library(treemapify)
library(treemap)

# Load data ---------------------------------------------------------------

answers <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/answers.csv')
color_ranks <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/color_ranks.csv')


color_ranks[color == "brown"]

pixel_chart <- function(color = "pink") {
  selected_color <- color_ranks[color == graph_color]
  
  all_color <- answers[rank == selected_color$rank, .(freq = .N), by=hex]
  
  
  nrow(all_color)
  vmax <- floor(sqrt(nrow(all_color))-1)
  # vmax <- 500
  
  grid_color <- cbind(expand.grid(1:vmax, 1:vmax), color = sample(all_color$hex, vmax^2))
  
  cols <- grid_color$color
  names(cols) <- grid_color$color
  
  grid_color %>% 
    ggplot(aes(x=Var1, y=Var2, fill = color)) +
    geom_tile()+
    coord_fixed() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_manual(values = cols, guide="none") +
    labs(title = paste0("Is this ", graph_color, "?"),
         subtitle = paste0("Colors perceived as ", graph_color, " by users"),
         caption = "XKCD Color Survey Results") +
    theme_void() +
    theme(plot.title = element_text(colour = selected_color$hex, face = "bold", size = 20, hjust = 0.5),
          plot.subtitle = element_text(colour = "black", size = 14, hjust = 0.5))
  
}

pink_chart <- pixel_chart(graph_color = "pink")
purple_chart <- pixel_chart(graph_color = "purple")
blue_chart <- pixel_chart(graph_color = "blue")
green_chart <- pixel_chart(graph_color = "green")
