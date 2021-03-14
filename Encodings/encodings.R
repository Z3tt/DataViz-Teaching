library(tidyverse)
library(palmerpenguins)
library(patchwork)
library(ragg)

theme_set(theme_light(base_family = "Roboto", base_size = 15))
theme_update(
  legend.text = element_text(size = 15),
  legend.title = element_text(size = 15),
  legend.position = "top",
  plot.margin = margin(rep(20, 4)),
  panel.grid.minor = element_blank()
)

## shape vs color encoding
a1 <- ggplot(penguins, aes(bill_length_mm, bill_depth_mm, shape = species)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(1, 2, 5), name = "Species:") +
  labs(x = "Bill Length (mm)", y = "Bill Depth (mm)") +
  theme(plot.background = element_rect(size = 2, color = "#D1AF07"))

a2 <- ggplot(penguins, aes(bill_length_mm, bill_depth_mm, color = species)) +
  geom_point(size = 3, alpha = .6) +
  scale_color_brewer(palette = "Dark2", name = "Species:") +
  labs(x = "Bill Length (mm)", y = "Bill Depth (mm)") +
  theme(plot.background = element_rect(size = 2, color = "#28a87d"))

a1 + plot_spacer() + a2 + plot_layout(nrow = 1, widths = c(1, .02, 1))

ggsave(here::here("Encodings", "encoding_scatter.png"), 
       width = 8000, height = 4000, limitsize = FALSE, 
       res = 500, unit = "in", device = agg_png)

## combined solution
a3 <- ggplot(penguins, aes(bill_length_mm, bill_depth_mm, 
                           fill = species, shape = species)) +
  geom_point(size = 3, alpha = .8) +
  scale_fill_brewer(palette = "Dark2", name = "Species:") +
  scale_shape_manual(values = c(21, 23, 24), name = "Species:") +
  labs(x = "Bill Length (mm)", y = "Bill Depth (mm)") +
  theme(plot.background = element_rect(size = 2, color = "#28a87d"))

ggsave(here::here("Encodings", "encoding_scatter_combo.png"), 
       width = 4000, height = 4000, limitsize = FALSE, 
       res = 500, unit = "in", device = agg_png)


## bars vs circles (mapped to area)
b1 <- 
  penguins %>% 
  count(species) %>% 
  mutate(species = fct_reorder(species, n),
         species_num = as.numeric(species),
         species_num = if_else(species == "Gentoo", species_num - .09, species_num)) %>% 
  ggplot(aes(species_num, 1)) + 
  geom_point(aes(size = n), shape = 21, color = "#D1AF07", 
             fill = "#E9C642", stroke = 2, show.legend = FALSE) +
  geom_text(aes(label = species), family = "Roboto", color = "white",
            size = 7, fontface = "bold") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(.2, .2), limits = c(1, 3.2)) +
  scale_size_area(max_size = 70) +
  labs(x = NULL, y = NULL) +
  theme(plot.background = element_rect(size = 2, color = "#D1AF07"),
        panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(color = NA))

b2 <- penguins %>% 
  count(species) %>% 
  mutate(species = fct_reorder(species, n)) %>% 
  ggplot(aes(species, n)) + 
  geom_col(width = .8, color = "#28a87d", fill = "#57C79C", size = 1.6) +
  geom_text(aes(y = 0, label = species), family = "Roboto", color = "white",
            hjust = 0, nudge_y = 5, size = 7, fontface = "bold") +
  coord_flip() +
  scale_x_discrete(expand = c(.23, .23)) +
  scale_y_continuous(expand = c(.01, .01)) +
  labs(x = NULL, y = NULL) +
  theme(plot.background = element_rect(size = 2, color = "#28a87d"),
        panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(color = NA))

b1 + plot_spacer() + b2 + plot_layout(nrow = 1, widths = c(1, .02, 1))

ggsave(here::here("Encodings", "encoding_amount.png"), 
       width = 8000, height = 2000, limitsize = FALSE, 
       res = 500, unit = "in", device = agg_png)


## bars vs circles (mapped to radius)
b1r <- 
  penguins %>% 
  count(species) %>% 
  mutate(species = fct_reorder(species, n),
         species_num = as.numeric(species),
         species_num = if_else(species == "Gentoo", species_num - .17, species_num)) %>% 
  ggplot(aes(species_num, 1)) + 
  geom_point(aes(size = n), shape = 21, color = "#D1AF07", 
             fill = "#E9C642", stroke = 2, show.legend = FALSE) +
  geom_text(aes(label = species), family = "Roboto", color = "white",
            size = 6, fontface = "bold") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(.2, .2), limits = c(1, 3.2)) +
  #scale_size_area(max_size = 70) +
  scale_radius(range = c(0, 80), limits = c(0, NA)) +
  labs(x = NULL, y = NULL) +
  theme(plot.background = element_rect(size = 2, color = "#D1AF07"),
        panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(color = NA))

b1r + plot_spacer() + b2 + plot_layout(nrow = 1, widths = c(1, .02, 1))

ggsave(here::here("Encodings", "encoding_amount_radius.png"), 
       width = 8000, height = 2000, limitsize = FALSE, 
       res = 500, unit = "in", device = agg_png)


## circles area vs. circles radius
b1_green <- 
  penguins %>% 
  count(species) %>% 
  mutate(species = fct_reorder(species, n),
         species_num = as.numeric(species),
         species_num = if_else(species == "Gentoo", species_num - .09, species_num)) %>% 
  ggplot(aes(species_num, 1)) + 
  geom_point(aes(size = n), shape = 21, color = "#28a87d", 
             fill = "#49BD92", stroke = 2, show.legend = FALSE) +
  geom_text(aes(label = species), family = "Roboto", color = "white",
            size = 7, fontface = "bold") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(.2, .2), limits = c(1, 3.2)) +
  scale_size_area(max_size = 70) +
  labs(x = NULL, y = NULL) +
  theme(plot.background = element_rect(size = 2, color = "#28a87d"),
        panel.grid.major = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(color = NA))

b1_green + plot_spacer() + b1r + plot_layout(nrow = 1, widths = c(1, .02, 1))

ggsave(here::here("Encodings", "encoding_area_radius.png"), 
       width = 8000, height = 2000, limitsize = FALSE, 
       res = 500, unit = "in", device = agg_png)


