## setup
library(tidyverse)
library(patchwork)
library(extrafont)

set.seed(1)


## theme
extrafont::loadfonts()

theme_set(theme_bw(base_size = 18, base_family = "Montserrat"))

theme_update(plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 2)),
             plot.subtitle = element_text(size = 10, color = "grey60", hjust = 0.5),
             plot.caption = element_text(size = 10, color = "grey60"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())


## data
dat <- tibble(
  group = c(rep("1", 80), rep("2", 4), rep("3", 80), rep("4", 80)),
  value = c(seq(8, 12, length.out = 80),
            seq(8, 12, length.out = 4),
            runif(30, 8.9, 9.1), runif(30, 10.9, 11.1), runif(10, 7.9, 8.1), runif(10, 11.9, 12.1), 
            rnorm(75, 9.5, 1.3), runif(5, 15, 15.5))
  )


## plots
g <- ggplot(dat, aes(group, value, color = group, fill = group)) + 
  scale_x_discrete(NULL, breaks = 1:4, labels = c("A", "B", "C", "D")) +
  ggsci::scale_color_futurama(guide = F) +
  ggsci::scale_fill_futurama(guide = F) +
  theme(panel.grid.major.y = element_line(color = "grey90", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", size = 0.3))

p_bar <- dat %>% 
  group_by(group) %>% 
  summarize(
    mean = mean(value),
    sd = sd(value)
  ) %>% 
  ggplot(aes(group, mean, fill = group)) +
    geom_col(width = 0.8, size = 1) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
    scale_x_discrete(breaks = 1:4, labels = c("A", "B", "C", "D")) +
    ggsci::scale_fill_futurama(guide = F) +
    labs(x = NULL, y = "", title = "Barplot", subtitle = "mean ± SD") +
    theme(panel.grid.major.y = element_line(color = "grey90", size = 0.5),
          panel.grid.minor.y = element_line(color = "grey90", size = 0.3))

p_bar_nofill <- dat %>% 
  group_by(group) %>% 
  summarize(
    mean = mean(value),
    sd = sd(value)
  ) %>% 
  ggplot(aes(group, mean, color = group)) +
  geom_col(fill = "white", width = 0.8, size = 0.8) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) +
  geom_hline(yintercept = 0, color = "grey85", size = 0.8) +
  scale_x_discrete(breaks = 1:4, labels = c("A", "B", "C", "D")) +
  ggsci::scale_color_futurama(guide = F) +
  labs(x = NULL, y = "", title = "Barplot", subtitle = "mean ± SD") +
  theme(panel.grid.major.y = element_line(color = "grey90", size = 0.5),
        panel.grid.minor.y = element_line(color = "grey90", size = 0.3))

p_error <- dat %>% 
  group_by(group) %>% 
  summarize(
    mean = mean(value),
    sd = sd(value)
  ) %>% 
  ggplot(aes(group, mean, color = group)) +
    geom_point(size = 6) + 
    geom_linerange(aes(ymin = mean - sd, ymax = mean + sd), size = 1.5) +
    scale_x_discrete(breaks = 1:4, labels = c("A", "B", "C", "D")) +
    ggsci::scale_color_futurama(guide = F) +
    labs(x = NULL, y = "", title = "Error Plot", subtitle = "mean ± SD") +
    theme(panel.grid.major.y = element_line(color = "grey90", size = 0.5),
          panel.grid.minor.y = element_line(color = "grey90", size = 0.3))

p_box <- g + geom_boxplot(fill = "white", size = 0.8) +
  labs(title = "Box and Whiskers Plot", subtitle = "median, inter-quartile-range (IQR) and outliers")

p_violin <- g + geom_violin(alpha = 0.3) + 
  geom_boxplot(fill = "white", width = 0.08, outlier.color = NA, coef = 0) +
  labs(title = "Violin Plot", subtitle = "distribution, median and IQR")

p_sina <- g + geom_boxplot(color = "grey80", fill = NA, outlier.color = NA, size = 0.6) + 
  ggbeeswarm::geom_quasirandom(alpha = 0.6, size = 1.2, width = 0.25) +
  labs(title = "Jitter or Sina Plot", subtitle = "raw data (jittered)")

p_bee <- g + #geom_boxplot(fill = NA, outlier.color = NA) + 
  ggbeeswarm::geom_beeswarm(size = 1.2, alpha = 0.6) +
  labs(title = "Beeswarm Plot", subtitle = "raw data without overlap")

p_cloud <- ggplot(dat, aes(value, as.numeric(group), fill = group, color = group)) + 
  tidybayes::geom_halfeyeh(color = "grey20", scale = 0.5, alpha = 0.5, 
                           point_alpha = 1, interval_alpha = 1) +
  geom_jitter(aes(value, as.numeric(group) + 0.15), 
              alpha = 0.25, size = 1.5, height = 0.1) +
  coord_cartesian(ylim = c(0.6, 4.2)) +
  scale_x_continuous(limits = c(7, 15.5), breaks = seq(7.5, 15, by = 2.5)) +
  scale_y_reverse(breaks = 1:4, labels = c("A", "B", "C", "D"), ) +
  ggsci::scale_color_futurama(guide = F) +
  ggsci::scale_fill_futurama(guide = F) +
  labs(x = NULL, y = "", title = "Raincloud Plot", 
       subtitle = "distribution, median, density and raw data",
       caption = "\nVisualization by Cédric Scherer") +
  theme(panel.grid.major.x = element_line(color = "grey90", size = 0.5),
        panel.grid.minor.x = element_line(color = "grey90", size = 0.3))


## full panel
p <- ((p_bar + p_error + p_box + p_violin + p_sina + p_bee) * 
        scale_y_continuous("", limits = c(0, 16), breaks = seq(0, 15, by = 5), expand = c(0.01, 0.01))) 
panel <- p + p_cloud + plot_layout(nrow = 1)
ggsave(here::here("ChartsRaw", "charts_raw_panel.png"), panel, width = 34, height = 7.5)


## single plots
ggsave(here::here("ChartsRaw", "charts_raw_1_bar.png"), 
       p_bar + labs(caption = "\nVisualization by Cédric Scherer"),
       width = 6.5, height = 7.5)

ggsave(here::here("ChartsRaw", "charts_raw_2_error.png"), 
       p_error + labs(caption = "\nVisualization by Cédric Scherer"),
       width = 6.5, height = 7.5)

ggsave(here::here("ChartsRaw", "charts_raw_3_box.png"), 
       p_box + labs(caption = "\nVisualization by Cédric Scherer"),
       width = 6.5, height = 7.5)

ggsave(here::here("ChartsRaw", "charts_raw_4_violin.png"), 
       p_violin + labs(caption = "\nVisualization by Cédric Scherer"),
       width = 6.5, height = 7.5)

ggsave(here::here("ChartsRaw", "charts_raw_5_sina.png"), 
       p_sina + labs(caption = "\nVisualization by Cédric Scherer"),
       width = 6.5, height = 7.5)

ggsave(here::here("ChartsRaw", "charts_raw_6_bee.png"), 
       p_bee + labs(caption = "\nVisualization by Cédric Scherer"),
       width = 6.5, height = 7.5)

ggsave(here::here("ChartsRaw", "charts_raw_7_cloud.png"), 
       p_cloud, width = 6.5, height = 7.5)
