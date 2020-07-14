library(dplyr)
library(lubridate)
library(waffle)
library(hrbrthemes)
library(extrafont)
library(tibble)
library(forcats)
library(ggplot2)
library(tidyr)
loadfonts(device = "pdf", quiet = TRUE)

# Create the data-----
life_data <- expand_grid(
  month = month.name,
  year = 1991:2020
) %>%
  mutate(month = fct_relevel(month, month.name)) %>%
  arrange(year, month) %>%
  group_by(year) %>%
  mutate(month_number = row_number()) %>%
  ungroup() %>%
  filter(!(year == 1991 & month_number < 6))

# Add "eras" to be coloured ----
life_data <- life_data %>%
  rowwise() %>%
  mutate(year_month = paste0(c(year, month_number), collapse = ",")) %>%
  ungroup() %>%
  mutate(era = case_when(
    year_month == "1991,6" ~ "Childhood",
    year_month == "2006,9" ~ "High School",
    year_month == "2009,9" ~ "Undergraduate",
    year_month == "2013,9" ~ "Grad School",
    year_month == "2015,9" ~ "Unbounce",
    year_month == "2017,4" ~ "Wattpad",
    year_month == "2018,7" ~ "College of Nurses of Ontario",
    year_month == "2019,9" ~ "Freelance R and Shiny Developer"
  )) %>%
  fill(era) %>%
  mutate(era = fct_inorder(era))

# Waffle chart-----
life_in_months_base <- life_data %>%
  count(era) %>% ## the count of each era is the number of months in that era
  ggplot(aes(fill = era, values = n)) +
  geom_waffle(color = "#F7F7F7", n_rows = 12, size = 1, flip = FALSE) + ## make each row a year/12 months
  # scale_fill_manual(name = "", values = c("#EF476F", "#FCA311", "#FFD166", "#0EAD69", "#4ECDC4", "#118AB2")) + ## assign colors to the eras
  coord_equal() +
  # scale_y_continuous(breaks = c(1, 4, 7, 10), labels = c("June", "September", "December", "March")) +
  scale_x_continuous(limits = c(-0.5, 37.5)) +
  scale_y_continuous(limits = c(-2.5, 14.5)) +
  scale_fill_manual(values = c("#fbbcb8", "#bfdff6", "#9acbf0", "#9fabe8", "#a3e3c4", "#75d2a6", "#00c290", "#beaef5")) +
  labs(y = NULL, x = NULL) +
  theme_ipsum(grid = "") +
  theme(
    text = element_text(family = "IBM Plex Mono", face = "italic"),
    legend.position = "none",
    plot.background = element_rect(fill = "#F7F7F7", color = "#F7F7F7"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

annotation_base_size <- 10
annotation_lineheight <- 1
initial_annotations_font_family <- "IBM Plex Mono"
initial_annotations_colour <- "#666666"

life_in_months_initial_annotations <- life_in_months_base +
  annotate("text", x = 0, y = 6.5, label = "1 year", angle = 90, family = initial_annotations_font_family, fontface = "italic", size = annotation_base_size, colour = initial_annotations_colour) +
  geom_segment(aes(x = 0, xend = 0, y = 1, yend = 5), colour = initial_annotations_colour) +
  geom_segment(aes(x = -0.25, xend = 0.25, y = 1, yend = 1), colour = initial_annotations_colour) +
  geom_segment(aes(x = 0, xend = 0, y = 8, yend = 12), colour = initial_annotations_colour) +
  geom_segment(aes(x = -0.25, xend = 0.25, y = 12, yend = 12), colour = initial_annotations_colour) +
  annotate("text", x = 1, y = 14.5, label = "1 square = 1 month", family = initial_annotations_font_family, fontface = "italic", size = annotation_base_size * 0.8, lineheight = annotation_lineheight, hjust = 0.4, colour = initial_annotations_colour) +
  geom_curve(aes(x = 0, xend = 1, y = 14, yend = 12), arrow = arrow(length = unit(0.0175, "npc")), colour = initial_annotations_colour) +
  annotate("text", x = 0.5, y = 0, label = "age", family = initial_annotations_font_family, fontface = "italic", hjust = 0, size = annotation_base_size, colour = initial_annotations_colour) +
  geom_segment(aes(x = 2, xend = 4, y = 0, yend = 0), arrow = arrow(length = unit(0.0175, "npc")), colour = initial_annotations_colour) +
  annotate("text", x = 31, y = 6.5, label = "my life\nin months", hjust = 0, family = "Azo Sans", fontface = "bold", lineheight = 1, size = annotation_base_size * 2.5)

role_annotations_font_family <- "Cedarville Cursive"

role_annotations_y <- -0.25
roles_size <- annotation_base_size * 1.5

life_in_months_role_annotations <- life_in_months_initial_annotations +
  annotate("text", x = 8.5, y = role_annotations_y, label = "childhood", family = role_annotations_font_family, size = roles_size, colour = "#f88f88") +
  annotate("text", x = 17, y = role_annotations_y, label = "highschool", family = role_annotations_font_family, size = roles_size, colour = "#92c9f0") +
  annotate("text", x = 19, y = role_annotations_y - 1.25, label = "undergrad", family = role_annotations_font_family, size = roles_size, colour = "#6eb4e9") +
  annotate("text", x = 19, y = role_annotations_y - 2.25, label = "(stats)", family = role_annotations_font_family, size = roles_size * 0.75, colour = "#6eb4e9") +
  geom_curve(aes(x = 21.5, xend = 22, y = -1.5, yend = 0.35), curvature = 0.4, arrow = arrow(length = unit(0.0175, "npc")), colour = "#6eb4e9") +
  annotate("text", x = 24.25, y = role_annotations_y, label = "masters", family = role_annotations_font_family, size = roles_size, colour = "#7587de") +
  annotate("text", x = 24.25, y = role_annotations_y - 1, label = "(also stats)", family = role_annotations_font_family, size = roles_size * 0.75, colour = "#7587de") +
  annotate("text", x = 27.5, y = role_annotations_y - 1.5, label = "data\nanalyst", family = role_annotations_font_family, lineheight = annotation_lineheight - 0.25, size = roles_size, colour = "#4fc58d") +
  geom_curve(aes(x = 26.5, xend = 26, y = -1.15, yend = 0.35), curvature = -0.2, arrow = arrow(length = unit(0.0175, "npc")), colour = "#4fc58d") +
  geom_curve(aes(x = 27.5, xend = 28, y = -1, yend = 0.35), curvature = 0.3, arrow = arrow(length = unit(0.0175, "npc")), colour = "#4fc58d") +
  annotate("text", x = 31, y = role_annotations_y - 0.25, label = "statistician", family = role_annotations_font_family, lineheight = annotation_lineheight, size = roles_size, colour = "#00a87d") +
  geom_curve(aes(x = 28.75, xend = 29, y = role_annotations_y, yend = 0.35), curvature = -0.15, arrow = arrow(length = unit(0.0175, "npc")), colour = "#00a87d") +
  annotate("text", x = 33, y = 1.5, label = "freelance\nR dev", family = role_annotations_font_family, lineheight = annotation_lineheight - 0.25, size = roles_size, colour = "#ab97f3") +
  geom_curve(aes(x = 32.75, xend = 30, y = 0.5, yend = 0.35), curvature = -0.3, arrow = arrow(length = unit(0.0175, "npc")), colour = "#ab97f3")

location_colour <- "#8c8c8c"

life_in_months_final <- life_in_months_role_annotations +
  annotate("text", x = 11, y = 13.1, label = "born + raised in calgary", family = role_annotations_font_family, size = annotation_base_size, colour = location_colour) +
  geom_segment(aes(x = 1, xend = 7, y = 13, yend = 13), colour = location_colour) +
  geom_segment(aes(x = 15, xend = 22, y = 13, yend = 13), colour = location_colour) +
  geom_segment(aes(x = 1, xend = 1, y = 12.75, yend = 13.25), colour = location_colour) +
  geom_segment(aes(x = 22, xend = 22, y = 12.75, yend = 13.25), colour = location_colour) +
  annotate("text", x = 21, y = 14, label = "moved to vancouver", family = role_annotations_font_family, size = annotation_base_size, lineheight = annotation_lineheight, hjust = 0.75, colour = location_colour) +
  geom_curve(aes(x = 22.7, xend = 23, y = 13.8, yend = 12.6), curvature = -0.5, arrow = arrow(length = unit(0.0175, "npc")), colour = location_colour) +
  annotate("text", x = 27, y = 14, label = "moved to toronto", family = role_annotations_font_family, size = annotation_base_size, lineheight = annotation_lineheight, colour = location_colour) +
  geom_curve(aes(x = 27, xend = 26, y = 13.6, yend = 12.6), curvature = -0.2, arrow = arrow(length = unit(0.0175, "npc")), colour = location_colour)

ggsave("life_in_months.png", plot = life_in_months_final, device = "png", type = "cairo", width = 25, height = 15, dpi = 300)
