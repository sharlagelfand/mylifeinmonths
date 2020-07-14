library(dplyr)
library(lubridate)
library(forcats)
library(tidyr)
library(waffle)
library(hrbrthemes)
library(extrafont)
library(ggplot2)
library(prismatic)

loadfonts(device = "pdf", quiet = TRUE)

# Prep data ----

birth_year <- 1991
birth_month <- 6
current_year <- year(today())
current_month <- month(today())

life_data <- expand_grid(
  month = month.name,
  year = birth_year:current_year
) %>%
  mutate(month = fct_relevel(month, month.name)) %>%
  arrange(year, month) %>%
  group_by(year) %>%
  mutate(month_number = row_number()) %>%
  ungroup() %>%
  filter(!(year == birth_year & month_number < birth_month)) # %>%
# filter(!(year == current_year & month_number > current_month)) # If you want to exclude after the current month - I didn't, because it looked weird!

# Add "eras" to be coloured
# "era" text can be used for annotation, and the fill colour will colour the waffle chart

eras <- tribble(
  ~year_month, ~era, ~fill_colour,
  "1991,6", "childhood", "#fbbcb8",
  "2006,9", "highschool", "#bfdff6",
  "2009,9", "undergrad", "#9acbf0",
  "2013,9", "masters", "#78baeb",
  "2015,9", "data analyst 1", "#a3e3c4",
  "2017,4", "data\nanalyst", "#75d2a6",
  "2018,7", "statistician", "#00c290",
  "2019,9", "freelance\nR dev", "#beaef5"
)

# Darken fill colour to be used for text annotations

eras[["text_colour"]] <- as.character(clr_darken(eras[["fill_colour"]], shift = 0.1))

life_data <- life_data %>%
  rowwise() %>%
  mutate(year_month = paste0(c(year, month_number), collapse = ",")) %>%
  ungroup() %>%
  left_join(eras, by = "year_month") %>%
  fill(era, fill_colour, text_colour) %>%
  mutate(fill_colour = fct_inorder(fill_colour))

# Split life data into list based on era for using labels/colours later on

life_data_list <- split(life_data, life_data$era)

# Make waffle chart! ----

# Base plot

background_colour <- "#F7F7F7"

life_in_months_base <- life_data %>%
  count(fill_colour) %>% ## the count of each era is the number of months in that era
  ggplot(aes(fill = fill_colour, values = n)) +
  geom_waffle(color = background_colour, n_rows = 12, size = 1, flip = FALSE) + ## make each row a year/12 months
  coord_equal() +
  scale_x_continuous(limits = c(-0.5, 37.5)) + # The max here will differ based on how old you are! I'm 29 (so there are 30 squares), so ~7.5 more for the additional annotation on the side
  scale_y_continuous(limits = c(-2.5, 14.5)) +
  scale_fill_identity() +
  labs(y = NULL, x = NULL) +
  theme_ipsum(grid = "") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = background_colour, color = background_colour),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

# Initial annotations ----

annotation_base_size <- 10 # Use ~10 for exporting at dpi 300, and ~3 for working interactively
annotation_lineheight <- 1
initial_annotations_font_family <- "IBM Plex Mono"
initial_annotations_colour <- "#666666"

initial_text <- function(x, y, label, size = annotation_base_size, colour = initial_annotations_colour, ...) {
  annotate("text", x = x, y = y, label = label, size = size, colour = colour, family = "IBM Plex Mono", fontface = "italic", ...)
}

initial_segment <- function(x, xend, y, yend, colour = initial_annotations_colour) {
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend), colour = colour)
}

life_in_months_initial_annotations <- life_in_months_base +
  initial_text(x = 0, y = 6.5, label = "1 year", angle = 90) +
  initial_segment(x = 0, xend = 0, y = 1, yend = 5) +
  initial_segment(x = -0.25, xend = 0.25, y = 1, yend = 1) +
  initial_segment(x = 0, xend = 0, y = 8, yend = 12) +
  initial_segment(x = -0.25, xend = 0.25, y = 12, yend = 12) +
  initial_text(x = 1, y = 14.5, label = "1 square = 1 month", size = annotation_base_size * 0.8, lineheight = annotation_lineheight, hjust = 0.4) +
  geom_curve(aes(x = 0, xend = 1, y = 14, yend = 12), arrow = arrow(length = unit(0.0175, "npc")), colour = initial_annotations_colour) +
  initial_text(x = 0.5, y = 0, label = "age", size = annotation_base_size * 0.8, hjust = 0) +
  geom_segment(aes(x = 2, xend = 4, y = 0, yend = 0), arrow = arrow(length = unit(0.0175, "npc")), colour = initial_annotations_colour) +
  annotate("text", x = 31.25, y = 6.5, label = "my life\nin months", hjust = 0, family = "Azo Sans", fontface = "bold", lineheight = 1, size = annotation_base_size * 2.5)

# "Role" annotations ----

role_annotations_y <- -0.25
roles_size <- annotation_base_size * 1.5

role_text <- function(x, y = role_annotations_y, label, size = roles_size, ...) {
  annotate("text", x = x, y = y, label = label, size = size, colour = unique(unique(life_data_list[[label]][["text_colour"]])), family = "Cedarville Cursive", ...)
}

role_text_under <- function(x, y, label, colour_era, size, ...) {
  annotate("text", x = x, y = y, label = label, colour = unique(life_data_list[[colour_era]][["text_colour"]]), size = size, family = "Cedarville Cursive", ...)
}

# For annotations: x values are the usually ~midpoint of your age (+1) during that era, give or take for some shifting around to fit labels

life_in_months_role_annotations <- life_in_months_initial_annotations +
  role_text(x = 8.5, label = "childhood") +
  role_text(x = 17, label = "highschool") +
  role_text(x = 19, y = role_annotations_y - 1.25, label = "undergrad") +
  role_text_under(x = 19, y = role_annotations_y - 2.25, label = "(stats)", colour_era = "undergrad", size = roles_size * 0.75) +
  geom_curve(aes(x = 21.5, xend = 22, y = -1.5, yend = 0.35), curvature = 0.4, arrow = arrow(length = unit(0.0175, "npc")), colour = unique(life_data_list[["undergrad"]][["text_colour"]])) +
  role_text(x = 24.25, y = role_annotations_y, label = "masters") +
  role_text_under(x = 24.25, y = role_annotations_y - 1, label = "(also stats)", colour_era = "masters", size = roles_size * 0.75) +
  role_text(x = 27.5, y = role_annotations_y - 1.5, label = "data\nanalyst", lineheight = annotation_lineheight - 0.25) +
  geom_curve(aes(x = 26.5, xend = 26, y = -1.15, yend = 0.35), curvature = -0.2, arrow = arrow(length = unit(0.0175, "npc")), colour = unique(life_data_list[["data\nanalyst"]][["text_colour"]])) +
  geom_curve(aes(x = 27.5, xend = 28, y = -1, yend = 0.35), curvature = 0.3, arrow = arrow(length = unit(0.0175, "npc")), colour = unique(life_data_list[["data\nanalyst"]][["text_colour"]])) +
  role_text(x = 31, y = role_annotations_y - 0.25, label = "statistician", lineheight = annotation_lineheight, size = roles_size) +
  geom_curve(aes(x = 28.75, xend = 29, y = role_annotations_y, yend = 0.35), curvature = -0.15, arrow = arrow(length = unit(0.0175, "npc")), colour = unique(life_data_list[["statistician"]][["text_colour"]])) +
  role_text(x = 33, y = 1.5, label = "freelance\nR dev", lineheight = annotation_lineheight - 0.25) +
  geom_curve(aes(x = 32.75, xend = 30, y = 0.5, yend = 0.35), curvature = -0.3, arrow = arrow(length = unit(0.0175, "npc")), colour = unique(life_data_list[["freelance\nR dev"]][["text_colour"]]))

# Location annotations ----

location_colour <- "#8c8c8c"
location_annotations_y <- 13

location_text <- function(x, y = location_annotations_y, label, size = annotation_base_size, ...) {
  annotate("text", x = x, y = y, label = label, size = size, colour = location_colour, family = "Cedarville Cursive", ...)
}

life_in_months_final <- life_in_months_role_annotations +
  location_text(x = 11, y = location_annotations_y + 0.1, label = "born + raised in calgary") +
  geom_segment(aes(x = 1, xend = 7, y = 13, yend = 13), colour = location_colour) +
  geom_segment(aes(x = 15, xend = 22, y = 13, yend = 13), colour = location_colour) +
  geom_segment(aes(x = 1, xend = 1, y = 12.75, yend = 13.25), colour = location_colour) +
  geom_segment(aes(x = 22, xend = 22, y = 12.75, yend = 13.25), colour = location_colour) +
  location_text(x = 21, y = location_annotations_y + 1, label = "moved to vancouver", hjust = 0.75) +
  geom_curve(aes(x = 22.7, xend = 23, y = 13.8, yend = 12.6), curvature = -0.5, arrow = arrow(length = unit(0.0175, "npc")), colour = location_colour) +
  location_text(x = 27, y = location_annotations_y + 1, label = "moved to toronto") +
  geom_curve(aes(x = 27, xend = 26, y = 13.6, yend = 12.6), curvature = -0.2, arrow = arrow(length = unit(0.0175, "npc")), colour = location_colour)

# Save final plot ----

ggsave("life_in_months.png", plot = life_in_months_final, device = "png", type = "cairo", width = 25, height = 15, dpi = 300)
