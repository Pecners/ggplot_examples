library(tidyverse)
library(palmerpenguins)
library(glue)

# prepare data for faceting
g <- penguins |> 
  group_by(species,
           sex) |> 
  summarise(avg_bill_length = mean(bill_length_mm),
            avg_bill_depth = mean(bill_depth_mm)) |> 
  filter(!is.na(sex)) |> 
  pivot_longer(cols = c("avg_bill_length",
                        "avg_bill_depth"),
               names_to = "measurement",
               values_to = "value") |> 
  mutate(measurement = ifelse(measurement == "avg_bill_length", 
                              "Avg. Bill Length",
                              "Avg. Bill Depth"),
         sex = ifelse(sex == "female", "Female", "Male"))

# basic plot with no customization

g |> 
  ggplot(aes(measurement, value, fill = sex)) +
  geom_col(position = "dodge") +
  facet_wrap(~ species)

ggsave("plots/basic_facet_plot.png", bg = "white",
       width = 9, height = 6)

# remove panel spacing

g |> 
  ggplot(aes(measurement, value, fill = sex)) +
  geom_col(position = "dodge") +
  facet_wrap(~ species) +
  theme(panel.spacing = unit(0, "cm"))

ggsave("plots/no_space_facet_plot.png", bg = "white",
       width = 9, height = 6)

# move panel strip text to bottom
g |> 
  ggplot(aes(measurement, value, fill = sex)) +
  geom_col(position = "dodge") +
  facet_wrap(~ species, strip.position = "bottom") +
  theme(panel.spacing = unit(0, "cm")) +
  labs(title = "facet_wrap(..., strip.position = 'bottom')")

ggsave("plots/strip_position_facet_plot.png", bg = "white",
       width = 9, height = 6)

# move panel strip text outside, i.e. below x axis text

g |> 
  ggplot(aes(measurement, value, fill = sex)) +
  geom_col(position = "dodge") +
  facet_wrap(~ species, strip.position = "bottom") +
  theme(panel.spacing = unit(0, "cm"),
        strip.placement = "outside") +
  labs(title = "theme(strip.placement = 'outside')")

ggsave("plots/strip_outside_facet_plot.png", bg = "white",
       width = 9, height = 6)

# expand x axis scale to add space between bars
g |> 
  ggplot(aes(measurement, value, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(expand = c(.5, .5)) +
  facet_wrap(~ species, strip.position = "bottom") +
  theme(panel.spacing = unit(0, "cm"),
        strip.placement = "outside") +
  labs(title = "scale_x_discrete(expand = c(.5, .5))")

ggsave("plots/expand_x_facet_plot.png", bg = "white",
       width = 9, height = 6)

# full text for final plot with other theme adjustments
g |> 
  ggplot(aes(measurement, value, fill = sex)) +
  geom_col(position = "dodge", width = .75) +
  facet_wrap(~ species, strip.position = "bottom") +
  scale_x_discrete(expand = c(.5, .5),
                   labels = label_wrap_gen(width = 15)) +
  scale_fill_manual(values = c("darkorange","cyan4")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        strip.placement = "outside",
        strip.text = element_text(face = "bold", size = 14),
        panel.spacing = unit(0, "cm"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 20,
                                  margin = margin(b = 10)),
        plot.subtitle = element_text(lineheight = 1.1,
                                     margin = margin(b = 20),
                                     size = 14,
                                     color = "grey30"),
        plot.caption.position = "plot",
        plot.caption = element_text(color = "grey50",
                                    hjust = 0)) +
  labs(x = "", y = "Size (mm)",
       fill = "",
       title = "Comparative Bill Sizes",
       subtitle = glue("Dimensions for male and female Adelie, ",
                       "Chinstrap, and Gentoo penguins at Palmer ",
                       "Station LTER."),
       caption = "Data from {palmerpenguins} R package.")

ggsave("plots/final_plot.png", bg = "white",
       width = 9, height = 6)
