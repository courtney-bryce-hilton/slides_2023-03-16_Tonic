

library(tidyverse)
library(colorspace)
library(here)


# setup -------------------------------------------------------------------

# rough average ratings

ratings <- tibble(
  rating = c(6.3, 2.01, 3.2, 2.1, 4.3, 4.03, 2.3, 4.8, 2.12, 3.5, 2.14, 2.9),
  pitch = seq(0, 11, 1)
) |> 
  mutate(
    interval = c("U", "m2", "M2", "m3", "M3", "P4", "TT", "P5", "m6", "M6", "m7", "M7"),
    interval = fct_inorder(interval)
  )

ggplot(ratings, aes(x = interval, y = rating)) + 
  geom_path(linewidth = 1, colour = "#009AC7", mapping = aes(group = 1)) + 
  geom_point(pch = 17, size = 2, colour = "#00467F") + 
  scale_y_continuous(
    labels = seq(1,7),
    breaks = seq(1,7),
    limits = c(1,7)
  ) + 
  labs(
    x = "Pitch Chroma",
    y = "Goodness of Fit Rating"
  ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 17)
  )

ggsave(filename = "probe_tone.png", device = "png", path = here("media"), width = 6, height = 6)


# ordered version ---------------------------------------------------------

ratings |> 
  mutate(
    interval = fct_reorder(interval, rating, .desc = TRUE)
  ) |> 
  ggplot(aes(x = interval, y = rating)) + 
  geom_line(linewidth = 1, colour = "#009AC7", mapping = aes(group = 2)) + 
  geom_point(pch = 17, size = 2, colour = "#00467F") + 
  scale_y_continuous(
    labels = seq(1,7),
    breaks = seq(1,7),
    limits = c(1,7)
  ) + 
  labs(
    x = "Pitch Chroma (sorted)",
    y = "Goodness of Fit Rating"
  ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 17),
    axis.text.x = element_blank()
  )

ggsave(filename = "probe_tone_ordered.png", device = "png", path = here("media"), width = 6, height = 6)


# with exponential function fit -------------------------------------------


ratings2 <- ratings |> 
  mutate(order = rank(-rating))

# Fit exponential function to data
fit <- nls(rating ~ a * exp(-b * order), data = ratings2, start = list(a = max(ratings2$rating), b = 1))

# Create a data frame with the fitted curve
df_fit <- data.frame(order = seq(min(ratings2$order), max(ratings2$order), length.out = 100))
df_fit$rating <- predict(fit, newdata = df_fit)

ggplot() +
  geom_line(aes(order, rating), data = df_fit, color = "#009AC7", linewidth = 1) +
  geom_point(aes(order, rating), data = ratings2, colour = "#00467F", size = 2) +
  scale_y_continuous(
    labels = seq(1,7),
    breaks = seq(1,7),
    limits = c(1,7)
  ) + 
  labs(
    x = "Pitch Chroma (sorted)",
    y = "Goodness of Fit Rating"
  ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(colour = "black", size = 14),
    axis.title = element_text(colour = "black", size = 17),
    axis.text.x = element_blank()
  )

ggsave(filename = "probe_tone_exponential.png", device = "png", path = here("media"), width = 6, height = 6)


