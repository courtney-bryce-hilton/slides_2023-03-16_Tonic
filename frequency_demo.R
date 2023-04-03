

library(ggplot2)
library(patchwork)

# Set up data frame with time and amplitude values for each sinetone
df <- data.frame(
  time = rep(seq(0, 2*pi, length.out = 100), 4),
  amplitude = c(sin(seq(0, 8*pi, length.out = 100)),
                sin(seq(0, 16*pi, length.out = 100)) / 2,
                sin(seq(0, 24*pi, length.out = 100)) / 3,
                sin(seq(0, 32*pi, length.out = 100)) / 4),
  frequency = rep(c(1, 2, 3, 4), each = 100)
)

# Plot the sinetones
a <- ggplot(df, aes(x = time, y = amplitude)) +
  geom_line(linewidth = 0.2) +
  labs(
    x = "Time",
    y = "Amplitude"
  ) +
  theme_void()

b <- ggplot(df, aes(x = time, y = amplitude)) +
  geom_line(linewidth = 0.2) +
  facet_wrap(vars(frequency), ncol = 1) +
  labs(
    x = "Time",
    y = "Amplitude"
  ) +
  theme_void()

a + b

ggsave("frequency_demo.png", device = "png", width = 5, height = 3)


# no 2 --------------------------------------------------------------------

# Load the tidyverse package
library(tidyverse)

# Create a dataframe with x values from 0 to 10 in increments of 0.1
df <- tibble(x = seq(0, 10, by = 0.001))

# Add columns for the sine waves with different frequencies
df <- df |> 
  mutate(y1 = sin(x),
         y2 = sin(x * 4),
         y3 = sin(x * 10),
         y4 = sin(x * 40)) 

# Convert the dataframe to a long format
df_long <- df |> 
  pivot_longer(cols = starts_with("y"), names_to = "frequency", values_to = "y")

# Create the plot using ggplot2
ggplot(df_long, aes(x = x, y = y)) +
  geom_line() + 
  facet_wrap(~ frequency, ncol = 1) +
  labs(
    x = "Time",
    y = "Amplitude"
  ) + 
  theme_void() + 
  theme(
    strip.text = element_blank()
  )

ggsave("frequencies.png", device = "png", width = 5, height = 5)





