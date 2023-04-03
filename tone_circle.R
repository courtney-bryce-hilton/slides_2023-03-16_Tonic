
library(ggplot2)
library(here)

# official Auckland Uni colours
auckland_lightBlue <- "#009AC7"
auckland_darkBlue <- "#00467F"


# Draw circle of pitch classes for chromatic scale ------------------------


pitches <- c("C", "B", "A#", "A", "G#", "G", "F#", "F", "E", "D#", "D", "C#")

# Create a data frame with the numbers 0 to 11 and their corresponding angles
df <- data.frame(
  label = c(pitches, "blah"),
  angle = (2*pi/12)*0:12
)

# Adjust the angle of the 0 label to be at the top, and shift the rest of the angles by pi/2 to make them proceed clockwise
df$angle <- (df$angle + pi/2) %% (2*pi)

# Plot a circle with a radius of 1 and center at (0,0)
ggplot(df) +
  # Add a line that connects all the points to form a circle
  geom_path(
    aes(x = cos(angle), y = sin(angle)),
    colour = "black",
    size = 1,
    linetype = "dashed"
  ) +
  # Add the labels as text
  geom_label(
    data = df |> filter(label != "blah"),
    aes(x = cos(angle), y = sin(angle), label = label),
    size = 8
  ) +
  # Adjust the axis limits so the circle is centered
  scale_x_continuous(limits = c(-1.2, 1.2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1.2, 1.2), expand = c(0, 0)) + 
  coord_fixed() +
  theme_void()


ggsave("tone_circle.png", path = here("media", "figs"), device = "png", width = 5, height = 5)


# Now trace C major scale circumscribing the circle -----------------------


c_major_scale <- c("C", "D", "E", "F", "G", "A", "B")

# Add indicator for whether note is in C major scale
df2 <- df |>  
  filter(label %in% c_major_scale) |> 
  add_row(label = "C", angle = 1.5707963)

# Plot a circle with a radius of 1 and center at (0,0)
ggplot(df) +
  # Add a line that connects all the points to form a circle
  geom_path(
    aes(x = cos(angle), y = sin(angle)),
    colour = "black",
    size = 1,
    linetype = "dashed"
  ) +
  geom_path(
    aes(x = cos(angle), y = sin(angle)),
    colour = auckland_lightBlue,
    size = 1,
    data = df2,
    alpha = 0.6
  ) +
  # Add the labels as text
  geom_label(
    data = df |> filter(label != "blah"),
    aes(x = cos(angle), y = sin(angle), label = label),
    size = 8
  ) +
  # Adjust the axis limits so the circle is centered
  scale_x_continuous(limits = c(-1.2, 1.2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1.2, 1.2), expand = c(0, 0)) + 
  coord_fixed() +
  theme_void()

ggsave("tone_circle_c_major.png", path = here("media", "figs"), device = "png", width = 5, height = 5)


# now circumscribe the tonic triad ----------------------------------------


c_major_triad <- c("C", "E", "G")

# Add indicator for whether note is in C major scale
df_triad <- df |>  
  filter(label %in% c_major_triad) |> 
  add_row(label = "C", angle = 1.5707963)

# Plot a circle with a radius of 1 and center at (0,0)
ggplot(df) +
  # Add a line that connects all the points to form a circle
  geom_path(
    aes(x = cos(angle), y = sin(angle)),
    colour = "black",
    size = 1,
    linetype = "dashed"
  ) +
  # Circumscribing the major scale
  geom_path(
    aes(x = cos(angle), y = sin(angle)),
    colour = auckland_lightBlue,
    size = 1,
    data = df2,
    alpha = 0.6
  ) +
  # Circumscribing the major triad
  geom_path(
    aes(x = cos(angle), y = sin(angle)),
    colour = "red",
    size = 1,
    data = df_triad,
    alpha = 0.6
  ) +
  # Add the labels as text
  geom_label(
    data = df |> filter(label != "blah"),
    aes(x = cos(angle), y = sin(angle), label = label),
    size = 8
  ) +
  # Adjust the axis limits so the circle is centered
  scale_x_continuous(limits = c(-1.2, 1.2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1.2, 1.2), expand = c(0, 0)) + 
  coord_fixed() +
  theme_void()

ggsave("tone_circle_c_major_triad.png", path = here("media", "figs"), device = "png", width = 5, height = 5)


# Now add highlight for tonic ---------------------------------------------


ggplot(df) +
  # Add a line that connects all the points to form a circle
  geom_path(
    aes(x = cos(angle), y = sin(angle)),
    colour = "black",
    size = 1,
    linetype = "dashed"
  ) +
  # Circumscribing the major scale
  geom_path(
    aes(x = cos(angle), y = sin(angle)),
    colour = auckland_lightBlue,
    size = 1,
    data = df2,
    alpha = 0.6
  ) +
  # Circumscribing the major triad
  geom_path(
    aes(x = cos(angle), y = sin(angle)),
    colour = "red",
    size = 1,
    data = df_triad,
    alpha = 0.6
  ) +
  # Add the labels as text
  geom_label(
    data = df |> filter(label != "blah"),
    aes(x = cos(angle), y = sin(angle), label = label),
    size = 8
  ) +
  geom_label(
    data = df |> filter(label == "C"),
    aes(x = cos(angle), y = sin(angle), label = label),
    size = 14,
    fill = "yellow"
  ) +
  # Adjust the axis limits so the circle is centered
  scale_x_continuous(limits = c(-1.2, 1.2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1.2, 1.2), expand = c(0, 0)) + 
  coord_fixed() +
  theme_void()

ggsave("tone_circle_c_tonic.png", path = here("media", "figs"), device = "png", width = 5, height = 5)


