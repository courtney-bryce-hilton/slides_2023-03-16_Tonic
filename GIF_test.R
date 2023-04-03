library(ggplot2)
library(gganimate)
library(gifski)



# animate it --------------------------------------------------------------

c_major_scale <- c("C", "D", "E", "F", "G", "A", "B")

# Add indicator for whether note is in C major scale
df2 <- df |>
  filter(label %in% c_major_scale) |> 
  add_row(label = "C", angle = 1.5707963)

df_anim <- df |> 
  
map_dfr(c_major_scale, \(.note) {
  out <- df |> 
    mutate()
})
  
df3 <- df2 |> 
  slice(1:7) |> 
  rename(label2 = label) |> 
  mutate(label2 = factor(label2, levels = c_major_scale))

# Plot a circle with a radius of 1 and center at (0,0)
p <- ggplot(df3) +
  # Add a line that connects all the points to form a circle
  geom_path(
    aes(x = cos(angle), y = sin(angle)),
    colour = "black",
    size = 1,
    linetype = "dashed",
    data = df
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
  geom_label(
    aes(x = cos(angle), y = sin(angle), label = label2),
    size = 8,
    fill = "#009AC7"
  ) +
  # Adjust the axis limits so the circle is centered
  scale_x_continuous(limits = c(-1.2, 1.2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1.2, 1.2), expand = c(0, 0)) + 
  coord_fixed() +
  theme_void() + 
  theme(
    legend.position = "none"
  ) + 
  transition_manual(label2)


gganimate::animate(p, width = 400, height = 400, duration = 2.8)

anim_save("c_maj_animation_2.5hz.gif", path = here("media", "figs"))


