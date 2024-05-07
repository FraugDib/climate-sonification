# animate line chart
library(gganimate)
library(showtext)

font_add_google(name = "Open Sans", family = "open-sans")
showtext_auto()

line_params <- list(
  colour = c("white", "#f4c907", "#f45207", "red"),
  size = c(5, 3, 1.5, 1),
  alpha = c(0.3, 0.5, 0.7, 1)
)

point_params <- list(
  colour = c("white", "#f4c907", "#f45207", "red"),
  size = c(8, 6, 4, 2),
  alpha = c(0.3, 0.5, 0.7, 1)
)

anim_chart <- ggplot(data = temp_change) +
  geom_line(aes(x = Year, y = lowess_temp, group = geo),
             color = line_params$colour,
             size = line_params$size,
             alpha = line_params$alpha) +
  geom_point(aes(x = Year, y = lowess_temp),
             color = point_params$colour,
             size = point_params$size,
             alpha = point_params$alpha) +
  labs(y = "Degrees Celsius Difference",
       title = "Global Temperature - Annual Difference\nfrom the 1951 - 1980 Mean",
       caption = "Data: NASA | audio/visualisation: @northernjamie") +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.background = element_rect(colour = 'black',
                                       fill = 'black'),
        panel.background = element_rect(colour = 'black',
                                        fill = 'black'),
        line = element_blank(),
        axis.text = element_text(colour="white",
                                 size = 8,
                                 family = 'open-sans'),
        plot.title = element_text(colour = 'white',
                                  size = 16,
                                  family = 'open-sans'),
        plot.caption = element_text(colour = 'white',
                                    size = 11,
                                    family = 'open-sans'),
        axis.title = element_text(colour = 'white',
                                  size = 10,
                                  family = 'open-sans'),) +
  transition_reveal(Year)

animate(anim_chart, nframes = nrow(temp_change), fps = 4, width = 16, height = 9, units = 'cm', res = 150)

anim_save(file = 'tempoutput/animate_line_4fps.gif')
