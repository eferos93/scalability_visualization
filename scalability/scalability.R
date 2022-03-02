library(tidyverse)

times <- read.csv("times.csv")
t1 <- 52346
speedup <-
  times %>%
    mutate(speedup = t1/exec_time_sec)
ggplot(times, aes(nodes, exec_time_sec)) +
  geom_line()

ggplot(speedup, aes(nodes, speedup)) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "K8s genomics pipeline scalability"
  ) +
  geom_line(color = "orange") +
  geom_point(color = "orange")