library(tidyverse)

times <- read.csv("times.csv")
t1 <- 52346
speedup <-
  times %>%
    mutate(speedup = t1/exec_time_sec)
ggplot(times, aes(nodes, exec_time_sec)) +
  geom_line()

ggplot(speedup, aes(nodes, speedup)) +
  geom_line()