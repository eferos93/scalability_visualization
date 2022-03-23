library(tidyverse)
library(ggthemes)
get_speedup <- function (dataframe) {
  t1s <- dataframe %>%
    filter(nodes == 1) %>%
    select(exec_time_sec) %>%
    rename(t1 = exec_time_sec)

  speedup <- crossing(t1s, dataframe) %>%
    mutate(speedup = t1/exec_time_sec) %>%
    group_by(nodes) %>%
    summarise(
      mean_speedup = mean(speedup),
      sd_speedup = sd(speedup)
    )
  return(speedup)
}
plot_scalability <- function (speedup) {
  p <- ggplot(speedup, aes(nodes, mean_speedup)) +
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
    geom_point(color = "orange") +
    geom_errorbar(aes(x=nodes,
                      y=sd_speedup,
                      ymin=mean_speedup-sd_speedup,
                      ymax=mean_speedup+sd_speedup),
                  color = "blue"
    ) +
    coord_cartesian(ylim = c(1.0, 3.0))
  return(p)
}
times <- read.csv("scalability/times.csv")
pipeline_speedup <- get_speedup(times)
plot_scalability(pipeline_speedup)

fastqc_times <- read.csv("scalability/fastqc_times.csv")
fastqc_speedup <- get_speedup(fastqc_times)
plot_scalability(fastqc_speedup)

pipeline_speedup <-
  pipeline_speedup %>%
    mutate(type = "whole pipeline")

fastqc_speedup <-
  fastqc_speedup %>%
    mutate(type = "fastqc part")

t <- bind_rows(pipeline_speedup, fastqc_speedup) %>%
  mutate(type = as.factor(type))

ggplot(t, aes(nodes, mean_speedup)) +
  facet_wrap(~ type) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.ticks = element_line(colour = "grey70", size = 0.5),
    panel.grid.major = element_line(colour = "grey70", size = 0.5),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "K8s genomics pipeline scalability",
    x = "number of nodes",
    y = "mean speedup"
  ) +
  geom_line(color = "orange") +
  geom_point(color = "orange") +
  geom_errorbar(aes(x=nodes,
                    y=sd_speedup,
                    ymin=mean_speedup-sd_speedup,
                    ymax=mean_speedup+sd_speedup),
                color = "blue",
                width = 0.25
  ) +
  coord_cartesian(ylim = c(1.0, 3.0))


ggplot(t, aes(nodes, mean_speedup, group = type, color = type)) +
  theme_hc() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    axis.ticks = element_line(colour = "grey70", size = 1.5),
    panel.grid.major = element_line(colour = "grey70", size = 1.5),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 17),
    legend.title = element_blank(),
    legend.key.size = unit(1.25, 'cm')
  ) +
  labs(
    title = "K8s genomics pipeline scalability",
    x = "number of nodes",
    y = "mean speedup"
  ) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x=nodes,
                    y=sd_speedup,
                    ymin=mean_speedup-sd_speedup,
                    ymax=mean_speedup+sd_speedup),
                width = 0.25
  ) +
  geom_abline(aes(fill="black"), slope = 1, intercept = 0, linetype = "dashed") +
  coord_cartesian(ylim = c(1.0, 4.0))
