library(tidyverse)
library(lubridate)


times <- read.csv("benchmark_comparison/times.csv") %>%
  mutate(
    runtime = hms(runtime),
    runtime_minutes = as.numeric(hms(runtime), "minutes")
  )


ggplot(times %>% filter(run != "snakemake"), aes(x=run, y=runtime_minutes)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = times %>% filter(run == "snakemake") %>% select(runtime_minutes) %>% pull())



