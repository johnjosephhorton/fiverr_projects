#!/usr/bin/env Rscript

## Instructions:  OK, what I really want is a version of my typical Stata graphs, but in `ggplot`.
## Basically, I want to do Figure 3 in [this paper]() but in R rather than Stata.
# It's tricky to get the two axes formatted this way, without them meeting at the origin. Also, ideally, the y-axis labels have the same number of digits: 0.0, 0.1, 0.2... As a dummy CSV file, what about:

library(ggplot2)
library(tidyr)
library(magrittr)
library(dplyr)

df.raw <- read.csv("my-csv-file.csv") 
df <- df.raw %>% pivot_longer(-year) # go wide

# point where two series will have the same value on both scales
focal.year <- 2001 

# Get the delta between the two at the focal year 
delta <- df.raw %>% filter(year == focal.year) %>%
    mutate(delta = treated_state - control_states) %$% delta

g <- ggplot(data = df, aes(x = year,
                      y = ifelse(name == "control_states", value + delta, value),
                      colour = name)) + geom_line() + geom_point() +
    theme_bw() +
    ylab("Treated state value") + 
    scale_y_continuous(sec.axis = sec_axis(trans=~. - delta, name="Control State value")) +
    xlab("year") +
    theme(legend.position = "top")

pdf("outcome.pdf", width = 6, height = 3)
print(g)
dev.off()
