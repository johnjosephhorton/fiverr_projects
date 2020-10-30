#!/usr/bin/env Rscript

suppressPackageStartupMessages({
    library(ggplot2)
    library(dplyr)
    library(magrittr)
    library(scales)
})

df <- read.csv("president_polls-2.csv") %>%
    filter(answer %in% c("Trump", "Biden")) %>%
    mutate(date = as.Date(start_date, format = "%m/%d/%y")) %>%
    group_by(state) %>%
    mutate(mean.biden.share = mean(pct[answer == "Biden"])) %>%
    filter(state != "")

# order by mean biden share
df$state <- with(df, reorder(state, mean.biden.share))

g <- ggplot(data = df, aes(x = date, y = I(pct/100), colour = factor(answer))) +
    geom_point() +
    geom_smooth(aes(group = answer)) + 
    facet_wrap(~state, ncol = 5) +
    theme_bw() +
    scale_colour_manual(values = c("Blue", "Red")) +
    xlab("Polling start date") +
    ylab("Pct Support") +
    theme(legend.position = "top") + 
    scale_y_continuous(labels = scales::percent, breaks = seq(0,1, 0.25))

pdf("all_50.pdf", width = 12, height = 12)
print(g)
dev.off()


g <- ggplot(data = df %>% filter(mean.biden.share < 55 & mean.biden.share > 45),
            aes(x = date, y = I(pct/100), colour = factor(answer))) +
    geom_point() +
    ylim(0.45, 0.55) + 
    geom_smooth(aes(group = answer)) + 
    facet_wrap(~state, ncol = 5) +
    theme_bw() +
    scale_colour_manual(values = c("Blue", "Red")) +
    xlab("Polling start date") +
    ylab("Pct Support") +
    theme(legend.position = "top") + 
    scale_y_continuous(labels = scales::percent, breaks = seq(0,1, 0.25))

pdf("tossups.pdf", width = 12, height = 12)
print(g)
dev.off()
