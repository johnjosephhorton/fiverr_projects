#!/usr/bin/env Rscript

suppressPackageStartupMessages({
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
})

df <- read.csv("proportions.csv") %>%
    pivot_longer(-c(word,position)) %>%
    arrange(word, position, name) %>% 
    group_by(word, position) %>%
    mutate(label.height = sum(value) - cumsum(value) + value/2)

g <- ggplot(data = df, aes(x = position, y = value, fill = factor(name), group = factor(name))) +
    geom_bar(stat = "identity")  +
    facet_wrap(~word, ncol = 2)  +
    geom_text(aes(y = label.height, label = value)) +
    scale_y_continuous(label = scales::percent) +
    ylab("% of observations")

pdf("v0.pdf", width = 6, height = 6)
print(g)
dev.off()


g <- ggplot(data = df, aes(x = position, y = value, colour = factor(name), group = factor(name))) +
    geom_line() +
    facet_wrap(~word, ncol = 2) +
    theme_bw() +
    geom_label_repel(data = df %>% filter(position == "identical"), aes(label = name),
                     segment.colour = "gray", xlim = c(2, NA)) +
    theme(legend.position = "None") +
    scale_y_continuous(label = scales::percent) +
    ylab("% of observations")
    
pdf("v1.pdf", width = 6, height = 6)
print(g)
dev.off()

g <- ggplot(data = df, aes(x = position,
                           y = value,
                           colour = factor(name),
                           linetype = factor(word),
                           group = interaction(name, word))) +
    geom_line() +
    theme_bw() +
    facet_wrap(~name, ncol = 3) + 
    geom_label_repel(data = df %>% filter(position == "identical"), aes(label = gsub("word-", "", word)),
                     segment.colour = "gray", xlim = c(2, NA)) +
    theme(legend.position = "None") +
    scale_y_continuous(label = scales::percent) +
    ylab("% of observations")

pdf("v2.pdf", width = 7, height = 7)
print(g)
dev.off()
