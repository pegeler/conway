#!/usr/bin/env Rscript
library(life)

X11(title = "Conway's Game of Life")
par(mai = rep(0, 4))
life(
  state_init(100, 100, 10),
  col = c("#FFFFFF", "#000000"),
  axes = FALSE,
  sleep = 0.5
)
