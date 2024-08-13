#!/usr/bin/env Rscript
library(life)

device <- switch(
  .Platform$OS.type,
  "windows" = \(title) windows(title = title, buffered = FALSE),
  "unix" = X11
  ) %||% stop("Unsupported platform")


device(title = "Conway's Game of Life")
par(mai = rep(0, 4))
life(
  state_init(100, 100, 10),
  col = c("#FFF", "#000"),
  axes = FALSE,
  sleep = 0.5
)
