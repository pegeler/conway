#!/usr/bin/env Rscript
library(life)

png('frames/life%03i.png', 1000L, 1000L, "px")
par(mai = rep(0, 4))
life(
  state_init(100, 100, 50),
  col = c("#FFFFFF", "#000000"),
  axes = FALSE,
  gen_max = 100
)
dev.off()

system("ffmpeg -y -framerate 3 -i frames/life%03d.png out.mp4")
