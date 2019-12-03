life <- function(state, ..., generation = 0, gen_max  = 100) {
  generation <- generation + 1L
  state <- apply_rules(state, count_neighbors(state))
  image(state, ...)
  if ( generation < gen_max )
    sys.function()(state, ..., generation = generation)
}

png('frames/life%03i.png', 1000L, 1000L, "px")
par(mai = rep(0, 4))
life(
  state_init(100, 100, 50),
  col = c("#FFFFFF", "#000000"),
  axes = FALSE
)
dev.off()

system("ffmpeg -y -framerate 3 -i frames/life%03d.png out.mp4")
