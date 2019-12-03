#!/usr/bin/env Rscript
# Conway's Game of Life
#
# Rules
# ------------------------------------------------------
# 1. Any live cell with two or three neighbors survives.
# 2. Any dead cell with three live neighbors becomes a live cell.
# 3. All other live cells die in the next generation.
#    Similarly, all other dead cells stay dead.
#
# Source: https://en.wikipedia.org/wiki/Conway's_Game_of_Life#Rules


state_init <- function(r, c, p) {
  if ( p > 1 ) p <- p / 100
  matrix(sample(0:1, r*c, TRUE, c(1-p, p)), r, c)
}

count_neighbors <- function(state) {
  dims <- dim(state)
  neighbors <- apply(
    expand.grid(seq_len(dims[1]), seq_len(dims[2])),
    1,
    function(x) {
      n <- 0
      v <- -1:1 + x[1]
      h <- -1:1 + x[2]
      v <- v[v > 0 & v <= dims[1]]
      h <- h[h > 0 & h <= dims[2]]

      for ( i in v ) {
        for ( j in h ) {
          if ( i == x[1] && j == x[2] ) next
          if ( state[i, j] ) n <- n + 1
        }
      }
      n
    }
  )
  matrix(neighbors, dims[1], dims[2])
}

apply_rules <- function(state, neighbors) {
  state & neighbors == 2 | neighbors == 3
}

life <- function(state, ..., generation = 0, gen_max  = 100, time = 1) {
  generation <- generation + 1L
  state <- apply_rules(state, count_neighbors(state))
  image(state, ...)
  Sys.sleep(time)
  if ( generation < gen_max )
    sys.function()(
      state,
      ...,
      generation = generation,
      gen_max = gen_max,
      time = time)
}

X11(title = "Conway's Game of Life")
par(mai = rep(0, 4))
life(
  state_init(100, 100, 10),
  col = c("#FFFFFF", "#000000"),
  axes = FALSE
)
