#' Initialize a matrix to play Conway's Game of Life
#'
#' @param r,c The number of rows and columns.
#' @param p The proportion of "living" cells.
#' @export
state_init <- function(r, c, p) {
  if ( p > 1 ) p <- p / 100
  matrix(sample(0:1, r*c, TRUE, c(1-p, p)), r, c)
}


#' Play Conway's Game of Life
#'
#' Given a starting state, this function will display each generation of
#' Conway's Game of Life on the active graphics device. The state is updated
#' according to the rules in the Rules section.
#'
#' @section Rules:
#'
#' 1. Any live cell with two or three neighbors survives.
#' 2. Any dead cell with three live neighbors becomes a live cell.
#' 3. All other live cells die in the next generation. Similarly,
#'    all other dead cells stay dead.
#'
#' @param state A matrix representing the initial state.
#' @param ... Graphics options passed down to `image`.
#' @param gen_max The maximum number of generations to compute. Or `NULL` to
#'   run indefinitely.
#' @param sleep The sleep interval between generations, in seconds. Or `NULL`
#'   for no sleep interval.
#' @export
life <- function(state, ..., gen_max = NULL, sleep = NULL) {
  generation <- 0L
  while (is.null(gen_max) || generation < gen_max) {
    # Traded in recursive solution for while loop to prevent recursion depth
    # issues because R does not do tail call optimization.
    image(state, ...)
    state <- apply_rules(state, count_neighbors(state))
    if (!is.null(gen_max)) generation <- generation + 1L
    if (!is.null(sleep)) Sys.sleep(sleep)
  }
}

cycle_index <- function(len, n) {
  ((seq.int(0L, len - 1L) - n) %% len) + 1L
}

cycle_matrix <- function(x, n_r, n_c) {
  dims <- dim(x)
  x[cycle_index(dims[1], n_r), cycle_index(dims[2], n_c)]
}

cycles <- expand.grid(r = -1:1, c = -1:1)
cycles <- cycles[cycles$r != 0L | cycles$c != 0L, ]

count_neighbors <- function(state) {
  dims <- dim(state)
  apply(cycles, 1, \(r) cycle_matrix(state, r[1], r[2])) |>
    rowSums() |>
    matrix(dims[1], dims[2])
}

apply_rules <- function(state, neighbors) {
  state & neighbors == 2L | neighbors == 3L
}
