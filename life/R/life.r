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
#' @param recursive A logical requesting the use of the recursive algorithm,
#'   where available.
#' @export
life <- function(state, ..., gen_max = NULL, sleep = NULL, recursive = TRUE) {
  f <- if (recursive && exists("Tailcall", where = "package:base", mode = "function"))
    function(state) {
      loop <- function(state, generation = 0L) {
        if (!is.null(gen_max) && generation > gen_max)
          return(state)
        image(state, ...)
        if (!is.null(sleep)) Sys.sleep(sleep)
        Tailcall(
          loop,
          apply_rules(state, count_neighbors(state)),
          if(!is.null(gen_max)) generation + 1L else 0L
        )
      }
      loop(state)
    }
  else
    function(state) {
      # Fallback to while loop solution to prevent recursion depth issues
      # because `Tailcall` was not introduced until R 4.4.0
      generation <- 0L
      while (is.null(gen_max) || generation < gen_max) {
        image(state, ...)
        state <- apply_rules(state, count_neighbors(state))
        if (!is.null(gen_max)) generation <- generation + 1L
        if (!is.null(sleep)) Sys.sleep(sleep)
      }
      state
    }
  invisible(f(state))
}

cycle_index <- function(len, n) {
  ((seq.int(0L, len - 1L) - n) %% len) + 1L
}

cycle_matrix <- function(x, n_r, n_c) {
  dims <- dim(x)
  x[cycle_index(dims[1], n_r), cycle_index(dims[2], n_c)]
}

cycles <- expand.grid(r = -1:1, c = -1:1) |>
  subset(r != 0L | c != 0L)

count_neighbors <- function(state) {
  cycles |>
    apply(1, \(r) cycle_matrix(state, r[1], r[2])) |>
    rowSums() |>
    matrix(nrow(state), ncol(state))
}

apply_rules <- function(state, neighbors) {
  state & neighbors == 2L | neighbors == 3L
}
