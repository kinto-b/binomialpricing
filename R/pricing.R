
#' Compute stock prices
#'
#' @inheritParams params
#'
#' @return A list containing: (i) a path matrix, in which each row corresponds
#'   to a path, each column corresponds to a time-step, zeroes correspond to
#'   down moves and ones to up-moves; and (ii) a corresponding price matrix,
#'   containing the stock price at every node in the lattice.
#'
#' @export
#'
#' @examples
#' res <- crr_stockprice(
#'   S0 = 62,
#'   u = 1.05943,
#'   d = 1/1.05943,
#'   expiry = 5
#' )
#' crr_plot(res)
#'
crr_stockprice <- function(S0, u, d, expiry, geometric = TRUE) {
  # Enumerate paths. Each row is a path; each column is a time-step:
  steps <- 1:expiry
  paths <- rep(list(0:1), expiry)
  names(paths) <- paste0("dir", steps)
  paths <- expand.grid(paths)

  # Compute prices at all nodes in path
  prices <- paths |> apply(1, function(x) {
    n_u <- cumsum(x)
    n_d <- steps - n_u
    if (geometric) {
      S0 * u^n_u * d^n_d
    } else {
      S0 + u * n_u + d * n_d
    }
  }) |> t()
  colnames(prices) <- paste0("price", steps)

  # Add initial step
  paths <- cbind(dir0=NA, paths)
  prices <- cbind(price0=S0, prices)

  list(path = paths, prices = prices)
}

#' Compute option values
#'
#' @inheritParams params
#'
#' @return A list containing: (i) a path matrix, in which each row corresponds
#'   to a path, each column corresponds to a time-step, zeroes correspond to
#'   down moves and ones to up-moves; (ii) a corresponding price matrix,
#'   containing the stock price at every node in the lattice; and (iii) a
#'   corresponding value matrix, containing the option value at every node in
#'   the lattice
#'
#' @export
#'
#' @examples
#'
#' # Vanilla European call:
#' crr_optionvalue(
#'   S0 = 62,
#'   u = 1.05943,
#'   d = 1/1.05943,
#'   r = 0.1/12, # 10% p.a compounded monthly
#'   expiry = 5,
#'   implicit_value = implicit_value(Sk=60, contract = "call"),
#'   american = FALSE
#' )
#'
#' # Asian European call:
#' crr_optionvalue(
#'   S0 = 4,
#'   u = 2,
#'   d = 1/2,
#'   r = 1/4,
#'   expiry = 3,
#'   implicit_value = implicit_value(Sk=4, contract = "call", exotic = "asian"),
#'   american = FALSE
#' )
#'
crr_optionvalue <- function(S0,
                            u,
                            d,
                            r,
                            expiry,
                            implicit_value,
                            american = FALSE,
                            simple = TRUE,
                            geometric = TRUE) {
  if (!simple) {
    stop("Continuously compounding pricing not yet implemented")
  }

  # Risk-neutral prob
  if (geometric) {
    p <- (1+r-d)/(u-d)
    q <- 1-p
  } else {
    if (r != 0) stop("Arithmetic random walk not implemented for `r != 0`")
    p <- (-d)/(u-d) # Should be (Sk(omega)*r - d)/(u-d)
    q <- 1-p
  }

  # Generate lattice
  .path <- crr_stockprice(S0, u, d, expiry, geometric)
  paths <- data.table::as.data.table(.path$path)
  prices <- .path$prices

  # Initialise matrix to store values
  values <- matrix(NA, nrow(paths), expiry+1)
  colnames(values) <- paste0("value", c(0, 1:expiry))

  # Compute terminal value
  values[, expiry+1] <- implicit_value(prices)

  # Compute values at remaining nodes using backwards induction
  V1 <- NULL # Silence CMD warning
  for (i in seq(from=expiry, to=1)) {
    x <- paths[, 1:(i+1)]

    # Expected value
    x$p <- ifelse(as.logical(x[[i+1]]), p, q)
    x$v <- values[,i+1]
    x <- unique(x)[, sum(p*v), by=c(names(x)[1:i])]
    v <- paths[x, v:=V1, on = names(x)[1:i]]
    v <- v[,v]

    # Discount
    v <- v/(1+r)

    # Implicit value
    if (american) {
      impv <- implicit_value(prices[, 1:i, drop=FALSE])
      v <- ifelse(v < impv, impv, v)
    }

    values[, i] <- v
  }

  # Now return everything together
  list(
    path = .path$path,
    prices = prices,
    values = values
  )
}


#' Plot stock prices and option values
#'
#' Plot stock prices (blue) and option values (red) for each 'position' in the
#' lattice, where a 'position' is determined entirely by the number of up- and
#' down-jumps taken to get there. Note that this means that, e.g., the up-down
#' and down-up nodes will be displayed on top of one another. Consequently,
#' multiple value labels will attach to each position when exotic options
#' are being priced.
#'
#' @param res The result of either `crr_stockprice` or `crr_optionvalue`
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' res <- crr_optionvalue(
#'   S0 = 62,
#'   u = 1.05943,
#'   d = 1/1.05943,
#'   r = 0.1/12, # 10% p.a compounded monthly
#'   expiry = 5,
#'   implicit_value = implicit_value(Sk=60, contract = "call"),
#'   american = FALSE
#' )
#'
#' crr_plot(res)
crr_plot <- function(res) {
  requireNamespace("ggplot2", quietly = TRUE)
  requireNamespace("ggrepel", quietly = TRUE)

  df <- lapply(1:ncol(res$path), function(i) {
    xx <- res$prices[, i, drop=FALSE]
    colnames(xx) <- "price"

    if (!is.null(res$values)) {
      xx <- cbind(xx, res$values[, i, drop=FALSE])
      colnames(xx) <- c("price", "value")
    }

    xx <- as.data.frame(xx)
    xx$step <- i
    unique(xx)
  })
  df <- do.call(rbind, df)


  step <- price <- value <- NULL # Silence CMD warning
  p <- df |>
    unique() |>
    ggplot2::ggplot(ggplot2::aes(x=step, y = price)) +
    ggplot2::geom_point() +
    ggplot2::geom_text(
      ggplot2::aes(label = round(price, 2)),
      nudge_x = -0.2,
      colour = "blue"
    )

  if (!is.null(res$values)) {
    p <- p + ggrepel::geom_text_repel(
      ggplot2::aes(label = round(value, 2)),
      nudge_x = 0.2,
      force = 2,
      colour = "red"
    )
  }

  p
}
