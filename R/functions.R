
# ------------------------------------------------------------------------------
#' Payoff function
#' 
#' @param prices A matrix containing prices at every node in the lattice. Each
#'   row corresponds to a possible path; each column corresponds to a time step.
#' @param Sk The strike price
#' @param Sb The barrier price (ignored if `barrier='none'`)
#' @param contract The contract type
#' @param exotic The variety of 'exoticness'
#' @param barrier The type of barrier, up-and-in ('ui'), down-and-out ('do'), etc.
#'   
#' @return A vector of values of length `nrow(prices)`
implicit_value <- function(prices,
                           Sk,
                           Sb = NULL,
                           contract = c("call", "put", "straddle"),
                           exotic = c("no", "lookback_max", "lookback_min", "asian"),
                           barrier = c("none", "ui", "uo", "di", "do")) {
  contract <- match.arg(contract)
  exotic <- match.arg(exotic)
  barrier <- match.arg(exotic)
  
  # Claim value given by: X_t = f(S1, S2, ..., S_t)
  pr <- switch(
    exotic,
    lookback_max = prices |> apply(1, max), # path max
    lookback_min = prices |> apply(1, min), # path min
    asian = prices |> apply(1, mean), # path avg
    prices[, ncol(prices)] # path endpoint
  )
  
  # Payoff is function of X_t
  if (contract == "call") {
    # (X_t-K)+
    v <- pr - Sk
    v[v<0] <- 0
  } else if (contract == "put") {
    # (X_t-S)+
    v <- Sk-pr
    v[v<0] <- 0
  } else if (contract == "straddle") {
    # max{(X_t - K)+, (K-X_t)+}
    vc <- pr - Sk
    vp <- Sk-pr
    v <- ifelse(vc<vp, vp, vc)
    v[v<0] <- 0
  }
  
  # Now enforce barrier constraint:
  if (barrier == "ui") {
    # Valid if maximum price attained >= barrier; else invalid
    pathmax <- prices |> apply(1, max)
    v[pathmax < Sb] <- 0
  } else if (barrier == "uo") {
    # Valid if maximum price attained < barrier; else invalid
    pathmax <- prices |> apply(1, max)
    v[pathmax >= Sb] <- 0
  } else if (barrier == "di") {
    # Valid if minimum price attained <= barrier; else invalid
    pathmin <- prices |> apply(1, min)
    v[pathmin > Sb] <- 0
  } else if (barrier == "do") {
    # Valid if minimum price attained > barrier; else invalid
    pathmin <- prices |> apply(1, min)
    v[pathmin <= Sb] <- 0
  }
  
  v
}


# ------------------------------------------------------------------------------
#' Option pricing
#'
#' @param S0 
#' @param u 
#' @param d 
#' @param r 
#' @param nsteps 
#' @param implicit_value 
#' @param american 
#' @param geometric 
#'
#' @return
#' @export
#'
#' @examples
price_option <- function(S0, u, d, r, nsteps, implicit_value, american = FALSE, geometric = TRUE) {
  # Risk-neutral prob
  if (geometric) {
    p <- (1+r-d)/(u-d)
    q <- 1-p
  } else {
    if (r != 0) stop("Arithmetic random walk not implemented for `r != 0`")
    p <- (-d)/(u-d) # Should be (Sk(omega)*r - d)/(u-d)
    q <- 1-p
  }
  
  # Enumerate paths. Each row is a path; each column is a time-step:
  steps <- 1:nsteps
  paths <- rep(list(0:1), nsteps)
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
  
  # Initialise matrix to store values
  values <- matrix(NA, nrow(paths), nsteps+1)
  colnames(values) <- paste0("value", c(0, steps))
  
  # Compute terminal value
  values[, nsteps+1] <- implicit_value(prices)
  
  # Compute values at remaining nodes using backwards induction
  for (i in seq(from=nsteps, to=1)) {
    x <- paths |> dplyr::select(1:(i+1))
    
    # Expected value
    x$p <- ifelse(as.logical(x[[i+1]]), p, q)
    x$v <- values[,i+1] 
    x <- x |> 
      unique() |>
      dplyr::group_by_at(1:i) |> 
      dplyr::summarise(v=sum(p*v), .groups = "drop")
    v <- paths |> dplyr::left_join(x, names(x)[1:i]) |> dplyr::pull(v)
    
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
  dplyr::tibble(cbind(paths, prices, values))
} 


plot_option_prices <- function(df) {
  steps <- df |> dplyr::select(starts_with("dir")) |> names()
  steps <- gsub("[a-z]+", "", steps)
  
  df <- purrr::map_dfr(steps, function(i) {
    nm <- c("price", "value")
    x <- df[, paste0(nm, i)]
    names(x) <- nm
    x$step <- i
    
    unique(x)
  })

  df |>
    ggplot2::ggplot(ggplot2::aes(x=step, y = price)) +
    ggplot2::geom_point() +
    ggplot2::geom_text(ggplot2::aes(label=round(price, 2)), nudge_x = -0.2, colour = "blue") +
    ggplot2::geom_text(ggplot2::aes(label=round(value, 2)), nudge_x = 0.2, colour="red")
}


# Convert to matrix ------------------------------------------------------------

as_price_matrix <- function(res) {
  d <- res |> dplyr::select(dplyr::starts_with("dir"))
  p <- res |> dplyr::select(dplyr::starts_with("price"))
  v <- res |> dplyr::select(dplyr::starts_with("value"))
  
  pm <- matrix(NA, ncol(d), ncol(d))
  vm <- matrix(NA, ncol(d), ncol(d))
  
  todiag <- function(x, i, nup) {
    x <- x[, 1:i]
    x$nup <- nup
    x <- x[, i:(i+1)] |> unique() |> dplyr::arrange(nup)
    x[[1]]
  }
  
  for (i in seq_len(ncol(d))) {
    nup <- d[, 1:i] |> apply(1, sum, na.rm=TRUE)
    pm[row(pm)+col(pm)-1 == i] <- todiag(p, i, nup)
    vm[row(vm)+col(vm)-1 == i] <- todiag(v, i, nup)
  }
  
  list(price = pm, value = vm)
}
