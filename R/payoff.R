#' Payoff function factory
#'
#' @inheritParams params
#'
#' @return A function which takes a price matrix and returns a vector of option
#'   values.
#'
#' @export
implicit_value <- function(Sk,
                           Sb = NULL,
                           contract = c("call", "put", "straddle"),
                           exotic = c("no", "lookback_max", "lookback_min", "asian"),
                           barrier = c("none", "ui", "uo", "di", "do")) {
  contract <- match.arg(contract)
  exotic <- match.arg(exotic)
  barrier <- match.arg(exotic)

  function(prices) {
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
}
