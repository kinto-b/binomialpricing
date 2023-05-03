#' Parameters
#'
#' @param S0 The initial stock price
#' @param Sk The strike price
#' @param Sb The barrier price (ignored if `barrier='none'`)
#' @param contract The contract type
#' @param exotic The variety of 'exoticness'
#' @param barrier The type of barrier, up-and-in ('ui'), down-and-out ('do'), etc.
#' @param u The up factor
#' @param d The down factor
#' @param r The risk-free interest rate
#' @param expiry The time to expiry in discrete steps
#' @param simple A logical whether to use simple interest or continuously
#'   compounding interest
#' @param implicit_value A function which takes a price matrix and returns a
#'   vector of option values. The last column in the matrix is taken to be the
#'   'current' price. This is usually the result of calling `implicit_value()`.
#' @param american A logical, whether the contract allows for early exercise
#' @param geometric A logical, whether the stock price movements are geometric
#'   or arithmetic. Note that arithmetic movements can currently only be used if
#'   `r=0`.
#'
#' @name params
NULL
