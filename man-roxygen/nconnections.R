#' @note
#' R hardcodes the maximum number of socket connections it can use (currently set to 128 in R 4.1).
#' Three of these are reserved for the main R process, so practically speaking, a user can create
#' \emph{at most} 125 connections e.g., when creating a cluster.
#' See \url{https://github.com/HenrikBengtsson/Wishlist-for-R/issues/28}.
#'
#' We limit this a bit further here just in case the user already has open connections.
