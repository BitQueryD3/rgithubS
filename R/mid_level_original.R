################################################################################
# General user information

#' Get information on the current user
#'
#' @param ctx the github context object
#'
#' @return Information about the user
get.myself <- function(ctx = get.github.context())
  .api.get.request(ctx, "user")

#' Get an organization
#'
#' @param org the organization name
#'
#' @param ctx the githb context object
#'
#' @return the organization information
get.organization <- function(org, ctx = get.github.context())
  .api.get.request(ctx, c("orgs", org))

#' get list of repositories of given organization
#'
#' @param org the given organization
#'
#' @param ... extra parameters, see \url{http://developer.github.com/v3/repos/}
#'
#' @param ctx the github context object
#'
#' @return list of repositories
get.organization.repositories <- function(org, ..., ctx = get.github.context())
  .api.get.request(ctx, c("orgs", org, "repos"), params=list(...))

#' Get the contents of a file
#'
#' @param owner the repo owner (user, org, etc)
#'
#' @param repo the name of the repo
#'
#' @param path the file path
#'
#' @param ... extra parameters to be passed. See http://developer.github.com/v3/repos/contents/#get-contents
#'
#' @param ctx the github context object
#'
#' @return the file
get.repository.path <- function(owner, repo, path, ..., ctx = get.github.context())
  .api.get.request(ctx, c("repos", owner, repo, "contents", path), params=list(...))
