#' Search Github repositories.
#' @template srch
#' @examples \dontrun{
#' search.repositories("tetris language:assembly")
#' }
search.repositories <- function(q, ..., ctx = get.github.context()) {
  params <- list(...)
  params$q <- q
  .api.get.request(ctx, c("search", "repositories"), params=params, config=add_headers(Accept="application/vnd.github.preview+json"))
}

#' Search Github code.
#' @template srch
#' @examples \dontrun{
#' search.code("octokit in:file extension:gemspec -repo:octokit/octokit.rb", sort="indexed")
#' }
search.code <- function(q, ..., ctx = get.github.context()) {
  params <- list(...)
  params$q <- q
  .api.get.request(ctx, c("search", "code"), params=params, config=add_headers(Accept="application/vnd.github.preview+json"))
}

#' Search Github issues.
#' @template srch
#' @examples \dontrun{
#' search.issues("windows label:bug language:python state:open", sort="created", order="asc")
#' }
search.issues <- function(q, ..., ctx = get.github.context()) {
  params <- list(...)
  params$q <- q
  .api.get.request(ctx, c("search", "issues"), params=params, config=add_headers(Accept="application/vnd.github.preview+json"))
}

#' Search Github users.
#' @template srch
#' @examples \dontrun{
#' search.users("tom repos:>42 followers:>1000")
#' }
search.users <- function(q, ..., ctx = get.github.context()) {
  params <- list(...)
  params$q <- q
  .api.get.request(ctx, c("search", "users"), params=params, config=add_headers(Accept="application/vnd.github.preview+json"))
}


# New Search API functions

search.code.full = function(query_str, delay_l = 2, max_items = 1000, print_stats = TRUE) {

	if ( (max_items <= 0) | max_items %% 1 !=0 ) {stop("'max_items' must be a positive integer!")}
	
	sr = search.code(query_str, per_page = min(100, max_items))
	Sys.sleep(delay_l)
	
	items_total_count	= sr$content$total_count
	incomplete_results	= sr$content$incomplete_results
	rate_limit			= sr$headers[["x-ratelimit-remaining"]]
	
	print(query_str)
	print(paste("items_total_count:", items_total_count))
	print(paste("incomplete_results:", incomplete_results))
	print(paste("initial Rate limit::", rate_limit))
	flush.console()
	
	full_search = sr$content$items
	
	# makes sure that the loop terminates always after 1000 items, the Search API does not provide more
	max_items = min(max_items, 1000)
	iter_total_count = min(items_total_count, max_items)
	max_iter = ceiling(iter_total_count / 100)
	# calculate the remaining items for the last loop
	last_iter_rest = iter_total_count %% 100
	if (last_iter_rest == 0) {last_iter_rest = 100}
	
	if (max_iter > 1) {
		for (i in 2:max_iter){
			if (i == max_iter) { current_page_size = last_iter_rest } else { current_page_size = 100 }
			sr = search.code(query_str, per_page = current_page_size, page = i)
			Sys.sleep(delay_l)
			while (!sr$ok) {
				# try again if the api call not successful
				sr = search.code(query_str, per_page = current_page_size, page = i)
				print(paste("tried again, page:", i)); flush.console()
				Sys.sleep(delay_l)
			}
			full_search = c(full_search, sr$content$items)
			
			print(paste("length of the retrieved items:", length(full_search)))
			print(paste("Rate limit:", sr$headers[["x-ratelimit-remaining"]]))
			flush.console()
			while (as.numeric(sr$headers[["x-ratelimit-remaining"]]) < 5) {
				Sys.sleep(delay_l)
				print(paste("Waiting mode; rate limit:", sr$headers[["x-ratelimit-remaining"]]))
				flush.console()
			}
		}
	}
		
	( repo_desc	= sapply( full_search, function(item){ if (is.null(item$repository$description)) { "" } else {item$repository$description} } ))
	( repo_name	= sapply( full_search, function(item){ item$repository$name } ) )
	( gh_path	= sapply( full_search, function(item){ item$repository$full_name } ) )
	( gh_login	= sapply( full_search, function(item){ item$repository$owner$login } ) )
	( filepath	= sapply( full_search, function(item){ item$path } ) )
	( scores	= sapply( full_search, function(item){ item$score} ) )

	if (print_stats) {
		print(sort(table(repo_desc), decreasing = T))
		print(sort(table(repo_name), decreasing = T))
		print(sort(table(gh_path), decreasing = T))
		print(sort(table(gh_login), decreasing = T))
		print(sort(table(filepath), decreasing = T))
		boxplot(scores)
	}
	
	print("-------------------------")
	print(paste("Total number of the retrieved items:", length(full_search)))
	
	res = list()
	res$repo_desc	= repo_desc
	res$repo_name	= repo_name
	res$gh_path		= gh_path
	res$gh_login	= gh_login
	res$filepath	= filepath
	res$scores		= scores
	
	res$full_search	= full_search
	
	return(res)
}
