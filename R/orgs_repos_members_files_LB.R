### New GH-API-R functions, created by Lukas Borke

## basic functions
get.all.orgs <- function(..., ctx = get.github.context())
  .api.get.request(ctx, c("organizations"), params=list(...))

get.organization.public.members.pagination <- function(..., org, ctx = get.github.context())
  .api.get.request(ctx, c("orgs", org, "public_members"), params=list(...))


# Add a new file
create.file <- function(owner, repo, path, content, ctx = get.github.context())
  .api.put.request(ctx, c("repos", owner, repo, "contents", path), body = content)

# Update an existing file
#update.file <- function(owner, repo, path, content, ctx = get.github.context())
#  .api.put.request(ctx, c("repos", owner, repo, "contents", path), body = content)


## helping functions

# API Wrapper for file creation
create.file.content.on.github = function(user_name, repo_name, file_path, file_name, file_string_content, api_create_message) {
	path_name = paste(file_path, file_name, sep = "/")
	cont_base64 = base64encode(charToRaw(file_string_content))
	ready_content = list(message = api_create_message, content = cont_base64)
	result = create.file(user_name, repo_name, path_name, ready_content, ctx = get.github.context())
	return(result)
}


# API ratelimit
get.api.ratelimit = function(org_name) {
	get.organization(org_name)$headers$'x-ratelimit-remaining'
}

get.organization.public.members.fulllist = function(org_name) {
	all_memb_list = vector()
	i = 1
	org_memb = get.organization.public.members.pagination(org = org_name, per_page = 100, page = i)$content

	while (length(org_memb) > 0) {
		for (k in 1:length(org_memb)) {
			all_memb_list = c(all_memb_list, org_memb[[k]]$login)
		}
		i = i + 1
		org_memb = get.organization.public.members.pagination(org = org_name, per_page = 100, page = i)$content
	}
	return(all_memb_list)
}

get.repositories.full.description = function(org_name, public_repos_nr) {
	
	found_software = vector()
	full_rep_desc = ""
	result_api = list()
	
	if (public_repos_nr == 0) { return(NULL) }
	
	quotient =  public_repos_nr %/% 100
	remainder = public_repos_nr %% 100
	page_iter = quotient
	if (remainder > 0) {page_iter = page_iter + 1}
	
	for (i in 1:page_iter){
		org_rep = get.organization.repositories(org = org_name, type = "public", per_page = 100, page = i)$content	
		for (k in 1:length(org_rep)) {
			full_rep_desc = paste(full_rep_desc, org_rep[[k]]$description, "; ", sep = "")
			found_software = c(found_software, org_rep[[k]]$language)
		}
	}
	result_api$desc = full_rep_desc
	result_api$sw = sort(unique(found_software))
	return(result_api)
}

## CRAN helping functions
get.organization.public.repos.fulllist = function(org_name) {
	all_repo_list = vector()
	i = 1
	org_repos = get.organization.repositories(org = org_name, per_page = 100, page = i)$content

	while (length(org_repos) > 0) {
		for (k in 1:length(org_repos)) {
			all_repo_list = c(all_repo_list, org_repos[[k]]$name)
		}
		i = i + 1
		org_repos = get.organization.repositories(org = org_name, per_page = 100, page = i)$content
	}
	return(all_repo_list)
}


### ENDE New GH-API functions
