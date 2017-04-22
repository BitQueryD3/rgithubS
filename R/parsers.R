## yaml parser light
yaml_parser_light = function(search.code.results, print_item = TRUE, error_to_results = TRUE) {
	yaml_list = list()
	for (item in search.code.results$content$items) {
		path_raw = paste("https://raw.githubusercontent.com", item$repository$full_name, "master", item$path, sep ="/")
		r = GET(path_raw)
		q_str = httr::content(r, "text")
		result = try( meta_parsed_yaml <- yaml.load(q_str), silent = FALSE )
		if (class(result) == "try-error") {
			print(path_raw); flush.console()
			if (error_to_results) {yaml_list = c(yaml_list, list("YAML error"))}
		} else {
			if (print_item) {print(meta_parsed_yaml); flush.console()}
			yaml_list = c(yaml_list, list(meta_parsed_yaml))
		}
	}
	return(yaml_list)
}


## dcf parser light
dcf_parser_light = function(search.code.results, print_item = TRUE, error_to_results = TRUE) {
	dcf_list = list()
	for (item in search.code.results$content$items) {
		path_raw_dcf = paste("https://raw.githubusercontent.com", item$repository$full_name, "master/DESCRIPTION", sep ="/")
		con = url(path_raw_dcf)
		result = try( meta_parsed_cran <- read.dcf(con, all = TRUE) )
		close(con)
		if (class(result) == "try-error") {
			print(path_raw_dcf); flush.console()
			if (error_to_results) {dcf_list = c(dcf_list, NULL)}
		} else {
			if (print_item) {print(meta_parsed_cran); flush.console()}
			dcf_list = c(dcf_list, list(meta_parsed_cran))
		}
	}
	return(dcf_list)
}

