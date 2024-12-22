library("yaml")
library("rvest")
library("httr2")
library("checkmate")

conf <- read_yaml("config/config_web.yml")

path_assets <- file.path(getwd(), "assets")

periods_spd <- names(conf[["legislature_periods"]])[which(conf[["legislature_periods"]] == "SPD")]
periods_cdu <- names(conf[["legislature_periods"]])[which(conf[["legislature_periods"]] == "CDU")]


 get_title <- function(x) {
  x %>% 
    html_elements("h2") %>%  # titles are in a header tag
    head(n=1) %>% # use first header title
    html_text2() %>% # get text and remove white spaces
    gsub(pattern = "\\s+", replacement = "")
}

get_titles <- function(node){
  
  sapply(node, get_title)
}

filter_periods_idx <- function(x, periods) {
  
  x <- gsub("\\s+", "", x)
  
  pattern <- paste(periods, collapse = "|")
  
  grep(pattern = pattern, x = x)
}

filter_downloads_idx <- function(node) {
  
  node %>% 
    html_elements("a") %>% 
    grep(pattern = "download")
}




#----------------- Scrape federal coalition contracts --------------------------

## each text document resides within a 'detail section'
## each detail section corresponds to a coalition government
## no detail sections for other topics

url_contracts_spd <-conf[["spd_contract"]]
html_contracts_spd <- rvest::read_html(url_contracts_spd)

details_spd_contrac <- html_contracts_spd %>% 
  html_elements("details")

idx <- filter_periods_idx(details_spd_contrac, periods_spd)
details_spd_contrac <- details_spd_contrac[idx]


for (node in details_spd_contrac) {
  title <- get_title(node)
  downloads_idx <- filter_downloads_idx(node)
}

#----------------- Scrape federal election programs ----------------------------

## each text document resides within a detail section
## but not all detail sections contain federal party programs
## the relevant detail sections contain a header mentioning "Bundestagswahl"

url_programs_spd <- conf[["spd_program"]]
html_programs_spd <- rvest::read_html(url_programs_spd)

details_spd_program <- html_programs_spd %>% 
  html_elements("details")

idx <- grep("Bundestagswahl", lapply(details_spd_program, html_text))

details_spd_program <- details_spd_program[idx]

idx <- strsplit(periods_spd, "-") |>
  sapply(`[`, 1) |>
  filter_periods_idx(x = get_titles(details_spd_program))

get_titles(details_spd_program)[idx]
