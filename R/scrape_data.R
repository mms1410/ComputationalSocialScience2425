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
#----------------- Scrape federal coalition contracts spd ----------------------

## each text document resides within a 'detail section'
## each detail section corresponds to a coalition government
## no detail sections for other topics

url_contracts_spd <-conf[["spd_contract"]]
html_contracts_spd <- rvest::read_html(url_contracts_spd)

details_spd_contrac <- html_contracts_spd %>% 
  html_elements("details")

idx <- filter_periods_idx(details_spd_contrac, periods_spd)
details_spd_contrac <- details_spd_contrac[idx]

cat(paste0("Found ", length(details_spd_contrac), " coalition contracts to scrape"))

for (node in details_spd_contrac) {
  break
  title <- get_title(node)
  
  # extract election year
  year_match <- grep(x = title, pattern = "(\\d+)(?=-)",perl = TRUE, value = TRUE)
  foldername <- regmatches(year_match, regexpr("\\d+(?=-)", match, perl = TRUE))
  
  # get all linked pdf files
  downloads <- node %>% 
    html_elements("a") %>% 
    html_attr("href") %>% 
    grep(pattern = "\\.pdf$", value = TRUE)
  
  folderpath <- file.path(path_assets, "pdf","coalition_contracts", "spd", foldername)
  dir.create(folderpath, recursive = TRUE)
  
  cat(paste0("\n---------- Start scraping ", length(downloads)," item(s) for '", title, "' ---------- \n"))
  
   for (url_reference in downloads) {
     
     cat(paste0("  Scrape url ", url_reference, ":\n"))
     
     filename <- sub(".*\\/([^\\/]+\\.pdf)$", "\\1", url_reference)
     
     tryCatch({
         response <- request(url_reference) |> req_perform()
         
         }, error = function(e) {
           cat(paste0("    An unexpected error occured when handling url ", url_reference, "\n"))
       }
     )
     status <- resp_status(response)
     
     if (status >= 200 && status <= 299){
       
       destination <- file.path(folderpath, filename)
       
       writeBin(resp_body_raw(response), destination)
       
       if (file.exists(destination))
         cat(paste0("    Downloaded file into: ", destination, "\n"))
     }
   }
}
cat("\n--------- Finished scraping SPD contracts ---------\n")
#----------------- Scrape federal election programs spd -------------------------

## each text document resides within a detail section
## but not all detail sections contain federal party programs
## the relevant detail sections contain a header mentioning "Bundestagswahl"

url_programs_spd <- conf[["spd_program"]]
html_programs_spd <- rvest::read_html(url_programs_spd)

details_spd_program <- html_programs_spd %>% 
  html_elements("details")

idx <- grep("Bundestagswahl", lapply(X = details_spd_program, FUN = html_text))

details_spd_program <- details_spd_program[idx]

## only for periods where in coalition government
idx <- strsplit(periods_spd, "-") |>
  sapply(`[`, 1) |>
  filter_periods_idx(x = get_titles(details_spd_program))



cat(paste0("Found ", length(details_spd_program), " election programmes to scrape"))


for (node in details_spd_program[idx]) {
  break
  title <- get_title(node)
  
  # extract election year
  foldername <- sub(".*([0-9]{4})$", "\\1", title)
  
  downloads <- node %>% 
    html_elements("a") %>% 
    html_attr("href")
  
  folderpath <- file.path(path_assets, "pdf","election_programmes", "spd", foldername)
  
  dir.create(folderpath, recursive = TRUE)
  
  cat(paste0("\n---------- Start scraping ", length(downloads)," item(s) for '", title, "' ---------- \n"))
  
  for (url_reference in downloads) {
    
    cat(paste0("  Scrape url ", url_reference, ":\n"))
    
    filename <- sub(".*\\/([^\\/]+\\.pdf)$", "\\1", url_reference)
    
    tryCatch({
      response <- request(url_reference) |> req_perform()
      
    }, error = function(e) {
      cat(paste0("    An unexpected error occured when handling url ", url_reference, "\n"))
    }
    )
    status <- resp_status(response)
    
    if (status >= 200 && status <= 299){
      destination <- file.path(folderpath, filename)
      writeBin(resp_body_raw(response), destination)
      if (file.exists(destination))
        cat(paste0("    Downloaded file into: ", destination, "\n"))
    }
  }
  
}
cat("\n--------- Finished scraping SPD election programmes ---------\n")

#----------------- Scrape federal coalition contracts cdu ----------------------
url_contracts_cdu <-conf[["cdu_contract"]]
html_contracts_cdu <- rvest::read_html(url_contracts_cdu)

html_contracts_cdu %>% html_elements("a.matomo-download")
html_contracts_spd %>% html_elements("a") %>% html_attrs()

#----------------- Scrape federal election programs cdu ------------------------