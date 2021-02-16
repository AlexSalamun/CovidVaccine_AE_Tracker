# Clinical Trial Sites
# Pfizer
pfizer.url <- "https://www.pfizer.com/science/find-a-trial/search/BNT162b2"
trial.sites <- read_html(pfizer.url) %>%
  html_nodes(xpath = '/html/body/div[1]/div/div/section/div[2]/div/div/div[4]/div/div[6]/div/div/div[3]') %>%
  html_text()

trial.sites
