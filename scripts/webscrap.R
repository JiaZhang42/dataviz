library(rvest)
library(RSelenium)
library(robotstxt)
library(polite)
library(tidyverse)

robotstxt('https://luzpar.netlify.app/')$permissions
paths_allowed(domain = 'https://luzpar.netlify.app/')
paths_allowed(domain = 'https://luzpar.netlify.app/', paths = c('/states/', '/constituencies/'))
paths_allowed(domain = 'https://luzpar.netlify.app/', paths = c('/states/', '/constituencies/'), bot = 'googlebot')
paths_allowed(domain = 'https://luzpar.netlify.app/', paths = c('/states/', '/constituencies/'), bot = 'zj')

robotstxt('https://www.theguardian.com')$permissions
robotstxt('https://www.douban.com/')$permissions
paths_allowed(domain = 'https://movie.douban.com/', paths = '/explore#!type=movie&tag=%E6%9C%80%E6%96%B0')

## rvest for static pages----
paths_allowed(domain = 'https://luzpar.netlify.app/')
read_html('https://luzpar.netlify.app/')
paths_allowed(domain = 'https://luzpar.netlify.app/', paths = '/states/')
read_html('https://luzpar.netlify.app/states/')
bow('https://luzpar.netlify.app/states/', user_agent = 'zj', delay = 5) %>% scrape()

read_html('https://luzpar.netlify.app') %>% html_elements(css = "a")
read_html('https://luzpar.netlify.app') %>% html_elements(css = "#title a")
read_html('https://luzpar.netlify.app') %>% html_elements(css = "br+ p a , p:nth-child(3) a")
read_html('https://luzpar.netlify.app/states/') %>% html_elements(css = '#top > div.page-body > div:nth-child(2) > div > div.col-lg-12 > div > ul > li:nth-child(1) > a')
read_html('https://luzpar.netlify.app/states/') %>% html_elements(css = '.article-style a')
read_html('https://luzpar.netlify.app/states/') %>% html_elements(css = '.article-style li:nth-child(4) a , .article-style li:nth-child(2) a')


read_html('https://luzpar.netlify.app') %>% 
  html_elements(css = "#title a") %>% 
  html_text()
read_html('https://luzpar.netlify.app/states/') %>% 
  html_elements(css = '.article-style a') %>% 
  html_text()
read_html('https://luzpar.netlify.app/constituencies/') %>% 
  html_elements(css = 'h2') %>% 
  html_text()

read_html('https://luzpar.netlify.app') %>% 
  html_elements(css = "#title a") %>% 
  html_attrs()
read_html('https://luzpar.netlify.app') %>% 
  html_elements(css = "#title a") %>% 
  html_attr(name='href')
read_html('https://luzpar.netlify.app') %>% 
  html_elements(css = "#title a") %>% 
  html_attr(name='href') %>% 
  url_absolute(base = 'https://luzpar.netlify.app')
read_html('http://luzpar.netlify.app/constituencies/') %>% 
  html_elements(css = 'h2 a') %>% 
  html_attr('href') %>% 
  url_absolute(base = 'http://luzpar.netlify.app/constituencies/')


read_html('https://luzpar.netlify.app/members/') %>% 
  html_elements(css = 'table') %>% 
  html_table()
read_html('https://luzpar.netlify.app/members/') %>% 
  html_elements(css = 'td:nth-child(1) a') %>% 
  html_attrs('href')
members_page <- read_html('https://luzpar.netlify.app/members/')
df_wide <- members_page %>% 
  html_elements(css = 'table') %>% 
  html_table() %>% 
  .[[1]] %>% 
  add_column(
    member_link = members_page %>% 
      html_elements(css = 'td:nth-child(1) a') %>% 
      html_attr('href') %>% 
      url_absolute(base = 'https://luzpar.netlify.app/members/'), 
    preferred_name = members_page %>% 
      html_elements(css = 'td:nth-child(1) a') %>% 
      html_attr('title'), 
    constituency_link = members_page %>% 
      html_elements(css = 'td:nth-child(2) a') %>% 
      html_attr('href') %>% 
      url_absolute(base = 'https://luzpar.netlify.app/constituencies/'), 
    constituency_location = members_page %>% 
      html_elements(css = 'td:nth-child(2) a') %>% 
      html_attr('title')
  )
i = 1

temp_list <- list()
for (i in seq_along(df_wide$constituency_link)) {
  the_page <- read_html(df_wide$constituency_link[i])
  temp_tibble <- tibble(
    'constituency' = the_page %>% html_element(css='#constituency') %>% html_text(), 
    'second_party' = the_page %>% html_element('tr:nth-child(3) td:nth-child(1)') %>% html_text(), 
    'vote_share' = the_page %>% html_element('tr:nth-child(3) td~ td+ td') %>% html_text()
  )
  temp_list[[i]] <- temp_tibble
}
df <- as_tibble(do.call(rbind, temp_list))

temp_list <- list()
for (i in seq_along(df_wide$member_link)) {
  the_page <- read_html(df_wide$member_link[i])
  temp_tibble <- tibble(
    'mp' = the_page %>% html_element('#top > div.page-body > article > div.article-container.pt-3 > h1') %>% html_text(), 
    'party' = the_page %>% html_element('#party') %>% html_text(), 
    'constituency' = the_page %>% html_element('#party+ a') %>% html_text(), 
    'state' = the_page %>% html_element('#state') %>% html_text(), 
    'mp_since' = the_page %>% html_element('#since') %>% html_text(), 
    'attendance' = the_page %>% html_element('#attendance') %>% html_text(), 
    'speeches' = the_page %>% html_element('#speeches') %>% html_text(), 
    'first_committee' = the_page %>% html_element('#committee-work tr:nth-child(2) td:nth-child(1)') %>% html_text(), 
    'second_committee' = the_page %>% html_element('#committee-work tr~ tr+ tr td:nth-child(1)') %>% html_text(), 
    'vote_share' = the_page %>% html_element('#election-results tr:nth-child(2) td~ td+ td') %>% html_text(), 
    'winning_margin' = the_page %>% html_element('#margin') %>% html_text(), 
    'challenger_party' = the_page %>% html_element('#election-results tr:nth-child(3) td:nth-child(1)') %>% html_text(), 
    'challenger_share' = the_page %>% html_element('#election-results tr:nth-child(3) td~ td+ td') %>% html_text(),
    'email' = the_page %>% html_element('#email a') %>% html_text(), 
    'phone' = the_page %>% html_element('#phone span') %>% html_text(), 
    'website' = the_page %>% html_element('#website a') %>% html_text(), 
    'prefer' = the_page %>% html_element('#contact-preference') %>% html_text()
  )
  temp_list[[i]] <- temp_tibble
}
df_member <- do.call(rbind, temp_list)



# read_html('https://finance.yahoo.com/quote/SPY/history?p=SPY') %>% 
#   html_element(css='#Col1-1-HistoricalDataTable-Proxy > section > div.Pb\(10px\).Ovx\(a\).W\(100\%\) > table')


paths_allowed(domain = 'https://www.quiverquant.com/')
read_html('https://www.quiverquant.com/') %>% html_elements('.table-hot-strats td:nth-child(1)') %>% html_text()
page <- read_html('https://www.quiverquant.com/congresstrading/')
page %>% html_elements('.table-politician p , .flex-column , th') %>% html_table()

page <- read_html('https://www.haifeng-xu.com/cmsc35401fa22/')
links <- tibble(
  'link' = page %>% 
    html_elements(css = 'a') %>% 
    html_attr('href') %>% 
    url_absolute(base = 'http://www.haifeng-xu.com/cmsc35401fa22/'))
a <- links %>% filter(str_detect(link, 'slides'))
for (i in seq_along(a$link)) {
  download.file(url = a$link[i], destfile = paste0('./data/', basename(a$link[i])), mode = 'wb')
}


pdf_link <- bow('https://luzpar.netlify.app/documents/human-rights-2021/') %>%
  scrape() %>%
  html_elements(css = ".btn-page-header") %>% 
  html_attr("href") %>% 
  url_absolute(base = "https://luzpar.netlify.app/")
download.file(url = pdf_link, destfile = basename(pdf_link), mode = "wb")

page %>% html_elements(css = 'table') %>% .[[1]] %>% 
  html_table() %>% 
  as_tibble()
page %>% 
  html_nodes('table') %>% 
  html_table()


page %>% html_elements(css = 'td:nth-of-type(2) a')

read_html('https://coinmarketcap.com/all/views/all/') %>% 
  html_elements('table') %>% 
  html_table()

read_html('https://www.douban.com/group/721013/') %>% 
  html_elements(css='table') %>% 
  html_table()

read_html('https://www.douban.com/group/721013/') %>% 
  html_elements('td:nth-child(1) , .title a') %>% 
  html_attrs()


##
page <- read_html('https://violationtracker.goodjobsfirst.org/pages/violation-tracker-data-sources')
vt_data_sources <- tibble(
  'federal_agency_data_terms' = page %>% html_elements('p+ p') %>% html_text() %>% 
    .[!str_detect(., 'FOIA')], 
  'link' = page %>% html_elements('.font-body a') %>% html_attr('href')
) 
vt_data_sources %>% 
  mutate(str_split(federal_agency_data_terms, ': '))

page %>% html_elements('p+ p') %>% 
  html_attr('href')

## dynamic page ----
rd <- rsDriver(chromever = NULL)
rd$client$close()
rd$server$stop()
binman::list_versions("chromedriver")
appdir <- binman::app_dir('chromedriver', FALSE)

rd <- rsDriver(chromever = NULL)
browser <- rd$client
server <- rd$server
browser$navigate('https://luzpar.netlify.app')
browser$goBack()
browser$goForward()
browser$refresh()
browser$maxWindowSize()
browser$screenshot(display = TRUE)
browser$getCurrentUrl()
browser$getTitle()
browser$close()
browser$open()
browser$getPageSource()[[1]] %>% read_html() %>% 
  html_elements('#title a') %>% 
  html_attr('href')

read_html('https://luzpar.netlify.app/members/') %>% 
  html_elements('td:nth-child(1)') %>% 
  html_text()

browser$navigate('https://luzpar.netlify.app/members/')
browser$getPageSource()[[1]] %>% read_html() %>% 
  html_elements('td:nth-child(1) a') %>% 
  html_text()
browser$navigate('https://luzpar.netlify.app/')
menu_states <- browser$findElement(using = "link text", value = "states")
menu_states$highlightElement()

search_icon <- browser$findElement(using = "css", value = ".fa-search")
search_icon$clickElement()

browser$navigate('https://luzpar.netlify.app/constituencies/')
next_page <- browser$findElement(using = 'css', value = '.page-link')
next_page$clickElement()
next_page2 <- browser$findElement(using = 'css', value = '.page-item+ .page-item .page-link')
next_page2$clickElement()

as_tibble(selKeys) %>% names()

browser$navigate("http://luzpar.netlify.app/")
search_icon <- browser$findElement(using = "css", value = ".fa-search")
search_icon$clickElement()
Sys.sleep(2)
search_bar <- browser$findElement(using = "css", value = "#search-query")
search_bar$clickElement()
search_bar$sendKeysToElement(list(value = "Law", key = "enter"))
search_bar$clearElement()

browser$navigate('https://duckduckgo.com/')
search_bar <- browser$findElement(using = 'css', value = '#search_form_input_homepage')
search_bar$clickElement()
search_bar$sendKeysToElement(list(value = "https://luzpar.netlify.app/", key = "enter"))
body <- browser$findElement(using = "css", value = "body")
body$sendKeysToElement(list(key = "page_down"))

#use javascript
#scroll down
body$executeScript("window.scrollBy(0, 1000);")
#hightlight
browser$executeScript("arguments[0].style.border='3px solid red'", list(body))

search_bar <- browser$findElement('css', '#search_form_input')
search_bar$clearElement()
search_bar$clickElement()
search_bar$sendKeysToElement(list(value = 'https://jiazhang42.github.io/mysite/', key = 'enter'))

browser$navigate("https://luzpar.netlify.app/documents/")
Sys.sleep(4)
app_frame <- browser$findElement('css', 'iframe')
browser$switchToFrame(Id = app_frame)
# browser$switchToFrame(Id = NULL)
drop_down <- browser$findElement(using = "css", value = ".bs-placeholder")
drop_down$clickElement()

proposal <- browser$findElement(using = 'css', "[id='bs-select-1-1']")
proposal$clickElement()

report <- browser$findElement(using = 'css', "[id='bs-select-1-2']")
report$clickElement()
drop_down$clickElement()
the_links <- browser$getPageSource()[[1]] %>% 
  read_html() %>% 
  html_elements('td a') %>% 
  html_attr('href')
the_links

for (i in 1:length(the_links)) {
  pdf_link <- read_html(the_links[i]) %>%
    html_elements(css = ".btn-page-header") %>% 
    html_attr("href") %>% 
    url_absolute(base = "https://luzpar.netlify.app/")
  download.file(url = pdf_link, destfile = basename(pdf_link), mode = "wb")
}

browser$navigate("https://luzpar.netlify.app/documents/")
Sys.sleep(4)
app_frame <- browser$findElement('css', 'iframe')
browser$switchToFrame(Id = app_frame)
drop_down <- browser$findElement('css', '.caret')
drop_down$clickElement()
law <- browser$findElement('css', '#bs-select-1-0')
law$clickElement()
proposal <- browser$findElement('css', '#bs-select-1-1')
proposal$clickElement()
drop_down$clickElement()
year <- browser$findElement('css', '.checkbox~ .checkbox+ .checkbox span')
year$clickElement()

the_links <- browser$getPageSource()[[1]] %>% 
  read_html() %>% 
  html_elements('td a') %>% 
  html_attr('href')
i <- 1
temp_list <- list()
for (i in seq_along(the_links)) {
  the_page <- read_html(the_links[i])
  tem_table <- tibble(
    'the_link' = the_links[i], 
    'article_tags' = the_page %>% html_elements('.article-categories a') %>% 
      html_text(), 
    'image_credits' = the_page %>% html_elements('.article-header-caption') %>% 
      html_text()
  )
  temp_list[[i]] <- tem_table
}
ex_table <- do.call(rbind, temp_list)
browser$close()


## 
