library(dplyr)
library(stringr)
library(rvest)
library(lubridate)
library(calendar)
library(magrittr)
library(purrr)
pages_a_scraper <-
  read.csv(
    "https://raw.githubusercontent.com/Valexandre/dates_publications/main/pagesascraper.csv"
  )

RajouteAnneeADate <- function(chainedecaracteres) {
  datedujour <- Sys.Date()
  justedate <- strsplit(chainedecaracteres, split = "à", 2)[[1]][1]
  justeheure <- strsplit(chainedecaracteres, split = "à", 2)[[1]][2]
  bonnejusteheure <-
    ifelse(
      substr(justeheure, nchar(justeheure), nchar(justeheure)) == "h",
      gsub("h", ":00:00", justeheure),
      paste0(gsub("h", ":", justeheure), ":00")
    )
  bonnejustedate <-
    dmy(paste0(justedate, substr(Sys.Date(), 1, 4)), locale = "fr_FR")
  retourchainecaracteres <-
    gsub("  ", " ", ifelse(
      bonnejustedate < Sys.Date(),
      paste0(dmy(paste0(
        justedate, (as.numeric(substr(Sys.Date(
        ), 1, 4)) + 1)
      ),locale = "fr_FR"), bonnejusteheure),
      paste0(bonnejustedate, " ", bonnejusteheure)
    ))
  ymd_hms(retourchainecaracteres, locale = "fr_FR")
}

rien<-boites<-tibble(categorie = "",
                     element = "",
                     date = "",
                     date_embargo="")
sors_les_publis_insee <- function() {
  pageindic <-
    read_html(pages_a_scraper$lien[pages_a_scraper$qui == "insee" &
                                     pages_a_scraper$quoi == "publications"])
  boites<-tibble(categorie = "",
                 element = "",
                 date = "")
  boites <- pageindic %>%
    html_elements("#produit-tableau-prog") %>%
    html_table() %>%
    .[[1]] 
  
  boites<-boites[,1:3]%>%
    rename(categorie = 1,
           element = 2,
           date = 3) %>%
    filter(!is.na(element)) %>%
    filter(element != "") %>%
    filter(date != "DATE D'EMBARGO") %>%
    rowwise() %>%
    mutate(date_embargo = case_when(nrow(boites)>1~RajouteAnneeADate(chainedecaracteres = date),
                                    TRUE~""))
  print(paste0("Insee : ",boites$element))
  return(boites)
}
safeinsee<-possibly(sors_les_publis_insee,otherwise = rien,quiet = TRUE)
Insee <- safeinsee()

###################
# scrapper ssmsi
MetLesDatesSansHeureA8h <- function(chainedecaracteres) {
  tmp <- paste0(chainedecaracteres, " 08:00:00")
  bonneheure <- dmy_hms(tmp, locale = "fr_FR")
  bonneheure
}


sors_les_publis_ssmsi <- function() {
  pageindic <-
    read_html(pages_a_scraper$lien[pages_a_scraper$qui == "SSMSI" &
                                     pages_a_scraper$quoi == "tout"])
  etudes_ssmsi <- pageindic %>%
    html_elements("p") %>% html_text()
  dates_etudes <- pageindic %>%
    html_elements(xpath = '//*[@id="col_principale"]/div[1]/div/div[3]') %>%
    html_elements("ul") %>%
    html_elements("li") %>% html_text()
  
  val_min<-min(c(length(etudes_ssmsi),length(dates_etudes)))
  valmax<-max(c(length(etudes_ssmsi),length(dates_etudes)))
  
  etudes_ssmsi_ok <- etudes_ssmsi[(length(etudes_ssmsi)-5):length(etudes_ssmsi)]
  dates_etudes_ok <- dates_etudes[(length(dates_etudes)-5):length(dates_etudes)]
  SSMSI <- tibble(element = etudes_ssmsi_ok,
                  date = dates_etudes_ok,
                  categorie = "SSMSI") %>%
    select(categorie, element, date) %>%
    rowwise() %>%
    mutate(date_embargo = MetLesDatesSansHeureA8h(date))
    
  SSMSI %>% filter(!is.na(date_embargo))
}
SSMSI <- sors_les_publis_ssmsi()


########
# Dares

NettoieLesDatesDares <- function(chainedecaracteres) {
  tmp <- gsub("\n                ", " ", chainedecaracteres)
  tmp <- gsub("  ", " ", tmp)
  datedujour <- Sys.Date()
  justedate <- strsplit(tmp, split = "  ", 2)[[1]][1]
  justeheure <- str_extract(tmp, pattern = "\\d\\d:\\d\\d")
  bonnejustedate <-
    dmy(paste0(justedate, substr(Sys.Date(), 1, 4)), locale = "fr_FR")
  retourchainecaracteres <-
    gsub("  ", " ", ifelse(
      bonnejustedate < Sys.Date(),
      paste0(dmy(paste0(
        justedate, (as.numeric(substr(Sys.Date(
          
        ), 1, 4)) + 1)
      ), locale = "fr_FR"), justeheure),
      paste0(bonnejustedate, " ", justeheure, ":00")
    ))
  ymd_hms(retourchainecaracteres, locale = "fr_FR")
}


sors_les_publis_dares <- function() {
  pageindic <-
    read_html(pages_a_scraper$lien[pages_a_scraper$qui == "dares" &
                                     pages_a_scraper$quoi == "tout"])
  dates_etudes <- pageindic %>%
    html_elements(".release-calendar-item") %>%
    html_elements(".release-calendar-date") %>%
    html_text() %>% trimws(., which = "both")
  titre_etudes <- pageindic %>%
    html_elements(".release-calendar-item") %>%
    html_elements(".release-calendar-article-title") %>%
    html_text()
  
  Dares_tmp <- tibble(element = titre_etudes,
                      date = dates_etudes,
                      categorie = "Dares") %>%
    select(categorie, element, date) %>%
    rowwise() %>%
    mutate(date_embargo = NettoieLesDatesDares(date)) %>%
    filter(!is.na(date_embargo))
  Dares_tmp
}

DARES <- sors_les_publis_dares()

#Envoie des maj des evenements
# rajotus eurostat
pageeurostat<-"https://ec.europa.eu/eurostat/fr/news/release-calendar"
PageChargee<-read_html(pageeurostat)

# on prend le début et la fin du json
Alltxt<-PageChargee%>%html_text()

fin<-str_locate_all(Alltxt,"window.location.href")
debut<-str_locate_all(Alltxt,"fullEvents")

VersionJson<-substr(Alltxt,(debut[[1]][7,2])+3 , (fin[[1]][2,1])-23 )
substr(VersionJson,1,30)

jsoncars<-str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(str_replace_all(
  str_replace_all(iconv(str_replace_all(str_replace_all(str_replace_all(VersionJson,"\\\\n" , ""),"\\\\t",""),"’",""),"UTF-8", "UTF-8",sub=''),'recordid:','"recordid":'),'title:','"title":'),'theme:','"theme":'),'period:','"period":'),'types:','"types":'),'preliminary:','"preliminary":'),'start:','"start":'),'end:','"end":'),'unit:','"unit":'),'euroindAuthor:','"euroindAuthor":'),'allDay:','"allDay":')
Encoding(jsoncars) <- "UTF-8"

jsoncars<-str_replace_all(str_replace_all(str_replace_all(str_replace_all(jsoncars,"'",'"'),"\\},\\]","}]"),"  \\]","]"),"  \\]","]")
jsoncars<-substr(jsoncars,1,nchar(jsoncars)-10)

#locate the last comma after } to remove it
douquelleestlavirgule<-str_locate_all(substr(jsoncars,(nchar(jsoncars)-40),nchar(jsoncars)),"\\},")
virg<-unlist(douquelleestlavirgule)[1]


jsoncars<-paste0(substr(jsoncars,1,((nchar(jsoncars)-40)+(virg-1))),"]")

# on veut enlever la virgule entre la dernière accolade fermante et l'accolade qu'on a rajouté

jsoncars<-str_replace_all(str_replace_all(str_replace_all(str_replace_all(jsoncars,"'",'"'),"\\},\\]","}]"),"  \\]","]"),"  \\]","]")

jsoncars<-str_replace_all(jsoncars,'\\/\\/\\"end\\"','"end"')
jsoncars<-str_replace_all(jsoncars,"  "," ")
jsoncars<-str_replace_all(jsoncars,'"Regional aggregates by typologies (.*)",','"Regional aggregates by typologies",')


fileConn<-file("agendaeurostat.json")
writeLines(jsoncars,fileConn)
close(fileConn)


AgendaEurostat<-jsonlite::fromJSON("agendaeurostat.json")

EuroStatFin<-AgendaEurostat%>%
  filter(start >= Sys.Date())

Eurostat_tmp <- tibble(element = paste0(EuroStatFin$theme," - ",EuroStatFin$title),
                    date = EuroStatFin$start,
                    categorie = "Eurostat") %>%
  select(categorie, element, date) %>%
  rowwise() %>%
  mutate(date_embargo =date) %>%
  filter(!is.na(date_embargo))




AllIndicateurs <- rbind(Insee, SSMSI, DARES,Eurostat_tmp)%>%filter(!is.na(date_embargo))

#Envoie des maj des evenements
events = ic_event(
  start = AllIndicateurs$date_embargo[1],
  end = AllIndicateurs$date_embargo[1] + hours(1) ,
  summary = paste0(AllIndicateurs$categorie[1], ": ", AllIndicateurs$element[1])
)

for (i in 2:nrow(AllIndicateurs)) {
  tmp <-
    ic_event(
      start = AllIndicateurs$date_embargo[i],
      end = AllIndicateurs$date_embargo[i] + hours(1) ,
      summary = paste0(AllIndicateurs$categorie[i], ": ", AllIndicateurs$element[i])
    )
  events <- rbind(events, tmp)
}

ic_write(events, "ic_insee.ics")
