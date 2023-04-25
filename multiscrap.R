library(dplyr)
library(stringr)
library(rvest)
library(lubridate)
library(calendar)
library(magrittr)
pages_a_scraper<-read.csv("https://raw.githubusercontent.com/Valexandre/dates_publications/main/pagesascraper.csv")

RajouteAnneeADate<-function(chainedecaracteres){
  datedujour<-Sys.Date()
  justedate<-strsplit(chainedecaracteres,split = "à",2)[[1]][1]
  justeheure<-strsplit(chainedecaracteres,split = "à",2)[[1]][2]
  bonnejusteheure<-ifelse(substr(justeheure,nchar(justeheure),nchar(justeheure))=="h",gsub("h",":00:00",justeheure),paste0(gsub("h",":",justeheure),":00"))
  bonnejustedate<-dmy(paste0(justedate,substr(Sys.Date(),1,4)))
  retourchainecaracteres<-gsub("  "," ",ifelse(bonnejustedate<Sys.Date(),paste0(dmy(paste0(justedate,(as.numeric(substr(Sys.Date(),1,4))+1))),bonnejusteheure),paste0(bonnejustedate," ",bonnejusteheure)))
  ymd_hms(retourchainecaracteres)
}
RajouteAnneeADate(boites$date[1])

sors_les_publis_insee<-function(){
  pageindic<-read_html(pages_a_scraper$lien[pages_a_scraper$qui=="insee" & pages_a_scraper$quoi=="publications"])
  boites <- pageindic %>%
    html_elements("#produit-tableau-prog") %>%
      html_table()%>%
    .[[1]]%>%
    rename(categorie=1,element=2,date=3)%>%
    filter(!is.na(element))%>%
    filter(element!="")%>%
    filter(date!="DATE D'EMBARGO")%>%
    rowwise()%>%
    mutate(date_embargo=RajouteAnneeADate(chainedecaracteres=date))
  boites
}
Insee<-sors_les_publis_insee()

###################
# scrapper ssmsi
MetLesDatesSansHeureA8h<-function(chainedecaracteres){
  tmp<-paste0(chainedecaracteres," 08:00:00")
  bonneheure<-dmy_hms(tmp)
  bonneheure
}

sors_les_publis_ssmsi<-function(){
  pageindic<-read_html(pages_a_scraper$lien[pages_a_scraper$qui=="SSMSI" & pages_a_scraper$quoi=="tout"])
  etudes_ssmsi <- pageindic %>%
    html_elements("p")%>%html_text()
  dates_etudes <- pageindic %>%
    html_elements(xpath = '//*[@id="col_principale"]/div[1]/div/div[3]')%>%
    html_elements("ul")%>%
    html_elements("li")%>%html_text()
  
  etudes_ssmsi_ok<-etudes_ssmsi[4:length(etudes_ssmsi)]
  dates_etudes_ok<-dates_etudes[5:length(dates_etudes)]
  
  SSMSI<-tibble(element=etudes_ssmsi_ok,
                date=dates_etudes_ok,
                categorie="SSMSI")%>%
    select(categorie,element,date)%>%
    rowwise()%>%
    mutate(date_embargo=MetLesDatesSansHeureA8h(date))
  SSMSI%>%filter(!is.na(date_embargo))
}
SSMSI<-sors_les_publis_ssmsi()


########
# Dares

NettoieLesDatesDares<-function(chainedecaracteres){
  tmp <- gsub("\n                ", " ", chainedecaracteres)
  tmp <- gsub("  ", " ", tmp)
  datedujour <- Sys.Date()
  justedate <- strsplit(tmp, split = "  ", 2)[[1]][1]
  justeheure <- str_extract(tmp,pattern = "\\d\\d:\\d\\d")
  bonnejustedate <- dmy(paste0(justedate, substr(Sys.Date(), 1, 4)))
  retourchainecaracteres <-
    gsub("  ", " ", ifelse(
      bonnejustedate < Sys.Date(),
      paste0(dmy(paste0(
        justedate, (as.numeric(substr(Sys.Date(
        ), 1, 4)) + 1)
      )), justeheure),
      paste0(bonnejustedate, " ", justeheure,":00")
    ))
  ymd_hms(retourchainecaracteres)
}


sors_les_publis_dares<-function(){
  pageindic<-read_html(pages_a_scraper$lien[pages_a_scraper$qui=="dares" & pages_a_scraper$quoi=="tout"])
  dates_etudes <- pageindic %>%
    html_elements(".release-calendar-item")%>%
    html_elements(".release-calendar-date")%>%
    html_text()%>%trimws(.,which = "both")
  titre_etudes <- pageindic %>%
    html_elements(".release-calendar-item")%>%
    html_elements(".release-calendar-article-title")%>%
    html_text()
  
  Dares_tmp<-tibble(element=titre_etudes,
                date=dates_etudes,
                categorie="Dares")%>%
    select(categorie,element,date)%>%
    rowwise()%>%
    mutate(date_embargo=NettoieLesDatesDares(date))%>%
    filter(!is.na(date_embargo))
}

DARES<-sors_les_publis_dares()

AllIndicateurs<-rbind(Insee,SSMSI,DARES)

#Envoie des maj des evenements
events= ic_event(start = AllIndicateurs$date_embargo[1], end = AllIndicateurs$date_embargo[1]+hours(1) , 
                 summary = paste0(AllIndicateurs$categorie[1],": ",AllIndicateurs$element[1]))
for( i in 2:nrow(Insee)){
  tmp<-ic_event(start = AllIndicateurs$date_embargo[i], end = AllIndicateurs$date_embargo[i]+hours(1) , 
                summary = paste0(AllIndicateurs$categorie[i],": ",AllIndicateurs$element[i]))
  events<-rbind(events,tmp)
}

ic_write(events, file.path(getwd(), "ic_insee.ics"))
