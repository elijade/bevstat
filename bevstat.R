
#---------------------------------------------------------------------------------------------------------------
#Re-export von %>%

#' re-export magrittr pipe operator
#' @description This line of code imports and re-exports the %>% operator from the package magrittr. Usage of oparator: \code{\link{%>%}}
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL

#---------------------------------------------------------------------------------------------------------------


#' copies finished Dataset in global environment
#' @description This function downloads the Data.frame from the URL: https://data.statistik.gv.at/web/meta.jsp?dataset=OGD_bevstandjbab2002_BevStand_2020.
#' The names of the collumns are automatically assigned. Additionally all codes are replaced with their corresponding labels.
#' The Data types are set, so they can be used for statistical analysis
#' @return A Dataframe which is in working condition
#' @importFrom stringi stri_extract_last_regex
#' @import dplyr
#' @export
#'
#' @examples
read_bevstat <- function() {
  bevoelkerungsdaten <- read.csv(url("https://data.statistik.gv.at/data/OGD_bevstandjbab2002_BevStand_2020.csv"),header = FALSE, sep = ';', skip = 1)


  ### bennenung der Daten


  names(bevoelkerungsdaten) <- c('time','sex','commune_code','age_code','number')


  ### ANpassen des Datentyps




  bevoelkerungsdaten$time <- stri_extract_last_regex(bevoelkerungsdaten$time, "\\d{4}")
  bevoelkerungsdaten$age_code <- as.character(bevoelkerungsdaten$age_code)


  ###temporärer import der tabelle für den alterscode und kommunencode


  alter <- read.csv(url("https://data.statistik.gv.at/data/OGD_bevstandjbab2002_BevStand_2020_C-GALTEJ112-0.csv"),header = FALSE, sep = ';', skip = 1)
  kommune <- read.csv2(url("https://data.statistik.gv.at/data/OGD_bevstandjbab2002_BevStand_2020_C-GRGEMAKT-0.csv"))
  ###ANpassung der temporären Tabelle  für das alter


  colnames(alter)[colnames(alter)=='V1'] <- 'age_code'
  colnames(alter)[colnames(alter)=='V2'] <- 'age'
  alter<- subset(alter, select = -c(V3,V4,V5,V6,V7,V8,V9,V10))

  ### Anpassung der temporären tabelle für die kommune


  colnames(kommune)[colnames(kommune)=='code'] <- 'commune_code'
  colnames(kommune)[colnames(kommune)=='name'] <- 'commune'
  kommune<- subset(kommune, select = -c(X, en_name, de_desc, de_link, en_desc, en_link, de_syn, en_syn))

  ###merge der Tabellen alter


  bevoelkerungsdaten <- merge(bevoelkerungsdaten,alter, by= 'age_code')


  ### Alter wird angepasst (Jahre entfernt und zur zahl gemacht MERKE 100=100+ muss also ausgeschlossen werden für analysen)


  bevoelkerungsdaten$age <- as.numeric(gsub("([0-9]+).*$", "\\1", bevoelkerungsdaten$age))


  ###merge mit der tabelle für commune


  bevoelkerungsdaten <- merge(bevoelkerungsdaten,kommune, by= 'commune_code')


  ### entfernen der merge variable


  bevoelkerungsdaten <- subset(bevoelkerungsdaten, select = -c(commune_code))
  bevoelkerungsdaten <- subset(bevoelkerungsdaten, select = -c(age_code))


  ### Anpassung der commune variable


  bevoelkerungsdaten$commune <- factor(bevoelkerungsdaten$commune)


  ###Anpassung geschlecht


  bevoelkerungsdaten[bevoelkerungsdaten == 'C11-1'] <- 'male'
  bevoelkerungsdaten[bevoelkerungsdaten == 'C11-2'] <- 'female'
  bevoelkerungsdaten$sex <- factor(bevoelkerungsdaten$sex)
  ###output tabelle


  assign('bevoelkerungsdaten', data.frame(bevoelkerungsdaten), envir = .GlobalEnv)
}
