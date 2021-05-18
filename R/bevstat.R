
#---------------------------------------------------------------------------------------------------------------
#Aufgabe 1 Re-export von %>%

#' re-export magrittr pipe operator
#' @description This line of code imports and re-exports the %>% operator from the package magrittr. Usage of oparator: \code{\link{%>%}}
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL

#---------------------------------------------------------------------------------------------------------------
### AUfgabe 2

#' copies finished Dataset in global environment
#' @description This function downloads the Data.frame from the URL: https://data.statistik.gv.at/web/meta.jsp?dataset=OGD_bevstandjbab2002_BevStand_2020.
#' The names of the collumns are automatically assigned. Additionally all codes are replaced with their corresponding labels.
#' The Data types are set, so they can be used for statistical analysis
#' @return A Dataframe which is in working condition
#' @importFrom stringi stri_extract_last_regex
#' @import dplyr
#' @export
#' @examples
#' read_bevstat()
read_bevstat <- function() {
  bevoelkerungsdaten <- read.csv(url("https://data.statistik.gv.at/data/OGD_bevstandjbab2002_BevStand_2020.csv"),header = FALSE, sep = ';', skip = 1, check.names=FALSE)


  ### bennenung der Daten


  names(bevoelkerungsdaten) <- c('time','sex','commune_code','age_code','number')


  ### ANpassen des Datentyps




  bevoelkerungsdaten$time <- stri_extract_last_regex(bevoelkerungsdaten$time, "\\d{4}")
  bevoelkerungsdaten$age_code <- as.character(bevoelkerungsdaten$age_code)


  ###temporärer import der tabelle für den alterscode und kommunencode


  alter <- read.csv(url("https://data.statistik.gv.at/data/OGD_bevstandjbab2002_BevStand_2020_C-GALTEJ112-0.csv"),header = FALSE, sep = ';', skip = 1, check.names=FALSE)
  kommune <- read.csv2(url("https://data.statistik.gv.at/data/OGD_bevstandjbab2002_BevStand_2020_C-GRGEMAKT-0.csv"), check.names=FALSE)


  ###ANpassung der temporären Tabelle  für das alter

  alter<- subset(alter, select = -c(V4,V5,V6,V7,V8,V9,V10))
  colnames(alter)<- c('age_code','age','age_cat')

  alter$age_cat[alter$age_cat == "GALT5J100-1"] <- 'Age 0 to 4'
  alter$age_cat[alter$age_cat == "GALT5J100-2"] <- 'Age 5 to 9'
  alter$age_cat[alter$age_cat == "GALT5J100-3"] <- 'Age 10 to 14'
  alter$age_cat[alter$age_cat == "GALT5J100-4"] <- 'Age 15 to 19'
  alter$age_cat[alter$age_cat == "GALT5J100-5"] <- 'Age 20 to 24'
  alter$age_cat[alter$age_cat == "GALT5J100-6"] <- 'Age 25 to 29'
  alter$age_cat[alter$age_cat == "GALT5J100-7"] <- 'Age 30 to 24'
  alter$age_cat[alter$age_cat == "GALT5J100-8"] <- 'Age 35 to 39'
  alter$age_cat[alter$age_cat == "GALT5J100-9"] <- 'Age 40 to 44'
  alter$age_cat[alter$age_cat == "GALT5J100-10"] <- 'Age 45 to 49'
  alter$age_cat[alter$age_cat == "GALT5J100-11"] <- 'Age 50 to 54'
  alter$age_cat[alter$age_cat == "GALT5J100-12"] <- 'Age 55 to 59'
  alter$age_cat[alter$age_cat == "GALT5J100-13"] <- 'Age 60 to 64'
  alter$age_cat[alter$age_cat == "GALT5J100-14"] <- 'Age 65 to 69'
  alter$age_cat[alter$age_cat == "GALT5J100-15"] <- 'Age 70 to 74'
  alter$age_cat[alter$age_cat == "GALT5J100-16"] <- 'Age 75 to 79'
  alter$age_cat[alter$age_cat == "GALT5J100-17"] <- 'Age 80 to 84'
  alter$age_cat[alter$age_cat == "GALT5J100-18"] <- 'Age 85 to 89'
  alter$age_cat[alter$age_cat == "GALT5J100-19"] <- 'Age 90 to 94'
  alter$age_cat[alter$age_cat == "GALT5J100-20"] <- 'Age 95 to 99'
  alter$age_cat[alter$age_cat == "GALT15J75-6"] <- 'Age 100+'

   ### Anpassung der temporären tabelle für die kommune


  colnames(kommune)[colnames(kommune)=='code'] <- 'commune_code'
  colnames(kommune)[colnames(kommune)=='name'] <- 'commune'
  kommune[3:10] <- NULL

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
  bevoelkerungsdaten$commune<- gsub('Ã–', 'Oe', bevoelkerungsdaten$commune)
  bevoelkerungsdaten$commune<- gsub('Ä', 'Ae', bevoelkerungsdaten$commune)
  bevoelkerungsdaten$commune<- gsub('Ãœ', 'Ue', bevoelkerungsdaten$commune)
  bevoelkerungsdaten$commune<- gsub('Ã¶', 'oe', bevoelkerungsdaten$commune)
  bevoelkerungsdaten$commune<- gsub('Ã¤', 'ae', bevoelkerungsdaten$commune)
  bevoelkerungsdaten$commune<- gsub('Ã¼', 'ue', bevoelkerungsdaten$commune)
  bevoelkerungsdaten$commune<- gsub('ÃŸ', 'ss', bevoelkerungsdaten$commune)
  bevoelkerungsdaten$commune <- factor(bevoelkerungsdaten$commune)
  ###Anpassung geschlecht


  bevoelkerungsdaten[bevoelkerungsdaten == 'C11-1'] <- 'male'
  bevoelkerungsdaten[bevoelkerungsdaten == 'C11-2'] <- 'female'
  bevoelkerungsdaten$sex <- factor(bevoelkerungsdaten$sex)
  ###output tabelle


  assign('bevoelkerungsdaten', data.frame(bevoelkerungsdaten), envir = .GlobalEnv)
}
#####------------------------------------------------------------------------------------------------------
### Aufgabe 3
#' calculating age
#' @description
#' calculates the relative age for each commune and outputs it in a table in the global environment. Also gives the mean age.
#'
#' @return two tables, one for age distribution and one for mean age
#' @importFrom plyr ddply
#' @importFrom scales label_percent
#' @export
calculate_age <- function() {


  ### 1. Teil der Aufgabe


  age_rel <- as.data.frame.matrix(xtabs( ~commune + age_cat , data=bevoelkerungsdaten))
  age_rel <- t(apply(age_rel, 1,  FUN = function(i) label_percent()(i/sum(i))))
  age_rel <- as.data.frame(age_rel)
  age_rel <- cbind(rownames(age_rel), age_rel)
  rownames(age_rel) <- NULL
  assign('age_rel', as.data.frame.matrix(age_rel), envir = .GlobalEnv)
  colnames(age_rel)[1]<- c("commune")

  ### 2. Teil der Aufgabe


  bevfilter <- dplyr::filter(bevoelkerungsdaten, age <100)
  age_per_commune <- ddply (bevfilter, .(commune), function(x) mean(x$age))
  colnames(age_per_commune) <- c('commune', 'mean age')
  age_per_commune$`mean age` <- round(age_per_commune$`mean age`, digits = 1)
  assign('age_per_commune', data.frame(age_per_commune), envir = .GlobalEnv)

  }

### Output Funktion gibt für eine Kommune


#' prints row in age distribution table
#' @description
#' writting the "katestral" code of the commune into the function will output the relative age distribution as well as the mean age in the commune
#' @param y
#'
#' @return relative age distribution and mean age
#' @export
#'
#' @example
#' \dontrun{
#' print_row(21001)
#' }
#'
print_row <- function(y){
  age_rel <- as.data.frame(age_rel)
  age_rel <- merge(age_rel, age_per_commune,by='commune')
  rowx <-as.data.frame(age_rel[which(grepl(y,age_rel$commune)),])
  row.names(rowx) <- rowx$commune
  rowx[,1] <- NULL
  rowx <- as.data.frame(t(rowx))
  as.numeric(rowx[,1])
  assign('rel_age_commune', data.frame(rowx), envir = .GlobalEnv)
  print(rel_age_commune)
    }
#####--------------------------------------------------------------------------------------------------------
####Aufgabe 4

#' scatterplot alter pro Gemeinde
#' @description outputs a scatterplot with chil
#' @return
#' @importFrom  stringr str_sub
#' @import ggplot2
#' @export
#' @examples
#' scatter()
scatter <- function(){
  ### Generierung der Variablen Kommunen Größe und Kind/Erwachsenen ratio
  size_commune <- data.frame(size = sample(summary(bevoelkerungsdaten$commune, maxsum = 2117)))
  bevfilter <- dplyr::filter(bevoelkerungsdaten, age <6)
  size_commune$child <- (summary(bevfilter$commune, maxsum = 2117))
  size_commune$relsize <- size_commune$child/size_commune$size
  size_commune$commune <- row.names(size_commune)
  size_commune$bundesland <- str_sub(size_commune$commune, start= -7)
  size_commune$bundesland <- as.numeric(gsub(".*?([0-9]+).*", "\\1", size_commune$bundesland))
  size_commune$bundesland <- floor(size_commune$bundesland/10000)
  size_commune$bundesland[size_commune$bundesland == 1] <- 'Burgenland'
  size_commune$bundesland[size_commune$bundesland == 2] <- 'Kaernten'
  size_commune$bundesland[size_commune$bundesland == 3] <- 'Niederoesterreich'
  size_commune$bundesland[size_commune$bundesland == 4] <- 'Oberoesterreich'
  size_commune$bundesland[size_commune$bundesland == 5] <- 'Salzburg'
  size_commune$bundesland[size_commune$bundesland == 6] <- 'Steiermark'
  size_commune$bundesland[size_commune$bundesland == 7] <- 'Tirol'
  size_commune$bundesland[size_commune$bundesland == 8] <- 'Voralberg'
  size_commune$bundesland[size_commune$bundesland == 9] <- 'Wien'
  size_commune$bundesland <- as.factor(size_commune$bundesland)
  size_commune$commune <- as.factor(size_commune$commune)
  age_per_commune <- merge(age_per_commune,size_commune, by= 'commune')


  test <- ggplot(age_per_commune, aes(x = mean.age, y = relsize, color= bundesland))+
    geom_point(aes(size = size)) +
    theme_classic()

  test <- test+
    xlab("Mean Age")+
    ylab("Child/Adult ratio")+
    labs(title = "Mean Age X Child to Adult Ratio")
  assign('scatter', plot(test), envir = .GlobalEnv)
  test
}
