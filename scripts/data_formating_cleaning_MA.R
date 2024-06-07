# Data formating and cleaning for MA thesis MEG
# Dani Gargya
# June 24

# loading libraries ----
library(tidyverse)
library(ggplot2)
library(dplyr) 
library(haven) # to transform data from sav to csv
library(readr) # to transform data from sav to csv

# workflow ----
# data conversion Lisa
# import new data and formating (from webside)
# allocate groups (0,1,2)
# data cleaning
### exclude imcomplete data?
### exclude data with more than 25% missing (6 answers)
### exclude data that took less than 4 minutes
# data transformation
### scaling according to Lisa (0-3, instead of 1-4)
### inverse scales to reflect meaning


# data conversion Lisas data from sav to csv ----
### import and convert lisas data from sav to csv
#pre angell
lisa_data_angell_pre <- read_sav("data/data_collection/data_pauli/ANGELL_PRE_anonym.SAV")
write_csv(x=lisa_data_angell_pre, path="data/data_collection/data_pauli/angell_pre.csv")

#post angell
lisa_data_angell_post <- read_sav("data/data_collection/data_pauli/ANGELL_POST_anonym.SAV")
write_csv(x=lisa_data_angell_post, path="data/data_collection/data_pauli/angell_post.csv")

## not needed
# pre goethe
#lisa_data_goethe_pre <- read_sav("data/data_collection/data_pauli/GOETHE_PRE_anonym.SAV")
#write_csv(x=lisa_data_goethe_pre, path="data/data_collection/data_pauli/goethe_pre.csv")

# goethe post
#lisa_data_goethe_post <- read_sav("data/data_collection/data_pauli/GOETHE_POST_anonym.SAV")
#write_csv(x=lisa_data_goethe_post, path="data/data_collection/data_pauli/goethe_post.csv")





# import new data and formating ----
## copied from scoscie website: GNU R-SCript für Daten-Import
ds_file = file.choose("data/data_collection/rdata_KRSumfrage_2024-06-07_10-21.csv")
# setwd("./")
#ds_file = "rdata_KRSumfrage_2024-06-07_10-21.csv"
options(encoding = "UTF-8")
ds = read.delim(
  file=ds_file, encoding="UTF-8", fileEncoding="UTF-8",
  header = FALSE, sep = "\t", quote = "\"",
  dec = ".", row.names = NULL,
  col.names = c(
    "CASE","SERIAL","REF","QUESTNNR","MODE","STARTED","AT01_01","AT01_02","AT01_03",
    "AT01_04","B001_01","B001_02","B001_03","B001_04","B001_05","B001_06","B001_07",
    "B001_08","B001_09","B001_10","CS01_01","CS01_02","CS01_03","IN01_01","IN01_02",
    "IN01_03","PB01_01","PB01_02","SN01_01","SN01_02","SN01_03","SW01_01","SW01_02",
    "SW01_03","SW01_04","SW01_05","SW01_06","SW01_07","SW01_08","WD01","WD02",
    "WD02_01","WD02_02","WD02_03","TIME001","TIME002","TIME003","TIME004","TIME005",
    "TIME006","TIME007","TIME008","TIME009","TIME_SUM","MAILSENT","LASTDATA",
    "FINISHED","Q_VIEWER","LASTPAGE","MAXPAGE","MISSING","MISSREL","TIME_RSI"
  ),
  as.is = TRUE,
  colClasses = c(
    CASE="numeric", SERIAL="character", REF="character", QUESTNNR="character",
    MODE="factor", STARTED="character", AT01_01="numeric", AT01_02="numeric",
    AT01_03="numeric", AT01_04="numeric", B001_01="numeric", B001_02="numeric",
    B001_03="numeric", B001_04="numeric", B001_05="numeric", B001_06="numeric",
    B001_07="numeric", B001_08="numeric", B001_09="numeric", B001_10="numeric",
    CS01_01="numeric", CS01_02="numeric", CS01_03="numeric", IN01_01="numeric",
    IN01_02="numeric", IN01_03="numeric", PB01_01="numeric", PB01_02="numeric",
    SN01_01="numeric", SN01_02="numeric", SN01_03="numeric", SW01_01="numeric",
    SW01_02="numeric", SW01_03="numeric", SW01_04="numeric", SW01_05="numeric",
    SW01_06="numeric", SW01_07="numeric", SW01_08="numeric", WD01="numeric",
    WD02="numeric", WD02_01="logical", WD02_02="logical", WD02_03="logical",
    TIME001="integer", TIME002="integer", TIME003="integer", TIME004="integer",
    TIME005="integer", TIME006="integer", TIME007="integer", TIME008="integer",
    TIME009="integer", TIME_SUM="integer", MAILSENT="character",
    LASTDATA="character", FINISHED="logical", Q_VIEWER="logical",
    LASTPAGE="numeric", MAXPAGE="numeric", MISSING="numeric", MISSREL="numeric",
    TIME_RSI="numeric"
  ),
  skip = 1,
  check.names = TRUE, fill = TRUE,
  strip.white = FALSE, blank.lines.skip = TRUE,
  comment.char = "",
  na.strings = ""
)

row.names(ds) = ds$CASE

rm(ds_file)

attr(ds, "project") = "KRSumfrage"
attr(ds, "description") = "KlimaRatSchule Umfrage"
attr(ds, "date") = "2024-06-04 13:11:29"
attr(ds, "server") = "https://www.soscisurvey.de"

# Variable und Value Labels
#### irrelevant attributes? changed the wrong numbers and inversed, but not needed anyways?!
ds$WD01 = factor(ds$WD01, levels=c("1","2","3","4","5","6","7","-9"), labels=c("6. Klasse","7. Klasse","8. Klasse","9. Klasse","10. Klasse","11. Klasse","12. Klasse","[NA] nicht beantwortet"), ordered=FALSE)
attr(ds$AT01_01,"0") = "stimme gar nicht zu"
attr(ds$AT01_01,"1") = "stimme eher nicht zu"
attr(ds$AT01_01,"2") = "stimme eher zu"
attr(ds$AT01_01,"3") = "stimme völlig zu"
attr(ds$AT01_01,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$AT01_02,"3") = "stimme gar nicht zu" #inverse
attr(ds$AT01_02,"2") = "stimme eher nicht zu" #inverse
attr(ds$AT01_02,"1") = "stimme eher zu" #inverse
attr(ds$AT01_02,"0") = "stimme völlig zu" #inverse
attr(ds$AT01_02,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$AT01_03,"0") = "stimme gar nicht zu"
attr(ds$AT01_03,"1") = "stimme eher nicht zu"
attr(ds$AT01_03,"2") = "stimme eher zu"
attr(ds$AT01_03,"3") = "stimme völlig zu"
attr(ds$AT01_03,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$AT01_04,"0") = "stimme gar nicht zu"
attr(ds$AT01_04,"1") = "stimme eher nicht zu"
attr(ds$AT01_04,"2") = "stimme eher zu"
attr(ds$AT01_04,"3") = "stimme völlig zu"
attr(ds$AT01_04,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$B001_01,"0") = "stimme gar nicht zu"
attr(ds$B001_01,"1") = "stimme eher nicht zu"
attr(ds$B001_01,"2") = "stimme eher zu"
attr(ds$B001_01,"3") = "stimme völlig zu"
attr(ds$B001_01,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$B001_02,"0") = "stimme gar nicht zu"
attr(ds$B001_02,"1") = "stimme eher nicht zu"
attr(ds$B001_02,"2") = "stimme eher zu"
attr(ds$B001_02,"3") = "stimme völlig zu"
attr(ds$B001_02,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$B001_03,"3") = "stimme gar nicht zu" #inverse
attr(ds$B001_03,"2") = "stimme eher nicht zu" #inverse
attr(ds$B001_03,"1") = "stimme eher zu" #inverse
attr(ds$B001_03,"0") = "stimme völlig zu" #inverse
attr(ds$B001_03,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$B001_04,"3") = "stimme gar nicht zu" #inverse
attr(ds$B001_04,"2") = "stimme eher nicht zu" #inverse
attr(ds$B001_04,"1") = "stimme eher zu" #inverse
attr(ds$B001_04,"0") = "stimme völlig zu" #inverse
attr(ds$B001_04,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$B001_05,"0") = "stimme gar nicht zu"
attr(ds$B001_05,"1") = "stimme eher nicht zu"
attr(ds$B001_05,"2") = "stimme eher zu"
attr(ds$B001_05,"3") = "stimme völlig zu"
attr(ds$B001_05,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$B001_06,"0") = "stimme gar nicht zu"
attr(ds$B001_06,"1") = "stimme eher nicht zu"
attr(ds$B001_06,"2") = "stimme eher zu"
attr(ds$B001_06,"3") = "stimme völlig zu"
attr(ds$B001_06,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$B001_07,"0") = "stimme gar nicht zu"
attr(ds$B001_07,"1") = "stimme eher nicht zu"
attr(ds$B001_07,"2") = "stimme eher zu"
attr(ds$B001_07,"3") = "stimme völlig zu"
attr(ds$B001_07,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$B001_08,"3") = "stimme gar nicht zu" #inverse
attr(ds$B001_08,"2") = "stimme eher nicht zu" #inverse
attr(ds$B001_08,"1") = "stimme eher zu" #inverse
attr(ds$B001_08,"0") = "stimme völlig zu" #inverse
attr(ds$B001_08,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$B001_09,"3") = "stimme gar nicht zu" #inverse
attr(ds$B001_09,"2") = "stimme eher nicht zu" #inverse
attr(ds$B001_09,"1") = "stimme eher zu" #inverse
attr(ds$B001_09,"0") = "stimme völlig zu" #inverse
attr(ds$B001_09,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$B001_10,"0") = "stimme gar nicht zu"
attr(ds$B001_10,"1") = "stimme eher nicht zu"
attr(ds$B001_10,"2") = "stimme eher zu"
attr(ds$B001_10,"3") = "stimme völlig zu"
attr(ds$B001_10,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$CS01_01,"0") = "stimme gar nicht zu"
attr(ds$CS01_01,"1") = "stimme eher nicht zu"
attr(ds$CS01_01,"2") = "stimme eher zu"
attr(ds$CS01_01,"3") = "stimme völlig zu"
attr(ds$CS01_01,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$CS01_02,"0") = "stimme gar nicht zu"
attr(ds$CS01_02,"1") = "stimme eher nicht zu"
attr(ds$CS01_02,"2") = "stimme eher zu"
attr(ds$CS01_02,"3") = "stimme völlig zu"
attr(ds$CS01_02,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$CS01_03,"0") = "stimme gar nicht zu"
attr(ds$CS01_03,"1") = "stimme eher nicht zu"
attr(ds$CS01_03,"2") = "stimme eher zu"
attr(ds$CS01_03,"3") = "stimme völlig zu"
attr(ds$CS01_03,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$IN01_01,"0") = "stimme gar nicht zu"
attr(ds$IN01_01,"1") = "stimme eher nicht zu"
attr(ds$IN01_01,"2") = "stimme eher zu"
attr(ds$IN01_01,"3") = "stimme völlig zu"
attr(ds$IN01_01,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$IN01_02,"0") = "stimme gar nicht zu"
attr(ds$IN01_02,"1") = "stimme eher nicht zu"
attr(ds$IN01_02,"2") = "stimme eher zu"
attr(ds$IN01_02,"3") = "stimme völlig zu"
attr(ds$IN01_02,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$IN01_03,"0") = "stimme gar nicht zu"
attr(ds$IN01_03,"1") = "stimme eher nicht zu"
attr(ds$IN01_03,"2") = "stimme eher zu"
attr(ds$IN01_03,"3") = "stimme völlig zu"
attr(ds$IN01_03,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$PB01_01,"0") = "stimme gar nicht zu"
attr(ds$PB01_01,"1") = "stimme eher nicht zu"
attr(ds$PB01_01,"2") = "stimme eher zu"
attr(ds$PB01_01,"3") = "stimme völlig zu"
attr(ds$PB01_01,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$PB01_02,"0") = "stimme gar nicht zu"
attr(ds$PB01_02,"1") = "stimme eher nicht zu"
attr(ds$PB01_02,"2") = "stimme eher zu"
attr(ds$PB01_02,"3") = "stimme völlig zu"
attr(ds$PB01_02,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$SN01_01,"0") = "stimme gar nicht zu"
attr(ds$SN01_01,"1") = "stimme eher nicht zu"
attr(ds$SN01_01,"2") = "stimme eher zu"
attr(ds$SN01_01,"3") = "stimme völlig zu"
attr(ds$SN01_01,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$SN01_02,"0") = "stimme gar nicht zu"
attr(ds$SN01_02,"1") = "stimme eher nicht zu"
attr(ds$SN01_02,"2") = "stimme eher zu"
attr(ds$SN01_02,"3") = "stimme völlig zu"
attr(ds$SN01_02,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$SN01_03,"0") = "stimme gar nicht zu"
attr(ds$SN01_03,"1") = "stimme eher nicht zu"
attr(ds$SN01_03,"2") = "stimme eher zu"
attr(ds$SN01_03,"3") = "stimme völlig zu"
attr(ds$SN01_03,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$SW01_01,"0") = "stimme gar nicht zu"
attr(ds$SW01_01,"1") = "stimme eher nicht zu"
attr(ds$SW01_01,"2") = "stimme eher zu"
attr(ds$SW01_01,"3") = "stimme völlig zu"
attr(ds$SW01_01,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$SW01_02,"0") = "stimme gar nicht zu"
attr(ds$SW01_02,"1") = "stimme eher nicht zu"
attr(ds$SW01_02,"2") = "stimme eher zu"
attr(ds$SW01_02,"3") = "stimme völlig zu"
attr(ds$SW01_02,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$SW01_03,"3") = "stimme gar nicht zu" #inverse
attr(ds$SW01_03,"2") = "stimme eher nicht zu" #inverse
attr(ds$SW01_03,"1") = "stimme eher zu" #inverse
attr(ds$SW01_03,"0") = "stimme völlig zu" #inverse
attr(ds$SW01_03,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$SW01_04,"0") = "stimme gar nicht zu"
attr(ds$SW01_04,"1") = "stimme eher nicht zu"
attr(ds$SW01_04,"2") = "stimme eher zu"
attr(ds$SW01_04,"3") = "stimme völlig zu"
attr(ds$SW01_04,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$SW01_05,"3") = "stimme gar nicht zu" #inverse
attr(ds$SW01_05,"2") = "stimme eher nicht zu" #inverse
attr(ds$SW01_05,"1") = "stimme eher zu" #inverse
attr(ds$SW01_05,"0") = "stimme völlig zu" #inverse
attr(ds$SW01_05,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$SW01_06,"0") = "stimme gar nicht zu"
attr(ds$SW01_06,"1") = "stimme eher nicht zu"
attr(ds$SW01_06,"2") = "stimme eher zu"
attr(ds$SW01_06,"3") = "stimme völlig zu"
attr(ds$SW01_06,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$SW01_07,"0") = "stimme gar nicht zu"
attr(ds$SW01_07,"1") = "stimme eher nicht zu"
attr(ds$SW01_07,"2") = "stimme eher zu"
attr(ds$SW01_07,"3") = "stimme völlig zu"
attr(ds$SW01_07,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$SW01_08,"0") = "stimme gar nicht zu"
attr(ds$SW01_08,"1") = "stimme eher nicht zu"
attr(ds$SW01_08,"2") = "stimme eher zu"
attr(ds$SW01_08,"3") = "stimme völlig zu"
attr(ds$SW01_08,"-100") = "Ich weiß nicht/ keine Angabe"
attr(ds$WD02_01,"F") = "nicht gewählt"
attr(ds$WD02_01,"T") = "ausgewählt"
attr(ds$WD02_02,"F") = "nicht gewählt"
attr(ds$WD02_02,"T") = "ausgewählt"
attr(ds$WD02_03,"F") = "nicht gewählt"
attr(ds$WD02_03,"T") = "ausgewählt"
attr(ds$FINISHED,"F") = "abgebrochen"
attr(ds$FINISHED,"T") = "ausgefüllt"
attr(ds$Q_VIEWER,"F") = "Teilnehmer"
attr(ds$Q_VIEWER,"T") = "Durchklicker"
comment(ds$SERIAL) = "Personenkennung oder Teilnahmecode (sofern verwendet)"
comment(ds$REF) = "Referenz (sofern im Link angegeben)"
comment(ds$QUESTNNR) = "Fragebogen, der im Interview verwendet wurde"
comment(ds$MODE) = "Interview-Modus"
comment(ds$STARTED) = "Zeitpunkt zu dem das Interview begonnen hat (Europe/Berlin)"
comment(ds$AT01_01) = "Einstellungen: Die Umwelt in Deutschland ist durch den globalen Klimawandel gefährdet."
comment(ds$AT01_02) = "Einstellungen: Die derzeitige globale Erwärmung ist NICHT vom Menschen verursacht, sondern ein natürlicher Vorgang."
comment(ds$AT01_03) = "Einstellungen: Der Klimawandel schadet der natürlichen Umwelt und der Tierwelt in Deutschland."
comment(ds$AT01_04) = "Einstellungen: Ich bin bereit, einen gewissen Betrag zu bezahlen, um die Auswirkungen des Klimawandels zu verringern."
comment(ds$B001_01) = "Verhalten: Ich habe meinen Fleischkonsum in den letzten Monaten bewusst reduziert."
comment(ds$B001_02) = "Verhalten: Ich kaufe in Deutschland produziertes Obst und vermeide den Kauf von importiertem Obst (z. B. Bananen, Kiwis)."
comment(ds$B001_03) = "Verhalten: In meinem Kühlschrank lagere ich oft Lebensmittel, die das Haltbarkeitsdatum überschritten haben."
comment(ds$B001_04) = "Verhalten: Beim Kauf von Elektrogeräten achte ich am meisten auf den Preis der Geräte."
comment(ds$B001_05) = "Verhalten: Ich kaufe Elektrogeräte, die ein Energiesparlabel haben."
comment(ds$B001_06) = "Verhalten: Ich schalte Lichter und Wasserhähne so oft wie möglich aus."
comment(ds$B001_07) = "Verhalten: Ich ziehe den Stecker von Geräten, die vorübergehend nicht in Gebrauch sind."
comment(ds$B001_08) = "Verhalten: Ich fahre hauptsächlich mit einem Auto oder einem Motorroller, beziehungsweise werde gefahren."
comment(ds$B001_09) = "Verhalten: Ich nutze Aufzüge und steige selten Treppen."
comment(ds$B001_10) = "Verhalten: Ich unterstütze eine Erhöhung der Besteuerung von Kraftstoffen, um den Verbrauch fossiler Kraftstoffe zu reduzieren."
comment(ds$CS01_01) = "Collective Selbstwirksamkeit: Wir, als Schülerinnnen und Schüler, können durch unsere Handlungen einen Beitrag zum Kllimaschutz leisten, wenn wir das möchten."
comment(ds$CS01_02) = "Collective Selbstwirksamkeit: Wir, als Schülerinnen und Schüler, können beeinflussen, wie unsere Schulleitung oder Schule bezogen auf den Klimaschutz handelt, wenn wir das möchten."
comment(ds$CS01_03) = "Collective Selbstwirksamkeit: Wir, als Schülerinnen und Schüler, sind in der Lage, andere davon zu überzeugen, sich für mehr Klimaschutz einzusetzen, wenn wir das möchten."
comment(ds$IN01_01) = "Intentionen: Es liegt in meiner Verantwortung, meine Mitbürgerinnen und Mitbürger zu ermutigen, den Klimawandel zu beachten."
comment(ds$IN01_02) = "Intentionen: Ich bin bereit dazu, mich in meinem täglichen Leben umweltfreundlicher zu verhalten."
comment(ds$IN01_03) = "Intentionen: Ich bin bereit, alles zu tun, um die Auswirkungen des Klimawandels zu mindern."
comment(ds$PB01_01) = "Wahrgenommene Verhaltenskontrolle: Ich glaube, dass ich dazu beitragen kann, die Auswirkungen des Klimawandels abzuschwächen."
comment(ds$PB01_02) = "Wahrgenommene Verhaltenskontrolle: Durch mein Handeln im Alltag kann ich zur Verringerung von CO2-Ausstoß beitragen."
comment(ds$SN01_01) = "Subjektive Normen: In meiner Familie wird oft über den Klimawandel oder die globale Erwärmung diskutiert."
comment(ds$SN01_02) = "Subjektive Normen: Meine Mitschülerinnen und Mitschüler diskutieren oft über den Klimawandel oder die globale Erwärmung."
comment(ds$SN01_03) = "Subjektive Normen: Meine Mitschülerinnen und Mitschüler könnten mich kritisieren, wenn ich keine Maßnahmen zum Klimaschutz ergreife."
comment(ds$SW01_01) = "Selbstwirksamkeit: Ich glaube, dass meine eigenen Handlungen einen Beitrag zum Klimaschutz leisten können, wenn ich das möchte."
comment(ds$SW01_02) = "Selbstwirksamkeit: Ich glaube, dass ich den Klimaschutz vorantreiben kann, indem ich in meinem Umfeld über den Klimawandel aufkläre, wenn ich das möchte."
comment(ds$SW01_03) = "Selbstwirksamkeit: Ich glaube nicht, dass ich in der Lage bin, mich für den Klimaschutz einzusetzen."
comment(ds$SW01_04) = "Selbstwirksamkeit: Ich glaube, dass ich dazu in der Lage bin, andere davon zu überzeugen, sich für mehr Klimaschutz einzusetzen, wenn ich das möchte."
comment(ds$SW01_05) = "Selbstwirksamkeit: Ich glaube nicht, dass ich Möglichkeiten habe, einen Einfluss auf den Klimawandel zu nehmen."
comment(ds$SW01_06) = "Selbstwirksamkeit: Ich glaube, dass ich beeinflussen kann, wie meine Schulleitung oder meine Schule bezogen auf den Klimaschutz handelt, wenn ich das möchte."
comment(ds$SW01_07) = "Selbstwirksamkeit: Ich glaube, dass ich meine Schulleitung oder Schule dabei unterstützen kann, sich für Klimaschutz einzusetzen, wenn ich das möchte."
comment(ds$SW01_08) = "Selbstwirksamkeit: Ich glaube, dass ich mich in Zusammenarbeit mit anderen sinnvoll für den Klimaschutz engagieren kann, wenn ich das möchte."
comment(ds$WD01) = "klasse"
comment(ds$WD02) = "gruppe: Ausweichoption (negativ) oder Anzahl ausgewählter Optionen"
comment(ds$WD02_01) = "gruppe: KRS-Schulgruppe (die sich regelmäßig getroffen haben)"
comment(ds$WD02_02) = "gruppe: KlimaRat Bürgergutachten (die zufällig ausgelost wurden)"
comment(ds$WD02_03) = "gruppe: Keiner der beiden Gruppen"
comment(ds$TIME001) = "Verweildauer Seite 1"
comment(ds$TIME002) = "Verweildauer Seite 2"
comment(ds$TIME003) = "Verweildauer Seite 3"
comment(ds$TIME004) = "Verweildauer Seite 4"
comment(ds$TIME005) = "Verweildauer Seite 5"
comment(ds$TIME006) = "Verweildauer Seite 6"
comment(ds$TIME007) = "Verweildauer Seite 7"
comment(ds$TIME008) = "Verweildauer Seite 8"
comment(ds$TIME009) = "Verweildauer Seite 9"
comment(ds$TIME_SUM) = "Verweildauer gesamt (ohne Ausreißer)"
comment(ds$MAILSENT) = "Versandzeitpunkt der Einladungsmail (nur für nicht-anonyme Adressaten)"
comment(ds$LASTDATA) = "Zeitpunkt als der Datensatz das letzte mal geändert wurde"
comment(ds$FINISHED) = "Wurde die Befragung abgeschlossen (letzte Seite erreicht)?"
comment(ds$Q_VIEWER) = "Hat der Teilnehmer den Fragebogen nur angesehen, ohne die Pflichtfragen zu beantworten?"
comment(ds$LASTPAGE) = "Seite, die der Teilnehmer zuletzt bearbeitet hat"
comment(ds$MAXPAGE) = "Letzte Seite, die im Fragebogen bearbeitet wurde"
comment(ds$MISSING) = "Anteil fehlender Antworten in Prozent"
comment(ds$MISSREL) = "Anteil fehlender Antworten (gewichtet nach Relevanz)"
comment(ds$TIME_RSI) = "Ausfüll-Geschwindigkeit (relativ)"



# Assure that the comments are retained in subsets
as.data.frame.avector = as.data.frame.vector
`[.avector` <- function(x,i,...) {
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  r
}
ds_tmp = data.frame(
  lapply(ds, function(x) {
    structure( x, class = c("avector", class(x) ) )
  } )
)
mostattributes(ds_tmp) = attributes(ds)
ds = ds_tmp
rm(ds_tmp)

### assign groups ----
ds <- ds %>%
  mutate(Group = ifelse(WD02_01, "group2", ifelse(WD02_03, "group0", "group1")))

# count occurences
value_counts <- ds %>%
  count(Group)  # Count occurrences of each value in col1


# ??exclude incomplete data ----
# Remove unnecessary columns with NA
ds$SERIAL <- NULL
ds$REF <- NULL
ds$MAILSENT <- NULL

# Function to count the number of NAs in a row
count_nas <- function(row) {
  sum(is.na(row))
}

# Apply the function to each row and create a new column 'count_nas'
ds$count_nas <- apply(ds, 1, count_nas)

# Filter the data frame to exclude rows with more than ??? NAs?
ds_filtered <- ds[ds$count_nas <= 20, ]


# ??exclude data with more than 25% missing (8 answers) ----
###### more than 8 times I don't know or does it change because more questions??!
# Function to count the number of -1s in a row
count_minus_ones <- function(row) {
  sum(row == -1, na.rm = TRUE)
}

ds_filtered$count_minus_ones <- apply(ds_filtered, 1, count_minus_ones)

# Filter the data frame to exclude rows with more than eight -1s
ds_filtered <- ds_filtered[ds_filtered$count_minus_ones <= 8, ]
# ?? exclude data with less than 4 minutes  (240 sec) or 150sec! processing time ----
#ds_filtered <- ds_filtered[ds_filtered$TIME_SUM >= 240, ]
ds_filtered <- ds_filtered[ds_filtered$TIME_SUM >= 150, ]


# data transformation # scaling according to Lisa (0-3, instead of 1-4) ----
# Define a function to convert values
convert_values <- function(x) {
  x <- ifelse(x == 4, 3,
              ifelse(x == 3, 2,
                     ifelse(x == 2, 1,
                            ifelse(x == 1, 0,
                                   ifelse(x == -1, -100, 
                                          x)))))
  return(x)
}
#### leave all other numbers the same!!!!!!

convert_values2 <- function(x) {
  x <- ifelse(x == 4, 3,
              ifelse(x == 3, 2,
                     ifelse(x == 2, 1,
                            ifelse(x == 1, 0,
                                   ifelse(x == -1, -100, x)))))
  return(x)
}

# Apply the function to all columns of the data frame
ds_scaled <- lapply(ds_filtered, convert_values)
ds_scaled <- as.data.frame(ds_scaled)

# invert certain scales to reflect meaning (AT2, B3, B4, B8, B9, SW3, SW5) ----
# Define a function to inverse scales and add new columns
inverse_scale_and_add_columns <- function(df, columns) {
  for (col in columns) {
    new_col_name <- paste(col, "inverse", sep = "_")
    df[[new_col_name]] <- inverse_scale(df[[col]])
  }
  return(df)
}

# Define a function to inverse scales
inverse_scale <- function(x) {
  x <- ifelse(x == 3, 0,
              ifelse(x == 2, 1,
                     ifelse(x == 1, 2,
                            ifelse(x == 0, 3, x))))
  return(x)
}

# Apply the function to add extra columns with inverse scales
ds_scaled_in <- inverse_scale_and_add_columns(ds_scaled, c("AT01_02", "B001_03", "B001_04", "B001_08", "B001_09", "SW01_03", "SW01_05"))

# save formated dataset as csv ----
write_csv(x=ds_scaled_in, path="data/data_collection/angell_mzp3.csv")


# checking ----
# for time 150-240 sec
filtered_df <- subset(ds_filtered, TIME_SUM >= 150 & TIME_SUM <= 240)

# for groups
value_counts2 <- ds_filtered %>%
  count(Group)
