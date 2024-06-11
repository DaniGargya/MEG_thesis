# Dieses Script liest eine CSV-Datendatei in GNU R ein.
# Beim Einlesen werden für alle Variablen Beschriftungen (comment) angelegt.
# Die Beschriftungen für Werte wird ebenfalls als Attribute (attr) abgelegt.

ds_file = file.choose()
# setwd("./")
# ds_file = "rdata_KRSumfrage_2024-06-07_13-03.csv"

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
    MODE="factor", STARTED="POSIXct", AT01_01="numeric", AT01_02="numeric",
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
    TIME009="integer", TIME_SUM="integer", MAILSENT="POSIXct",
    LASTDATA="POSIXct", FINISHED="logical", Q_VIEWER="logical",
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

ds_tmp = data.frame(
  lapply(ds, function(x) {
    structure( x, class = c("avector", class(x) ) )
  } )
)
mostattributes(ds_tmp) = attributes(ds)
ds = ds_tmp
rm(ds_tmp)

