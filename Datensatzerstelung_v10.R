#----------Pakete
library(tidyquant) #Schnittstelle von Quantmod und tidyverse -> lädt automatisch auch quantmod
library(tidyverse) #Sammlung von Packages (dyplr, ggplot usw.) immer laden!
library(bruceR) #für die scaler function
library(vip) #feature importance
library(naivebayes) # nb
library(randomForest) # rf
library(caret) #confusions Matrix
library(basemodels) #für dummy classifier
library(ggplot2) #Abbildungen
library(ggpubr) #Spezielles theme für ggplot
library(ggrepel) #theme für ggplot um Namen in Diagrammen abbilden zu lassen
library(GGally) #für gg plot
library(ggridges) #für gg plot
library(cowplot) #für vierfelder plots
library(ggExtra) #für marginal histogramme in plots
library(waterfalls) # für Wasserfalldiagramm
library(readxl) #einlesen von excel datein
library(writexl) #abspeichern von Datensätzen als Excel Datei
library(openxlsx) #zweite Varaitne zum excel einlesen
library(psych) #für wilcoxon test 

# Aktienticker als Vektor
ticker <- c("ADS.DE", "AIR.DE", "ALV.DE", "BAS.DE", "BAYN.DE", "BEI.DE", 
            "BMW.DE", "BNR.DE", "CBK.DE","CON.DE","1COV.DE", "DBK.DE",
            "DTG.DE", "DB1.DE", "DHL.DE", "DTE.DE", "EOAN.DE", "FRE.DE",
            "HNR1.DE", "HEI.DE", "HEN3.DE", "IFX.DE", "MBG.DE", "MRK.DE",
            "MTX.DE", "MUV2.DE", "P911.DE", "PAH3.DE", "QIA.DE", "RHM.DE",
            "RWE.DE", "SAP.DE", "SRT3.DE", "SIE.DE", "ENR.DE", "SHL.DE",
            "SY1.DE", "VOW3.DE", "VNA.DE", "ZAL.DE")

length(ticker) #check ob alle Ticker da sind = 40 = i. O. 

# Liste der Zeiträume = in der Reihenfolge der Tickersymbole
zeiträume <- list(
  list(von = "1998-06-22", bis = "2024-09-30"), #Adidas
  list(von = "2021-09-20", bis = "2024-09-30"), #Airbus
  list(von = "1987-12-30", bis = "2024-09-30"), #Allianz
  list(von = "1987-12-30", bis = "2024-09-30"), #BASF
  list(von = "1987-12-30", bis = "2024-09-30"), #BAYER
  list(von = "2022-06-20", bis = "2024-09-30"), #Beiersdorf
  list(von = "1987-12-30", bis = "2024-09-30"), #BMW
  list(von = "2021-09-20", bis = "2024-09-30"), #Brenntag
  list(von = "2023-02-27", bis = "2024-09-30"), #Commerzbank
  list(von = "2012-09-24", bis = "2024-09-30"), #Continental
  list(von = "2018-03-19", bis = "2024-09-30"), #Covestro
  list(von = "1987-12-30", bis = "2024-09-30"), #Deutsche Bank
  list(von = "2022-03-21", bis = "2024-09-30"), #Daimler Truck
  list(von = "2002-12-23", bis = "2024-09-30"), #Deutsche Börse
  list(von = "2001-03-19", bis = "2024-09-30"), #Deutsche Post
  list(von = "1996-11-18", bis = "2024-09-30"), #Deutsche Telekom
  list(von = "2000-06-19", bis = "2024-09-30"), #EON
  list(von = "2009-03-23", bis = "2024-09-30"), #Fresenius
  list(von = "2022-03-21", bis = "2024-09-30"), #Hannover Rück
  list(von = "2010-06-21", bis = "2024-09-30"), #Heidelberg materials
  list(von = "1987-12-30", bis = "2024-09-30"), #Henkel
  list(von = "2009-09-21", bis = "2024-09-30"), #Infineon
  list(von = "1987-12-30", bis = "2024-09-30"), #Mercedes
  list(von = "2007-06-18", bis = "2024-09-30"), #Merck
  list(von = "2019-09-23", bis = "2024-09-30"), #MTU
  list(von = "1996-09-23", bis = "2024-09-30"), #Münchner Rück
  list(von = "2022-12-19", bis = "2024-09-30"), #Porsche 
  list(von = "2021-09-20", bis = "2024-09-30"), #Porsche Holding
  list(von = "2021-09-20", bis = "2024-09-30"), #Qiagen
  list(von = "2023-03-20", bis = "2024-09-30"), #Rheinmetall
  list(von = "1987-12-30", bis = "2024-09-30"), #RWE
  list(von = "1995-09-18", bis = "2024-09-30"), #SAP
  list(von = "2021-09-20", bis = "2024-09-30"), #Sartorius
  list(von = "1987-12-30", bis = "2024-09-30"), #Siemens 
  list(von = "2022-09-19", bis = "2024-09-30"), #Siemens Energy
  list(von = "2021-09-20", bis = "2024-09-30"), #Siemens Healthineers
  list(von = "2021-09-20", bis = "2024-09-30"), #Symrise
  list(von = "2009-12-23", bis = "2024-09-30"), #VW
  list(von = "2015-09-21", bis = "2024-09-30"), #Vonovia
  list(von = "2021-09-20", bis = "2024-09-30")  #Zalando
)

length(zeiträume) #check ob alle Akien da :3

# Funktion zum abrufen der Daten mit tq_get
datenabzug <- function(ticker, period){
  aktienkurse <- tq_get(ticker, get = "stock.prices", from = period$von, to = period$bis)
  
  täglich <- aktienkurse %>%
    mutate(symbol = ticker) %>%
    select(symbol, everything())
  
  list(daily = täglich)
}

# Für alle Aktien Daten abrufen und Aggregieren je nach Intervall (täglich usw)
# Erstellt einen fusionierten Datensatz und entfernt anschließend den Rohdatensatz
alle <- map2(ticker, zeiträume, datenabzug)

Fusion_d <- bind_rows(map(alle, "daily"))
Fusion_d$Intervall <- "Täglich"
rm(alle)

# Bereinigung der Datensätze -> Inkikatoren sonst nicht berechenbar
# Entfernt alle Zeilen, bei denen der Wert von "volume" 0 ist
Fusion_d <- Fusion_d |> 
  drop_na() |>   # Entfernt alle Zeilen mit NAs
  filter(volume != 0)  

# Berechnung der technischen Indikatoren -> mit Mutate und den Funktionen aus Tidyquant
Fusion_d <- Fusion_d |>  
  group_by(symbol) |> 
  na.omit() |> 
  mutate(SMA_10 = SMA(close, n = 10),
         SMA_20 = SMA(close, n = 20),
         SMA_50 = SMA(close, n = 50),
         WMA_10 = WMA(close, n = 10),
         WMA_20 = WMA(close, n = 20),
         WMA_50 = WMA(close, n = 20),
         EMA_10 = EMA(close, n = 10),
         EMA_20 = EMA(close, n = 20), 
         EMA_50 = EMA(close, n = 50), 
         ADX = ADX(HLC = cbind(high, low, close), n = 14), #erste Spalte des ADX ist der ADX
         ADX = ADX[,1],
         MACD_Werte = MACD(close, nFast = 12, nSlow = 26, nSig = 9),
         MACD_Linie = MACD_Werte[,1],  # Die erste Spalte ist die MACD-Linie
         Signal_Linie = MACD_Werte[,2], # Die zweite Spalte ist die Signallinie
         MACD_Histogramm = MACD_Werte[,1] - MACD_Werte[,2],
         ROC = ROC(close, n = 14),
         RSI = RSI(close, n = 14),
         Williams_R_14 = WPR(HLC = cbind(high, low, close), n = 14),
         Stoch_Osz = stoch(HLC = cbind(high, low, close), nFastK = 14, nFastD = 3, nSlowD = 3),
         Stoch_K = Stoch_Osz[,1],  # Die erste Spalte ist die %K-Linie -> diese benutzen :)
         Stoch_D = Stoch_Osz[,2],
         OBV = OBV(close, volume),
         VWAP_10   = VWAP(close, volume, n = 10),
         VWAP_20   = VWAP(close, volume, n = 20),
         VWAP_50   = VWAP(close, volume, n = 50),
         CMF = CMF(HLC = cbind(high, low, close),volume= volume, n = 21),
) |> 
  na.omit()

# Im Datensatz nur die nötigen Variablen auswählen
Fusion_d <- Fusion_d %>%
  select(symbol, date, open, high, low, close, volume, adjusted, Intervall,
         SMA_10, SMA_20, SMA_50, WMA_10, WMA_20, WMA_50,EMA_10, EMA_20, EMA_50,
         MACD_Linie, ADX,
         Williams_R_14, Stoch_K, ROC, RSI,
         OBV, CMF, VWAP_10, VWAP_20, VWAP_50
         )
  

# Min- Max Scaler
min_max_scaler <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Test. dass der min max scaler geht :)
abc <- c(5,1,5,7,8)
min_max_scaler(abc)

# Skalierung der Variablen mit der function vom mit maxscaler
Fusion_d <- Fusion_d %>%
  group_by(symbol) %>%
  mutate(
    SMA_10 = min_max_scaler(SMA_10),
    SMA_20 = min_max_scaler(SMA_20),
    SMA_50 = min_max_scaler(SMA_50),
    WMA_10 = min_max_scaler(WMA_10),
    WMA_20 = min_max_scaler(WMA_20),
    WMA_50 = min_max_scaler(WMA_50),
    EMA_10 = min_max_scaler(EMA_10),
    EMA_20 = min_max_scaler(EMA_20),
    EMA_50 = min_max_scaler(EMA_50),
    MACD_Linie = min_max_scaler(MACD_Linie),
    ADX = min_max_scaler(ADX),
    Williams_R_14 = min_max_scaler(Williams_R_14),
    Stoch_K = min_max_scaler(Stoch_K),
    ROC = min_max_scaler(ROC),
    RSI = min_max_scaler(RSI),
    OBV = min_max_scaler(OBV),
    VWAP_10   = min_max_scaler(VWAP_10),
    VWAP_20   = min_max_scaler(VWAP_20),
    VWAP_50   = min_max_scaler(VWAP_50),
    CMF = min_max_scaler(CMF)
  ) %>%
  ungroup()  # Ungroupen

# Fusion der Datensätze
# Berechnung Rendite 
# Umwandlung in faktor (kat. Variable) + Entfernen von NAs (keine Handelstage)
Fusion_d <- Fusion_d |> 
  drop_na()

Fusion_g <- bind_rows(Fusion_d)

Fusion_g <- Fusion_g |>  
  group_by(symbol, Intervall) |> 
  mutate(Rendite_Folgetag = lead(close) / close - 1,
         Kursbewegung_Folgeperiode = ifelse(Rendite_Folgetag > 0, 1, 0))

Fusion_g$Kursbewegung_Folgeperiode <- as.factor(Fusion_g$Kursbewegung_Folgeperiode) #sicherghen, dass es ein Faktor ist


#Erstellung eines Datensatz von name und Ticker, sodass man zur Not immer den Ticker nachschauen kann für Vergessliche
Name <- c("Adidas_d", "Airbus_d", "Allianz_d", "BASF_d", "Bayer_d", "Beiersdorf_d", "BMW_d", 
          "Brenntag_d", "Commerzbank_d", "Continental_d", "Covestro_d", "DaimlerTruck_d", 
          "DeutscheBank_d", "DeutscheBörse_d", "DeutschePost_d", "DeutscheTelekom_d",
          "EON_d", "Fresenius_d", "HannoverRück_d", "HeidelbergMaterial_d", "Henkel_d",
          "Infineon_d", "Mercedes_d", "Merck_d", "MTU_d", "MünchnerRück_d", "Porsche_d",
          "PorscheHolding_d", "Qiagen_d", "Rheinmetall_d", "RWE_d", "SAP_d", "Sartorius_d",
          "Siemens_d", "SiemensEnergy_d", "SiemensHealthineers_d", "Symrise_d", "Volkswagen_d",
          "Vonovia_d", "Zalando_d")


ticker <- c("ADS.DE", "AIR.DE", "ALV.DE", "BAS.DE", "BAYN.DE", "BEI.DE", 
            "BMW.DE", "BNR.DE", "CBK.DE","CON.DE","1COV.DE","DTG.DE","DBK.DE",
            "DB1.DE", "DHL.DE", "DTE.DE", "EOAN.DE", "FRE.DE",
            "HNR1.DE", "HEI.DE", "HEN3.DE", "IFX.DE", "MBG.DE", "MRK.DE",
            "MTX.DE", "MUV2.DE", "P911.DE", "PAH3.DE", "QIA.DE", "RHM.DE",
            "RWE.DE", "SAP.DE", "SRT3.DE", "SIE.DE", "ENR.DE", "SHL.DE",
            "SY1.DE", "VOW3.DE", "VNA.DE", "ZAL.DE")

AktienUndTicker <- data.frame(Name, ticker)


# Erstellt eine Pivottabelle zum Datensatz
# Ermöglicht Analyse -> z. B. der Aufwärts-und Abwärtsbewegungen
library(rpivotTable)
pivot <- as.tibble(Fusion_g) |> 
  select(symbol, Kursbewegung_Folgeperiode, Intervall, volume)
  
rpivotTable(data = pivot)

# Abspeichern des Datensatzes als excel, falls gewünscht
library(openxlsx)
write.xlsx(Fusion_g, "Fusion_g.xlsx")

# Alle Indikatoren zur Kontrolle
SMA_10+SMA_20+SMA_50+WMA_10+WMA_20+WMA_50+EMA_10+EMA_20+EMA_50+MACD_Linie+ADX+Williams_R_14+Stoch_K+Stoch_D+ROC+RSI+OBV+CMF+VWAP_10+VWAP_20+VWAP_50,
