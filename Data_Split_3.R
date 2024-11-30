# 1. Erstellung von täglichen, wöchentlichen und monatlichen Datensätzen
# 2. Aufteilung in Trainings und Testdaten mit der Holdout Methode 

################################################################################
# Tägliche Datensätze # Alternative mit na.omit() -> aber erstmal weglassen
################################################################################
Adidas_d <- Fusion_g |> 
  filter(symbol == "ADS.DE", Intervall =="Täglich")


Airbus_d <- Fusion_g |> 
  filter(symbol == "AIR.DE", Intervall =="Täglich") 
 

Allianz_d <- Fusion_g |> 
  filter(symbol == "ALV.DE", Intervall =="Täglich")

BASF_d <- Fusion_g |> 
  filter(symbol == "BAS.DE", Intervall =="Täglich") 


Bayer_d <- Fusion_g |> 
  filter(symbol == "BAYN.DE", Intervall =="Täglich") 

Beiersdorf_d <- Fusion_g |> 
  filter(symbol == "BEI.DE", Intervall =="Täglich") 


BMW_d <- Fusion_g |> 
  filter(symbol == "BMW.DE", Intervall =="Täglich") 


Brenntag_d <- Fusion_g |> 
  filter(symbol == "BNR.DE", Intervall =="Täglich") 
 

Commerzbank_d <- Fusion_g |> 
  filter(symbol == "CBK.DE", Intervall =="Täglich")  

Continental_d <- Fusion_g |> 
  filter(symbol == "CON.DE", Intervall =="Täglich") 


Covestro_d <- Fusion_g |> 
  filter(symbol == "1COV.DE", Intervall =="Täglich") 


DaimlerTruck_d <- Fusion_g |> 
  filter(symbol == "DTG.DE", Intervall =="Täglich") 


DeutscheBank_d <- Fusion_g |> 
  filter(symbol == "DBK.DE", Intervall =="Täglich") 


DeutscheBörse_d <- Fusion_g |> 
  filter(symbol == "DB1.DE", Intervall =="Täglich") 
 

DeutschePost_d <- Fusion_g |> 
  filter(symbol == "DHL.DE", Intervall =="Täglich") 


DeutscheTelekom_d <- Fusion_g |> 
  filter(symbol == "DTE.DE", Intervall =="Täglich") 
 

EON_d <- Fusion_g |> 
  filter(symbol == "EOAN.DE", Intervall =="Täglich") 
 

Fresenius_d <- Fusion_g |> 
  filter(symbol == "FRE.DE", Intervall =="Täglich") 

HannoverRück_d <- Fusion_g |> 
  filter(symbol == "HNR1.DE", Intervall =="Täglich") 
 

HeidelbergMaterial_d <- Fusion_g |> 
  filter(symbol == "HEI.DE", Intervall =="Täglich") 


Henkel_d <- Fusion_g |> 
  filter(symbol == "HEN3.DE", Intervall =="Täglich") 
 

Infineon_d <- Fusion_g |> 
  filter(symbol == "IFX.DE", Intervall =="Täglich")


Mercedes_d <- Fusion_g |> 
  filter(symbol == "MBG.DE", Intervall =="Täglich") 
 

Merck_d <- Fusion_g |> 
  filter(symbol == "MRK.DE", Intervall =="Täglich")


MTU_d <- Fusion_g |> 
  filter(symbol == "MTX.DE", Intervall =="Täglich") 


MünchnerRück_d <- Fusion_g |> 
  filter(symbol == "MUV2.DE", Intervall =="Täglich") 

Porsche_d <- Fusion_g |> 
  filter(symbol == "P911.DE", Intervall =="Täglich") 


PorscheHolding_d <- Fusion_g |> 
  filter(symbol == "PAH3.DE", Intervall =="Täglich") 


Qiagen_d <- Fusion_g |> 
  filter(symbol == "QIA.DE", Intervall =="Täglich") 

Rheinmetall_d <- Fusion_g |> 
  filter(symbol == "RHM.DE", Intervall =="Täglich") 
 

RWE_d <- Fusion_g |> 
  filter(symbol == "RWE.DE", Intervall =="Täglich") 


SAP_d <- Fusion_g |> 
  filter(symbol == "SAP.DE", Intervall =="Täglich") 

Sartorius_d <- Fusion_g |> 
  filter(symbol == "SRT3.DE", Intervall =="Täglich") 


Siemens_d <- Fusion_g |> 
  filter(symbol == "SIE.DE", Intervall =="Täglich") 


SiemensEnergy_d <- Fusion_g |> 
  filter(symbol == "ENR.DE", Intervall =="Täglich")  
 

SiemensHealthineers_d <- Fusion_g |> 
  filter(symbol == "SHL.DE", Intervall =="Täglich") 

Symrise_d <- Fusion_g |> 
  filter(symbol == "SY1.DE", Intervall =="Täglich") 


Volkswagen_d <- Fusion_g |> 
  filter(symbol == "VOW3.DE", Intervall =="Täglich") 


Vonovia_d <- Fusion_g |> 
  filter(symbol == "VNA.DE", Intervall =="Täglich") 


Zalando_d <- Fusion_g |> 
  filter(symbol == "ZAL.DE", Intervall =="Täglich") 

################################################################################
# Wöchentliche Datensätze
################################################################################

Adidas_w <- Fusion_g |> 
  filter(symbol == "ADS.DE", Intervall =="Wöchentlich") 

Airbus_w <- Fusion_g |> 
  filter(symbol == "AIR.DE", Intervall =="Wöchentlich") 

Allianz_w <- Fusion_g |> 
  filter(symbol == "ALV.DE", Intervall =="Wöchentlich") 

BASF_w <- Fusion_g |> 
  filter(symbol == "BAS.DE", Intervall =="Wöchentlich")  

Bayer_w <- Fusion_g |> 
  filter(symbol == "BAYN.DE", Intervall =="Wöchentlich") 

Beiersdorf_w <- Fusion_g |> 
  filter(symbol == "BEI.DE", Intervall =="Wöchentlich") 

BMW_w <- Fusion_g |> 
  filter(symbol == "BMW.DE", Intervall =="Wöchentlich") 

Brenntag_w <- Fusion_g |> 
  filter(symbol == "BNR.DE", Intervall =="Wöchentlich") 

Commerzbank_w <- Fusion_g |> 
  filter(symbol == "CBK.DE", Intervall =="Wöchentlich") 

Continental_w <- Fusion_g |> 
  filter(symbol == "CON.DE", Intervall =="Wöchentlich") 

Covestro_w <- Fusion_g |> 
  filter(symbol == "1COV.DE", Intervall =="Wöchentlich") 

DaimlerTruck_w <- Fusion_g |> 
  filter(symbol == "DTG.DE", Intervall =="Wöchentlich") 

DeutscheBank_w <- Fusion_g |> 
  filter(symbol == "DBK.DE", Intervall =="Wöchentlich") 

DeutscheBörse_w <- Fusion_g |> 
  filter(symbol == "DB1.DE", Intervall =="Wöchentlich") 

DeutschePost_w <- Fusion_g |> 
  filter(symbol == "DHL.DE", Intervall =="Wöchentlich") 

DeutscheTelekom_w <- Fusion_g |> 
  filter(symbol == "DTE.DE", Intervall =="Wöchentlich") 

EON_w <- Fusion_g |> 
  filter(symbol == "EOAN.DE", Intervall =="Wöchentlich") 

Fresenius_w <- Fusion_g |> 
  filter(symbol == "FRE.DE", Intervall =="Wöchentlich") 

HannoverRück_w <- Fusion_g |> 
  filter(symbol == "HNR1.DE", Intervall =="Wöchentlich") 

HeidelbergMaterial_w <- Fusion_g |> 
  filter(symbol == "HEI.DE", Intervall =="Wöchentlich") 

Henkel_w <- Fusion_g |> 
  filter(symbol == "HEN3.DE", Intervall =="Wöchentlich") 

Infineon_w <- Fusion_g |> 
  filter(symbol == "IFX.DE", Intervall =="Wöchentlich") 

Mercedes_w <- Fusion_g |> 
  filter(symbol == "MBG.DE", Intervall =="Wöchentlich") 

Merck_w <- Fusion_g |> 
  filter(symbol == "MRK.DE", Intervall =="Wöchentlich") 

MTU_w <- Fusion_g |> 
  filter(symbol == "MTX.DE", Intervall =="Wöchentlich") 

MünchnerRück_w <- Fusion_g |> 
  filter(symbol == "MUV2.DE", Intervall =="Wöchentlich") 

Porsche_w <- Fusion_g |> 
  filter(symbol == "P911.DE", Intervall =="Wöchentlich") 

PorscheHolding_w <- Fusion_g |> 
  filter(symbol == "PAH3.DE", Intervall =="Wöchentlich") 

Qiagen_w <- Fusion_g |> 
  filter(symbol == "QIA.DE", Intervall =="Wöchentlich") 

Rheinmetall_w <- Fusion_g |> 
  filter(symbol == "RHM.DE", Intervall =="Wöchentlich") 

RWE_w <- Fusion_g |> 
  filter(symbol == "RWE.DE", Intervall =="Wöchentlich") 

SAP_w <- Fusion_g |> 
  filter(symbol == "SAP.DE", Intervall =="Wöchentlich") 

Sartorius_w <- Fusion_g |> 
  filter(symbol == "SRT3.DE", Intervall =="Wöchentlich") 

Siemens_w <- Fusion_g |> 
  filter(symbol == "SIE.DE", Intervall =="Wöchentlich") 

SiemensEnergy_w <- Fusion_g |> 
  filter(symbol == "ENR.DE", Intervall =="Wöchentlich") 

SiemensHealthineers_w <- Fusion_g |> 
  filter(symbol == "SHL.DE", Intervall =="Wöchentlich") 

Symrise_w <- Fusion_g |> 
  filter(symbol == "SY1.DE", Intervall =="Wöchentlich") 

Volkswagen_w <- Fusion_g |> 
  filter(symbol == "VOW3.DE", Intervall =="Wöchentlich")  

Vonovia_w <- Fusion_g |> 
  filter(symbol == "VNA.DE", Intervall =="Wöchentlich") 

Zalando_w <- Fusion_g |> 
  filter(symbol == "ZAL.DE", Intervall =="Wöchentlich") 


################################################################################
# Monatliche Datensätze
################################################################################

Adidas_m <- Fusion_g |> 
  filter(symbol == "ADS.DE", Intervall =="Monatlich")  

Airbus_m <- Fusion_g |> 
  filter(symbol == "AIR.DE", Intervall =="Monatlich")

Allianz_m <- Fusion_g |> 
  filter(symbol == "ALV.DE", Intervall =="Monatlich") 

BASF_m <- Fusion_g |> 
  filter(symbol == "BAS.DE", Intervall =="Monatlich") 

Bayer_m <- Fusion_g |> 
  filter(symbol == "BAYN.DE", Intervall =="Monatlich") 

Beiersdorf_m <- Fusion_g |> 
  filter(symbol == "BEI.DE", Intervall =="Monatlich") 

BMW_m <- Fusion_g |> 
  filter(symbol == "BMW.DE", Intervall =="Monatlich") 

Brenntag_m <- Fusion_g |> 
  filter(symbol == "BNR.DE", Intervall =="Monatlich") 

Commerzbank_m <- Fusion_g |> 
  filter(symbol == "CBK.DE", Intervall =="Monatlich") 

Continental_m <- Fusion_g |> 
  filter(symbol == "CON.DE", Intervall =="Monatlich") 

Covestro_m <- Fusion_g |> 
  filter(symbol == "1COV.DE", Intervall =="Monatlich") 

DaimlerTruck_m <- Fusion_g |> 
  filter(symbol == "DTG.DE", Intervall =="Monatlich") 

DeutscheBank_m <- Fusion_g |> 
  filter(symbol == "DBK.DE", Intervall =="Monatlich") 

DeutscheBörse_m <- Fusion_g |> 
  filter(symbol == "DB1.DE", Intervall =="Monatlich") 

DeutschePost_m <- Fusion_g |> 
  filter(symbol == "DHL.DE", Intervall =="Monatlich") 

DeutscheTelekom_m <- Fusion_g |> 
  filter(symbol == "DTE.DE", Intervall =="Monatlich") 

EON_m <- Fusion_g |> 
  filter(symbol == "EOAN.DE", Intervall =="Monatlich") 

Fresenius_m <- Fusion_g |> 
  filter(symbol == "FRE.DE", Intervall =="Monatlich") 

HannoverRück_m <- Fusion_g |> 
  filter(symbol == "HNR1.DE", Intervall =="Monatlich") 

HeidelbergMaterial_m <- Fusion_g |> 
  filter(symbol == "HEI.DE", Intervall =="Monatlich") 

Henkel_m <- Fusion_g |> 
  filter(symbol == "HEN3.DE", Intervall =="Monatlich") 

Infineon_m <- Fusion_g |> 
  filter(symbol == "IFX.DE", Intervall =="Monatlich")  

Mercedes_m <- Fusion_g |> 
  filter(symbol == "MBG.DE", Intervall =="Monatlich") 

Merck_m <- Fusion_g |> 
  filter(symbol == "MRK.DE", Intervall =="Monatlich") 

MTU_m <- Fusion_g |> 
  filter(symbol == "MTX.DE", Intervall =="Monatlich") 

MünchnerRück_m <- Fusion_g |> 
  filter(symbol == "MUV2.DE", Intervall =="Monatlich") 

Porsche_m <- Fusion_g |> 
  filter(symbol == "P911.DE", Intervall =="Monatlich") 

PorscheHolding_m <- Fusion_g |> 
  filter(symbol == "PAH3.DE", Intervall =="Monatlich") 

Qiagen_m <- Fusion_g |> 
  filter(symbol == "QIA.DE", Intervall =="Monatlich") 

Rheinmetall_m <- Fusion_g |> 
  filter(symbol == "RHM.DE", Intervall =="Monatlich") 

RWE_m <- Fusion_g |> 
  filter(symbol == "RWE.DE", Intervall =="Monatlich") 

SAP_m <- Fusion_g |> 
  filter(symbol == "SAP.DE", Intervall =="Monatlich") 

Sartorius_m <- Fusion_g |> 
  filter(symbol == "SRT3.DE", Intervall =="Monatlich") 

Siemens_m <- Fusion_g |> 
  filter(symbol == "SIE.DE", Intervall =="Monatlich")  

SiemensEnergy_m <- Fusion_g |> 
  filter(symbol == "ENR.DE", Intervall =="Monatlich") 

SiemensHealthineers_m <- Fusion_g |> 
  filter(symbol == "SHL.DE", Intervall =="Monatlich") 

Symrise_m <- Fusion_g |> 
  filter(symbol == "SY1.DE", Intervall =="Monatlich") 

Volkswagen_m <- Fusion_g |> 
  filter(symbol == "VOW3.DE", Intervall =="Monatlich") 

Vonovia_m <- Fusion_g |> 
  filter(symbol == "VNA.DE", Intervall =="Monatlich") 

Zalando_m <- Fusion_g |> 
  filter(symbol == "ZAL.DE", Intervall =="Monatlich") 


################################################################################
#Versuch mit Schleife
#Gleicher Ansatz wie oben nur cleaner
################################################################################

library(purrr)

# Liste aller Aktien!!
symbole <- c("Adidas", "Airbus", "Allianz", "BASF", "Bayer", "Beiersdorf", "BMW", "Brenntag", "Commerzbank", 
             "Continental", "Covestro", "DaimlerTruck", "DeutscheBank", "DeutscheBörse", "DeutschePost", 
             "DeutscheTelekom", "EON", "Fresenius", "HannoverRück", "HeidelbergMaterial", "Henkel", 
             "Infineon", "Mercedes", "Merck", "MTU", "MünchnerRück", "Porsche", "PorscheHolding", 
             "Qiagen", "Rheinmetall", "RWE", "SAP", "Sartorius", "Siemens", "SiemensEnergy", 
             "SiemensHealthineers", "Symrise", "Volkswagen", "Vonovia", "Zalando")

# Festlegung des Splits = hier 70 % Trainingsdaten!
split_verhältnis_d <- 0.5
split_verhältnis_w <- 0.6
split_verhältnis_m <- 0.7


# Schleife
for (symbol in symbole) {
  # Erstelle den Variablennamen dynamisch
  aktien <- get(paste0(symbol, "_d")) #holt sich die täglichen Datensätze
  
  train_index <- floor(split_verhältnis_d * nrow(aktien)) # Bestimmung des Index von test und trainingsdaten mit Floor wird auf ganze Zahl gerundet
  
  # Aufteilung in test und Trainingsdaten
  train_data <- aktien[1:train_index, ] # von 1 bis zur "Grenze"
  test_data <- aktien[(train_index + 1):nrow(aktien), ] #von Grenze +1 bis zum Ende des Datensatzes
  
  # Erzeugung von neuen Datensätzen
  assign(paste0(symbol, "_d_train_data"), train_data)
  assign(paste0(symbol, "_d_test_data"), test_data)
}

# Erstellung von Test und Trainingsdaten auf wöchentlicher Basis
for (symbol in symbole) {
  aktien <- get(paste0(symbol, "_w"))
  train_index <- floor(split_verhältnis_w * nrow(aktien))
  
  train_data <- aktien[1:train_index, ]
  test_data <- aktien[(train_index + 1):nrow(aktien), ]
  
  assign(paste0(symbol, "_w_train_data"), train_data)
  assign(paste0(symbol, "_w_test_data"), test_data)
}

# Erstellung von Test und Trainingsdaten auf monatlicher Basis
for (symbol in symbole) {
  
  aktien <- get(paste0(symbol, "_m"))
  
  train_index <- floor(split_verhältnis_m * nrow(aktien))
  
  train_data <- aktien[1:train_index, ]
  test_data <- aktien[(train_index + 1):nrow(aktien), ]
  
  assign(paste0(symbol, "_m_train_data"), train_data)
  assign(paste0(symbol, "_m_test_data"), test_data)
}







