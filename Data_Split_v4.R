# 1. Aufteilung des Gesamtdatensatzes in einzelne Datensätze
# 2. Aufteilung in Trainings und Testdaten mit der Holdout Methode 

################################################################################
# Tägliche Datensätze # Alternative mit na.omit() -> aber erstmal weglassen
# schleife ging nicht, daher manuelle Aufteilung des gesamtdatensatzes
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
#Versuch mit Schleife
#

################################################################################


# Liste aller Aktien!!
symbole <- c("Adidas", "Airbus", "Allianz", "BASF", "Bayer", "Beiersdorf", "BMW", "Brenntag", "Commerzbank", 
             "Continental", "Covestro", "DaimlerTruck", "DeutscheBank", "DeutscheBörse", "DeutschePost", 
             "DeutscheTelekom", "EON", "Fresenius", "HannoverRück", "HeidelbergMaterial", "Henkel", 
             "Infineon", "Mercedes", "Merck", "MTU", "MünchnerRück", "Porsche", "PorscheHolding", 
             "Qiagen", "Rheinmetall", "RWE", "SAP", "Sartorius", "Siemens", "SiemensEnergy", 
             "SiemensHealthineers", "Symrise", "Volkswagen", "Vonovia", "Zalando")

# Festlegung des Splits = hier 60 % Trainingsdaten!
split_verhältnis_d <- 0.6

# Schleife
for (symbol in symbole) {
  # Erstelle den Variablennamen dynamisch
  aktien <- get(paste0(symbol, "_d")) #holt sich die täglichen Datensätze
  
  train_index <- floor(split_verhältnis_d * nrow(aktien)) # Bestimmung des Index von test und trainingsdaten mit Floor wird auf ganze Zahl gerundet
  
  # Aufteilung in test und Trainingsdaten
  train_data <- aktien[1:train_index, ] # von 1 bis zur "Grenze"
  test_data <- aktien[(train_index + 1):nrow(aktien), ] #von Grenze +1 bis zum Ende des Datensatzes
  
  # Erzeugung von neuen Datensätzen -> "jeweils trains data und Test data"
  assign(paste0(symbol, "_d_train_data"), train_data)
  assign(paste0(symbol, "_d_test_data"), test_data)
}







