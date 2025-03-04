
###################################################################################################
#############--------------------- Tägliche Ergebnisse -------------------------###################
###################################################################################################
library(randomForest) #falls noch nicht geladen, sollte aber in der Datensatzerstellungs-Datei gemacht worden sein

# Aktiennamen
aktien_namen <- c("Adidas_d", "Airbus_d", "Allianz_d", "BASF_d", "Bayer_d", "Beiersdorf_d", "BMW_d", 
                  "Brenntag_d", "Commerzbank_d", "Continental_d", "Covestro_d", "DaimlerTruck_d", 
                  "DeutscheBank_d", "DeutscheBörse_d", "DeutschePost_d", "DeutscheTelekom_d", 
                  "EON_d", "Fresenius_d", "HannoverRück_d", "HeidelbergMaterial_d", "Henkel_d", 
                  "Infineon_d", "Mercedes_d", "Merck_d", "MTU_d", "MünchnerRück_d", "Porsche_d", 
                  "PorscheHolding_d", "Qiagen_d", "Rheinmetall_d", "RWE_d", "SAP_d", "Sartorius_d", 
                  "Siemens_d", "SiemensEnergy_d", "SiemensHealthineers_d", "Symrise_d", 
                  "Volkswagen_d", "Vonovia_d", "Zalando_d")

# Listen mit allen Trainings- und Testdatensätzen
trainings_daten <- list(
  Adidas_d_train_data, Airbus_d_train_data, Allianz_d_train_data, BASF_d_train_data, Bayer_d_train_data, 
  Beiersdorf_d_train_data, BMW_d_train_data, Brenntag_d_train_data, Commerzbank_d_train_data, Continental_d_train_data, 
  Covestro_d_train_data, DaimlerTruck_d_train_data, DeutscheBank_d_train_data, DeutscheBörse_d_train_data, 
  DeutschePost_d_train_data, DeutscheTelekom_d_train_data, EON_d_train_data, Fresenius_d_train_data, 
  HannoverRück_d_train_data, HeidelbergMaterial_d_train_data, Henkel_d_train_data, Infineon_d_train_data, 
  Mercedes_d_train_data, Merck_d_train_data, MTU_d_train_data, MünchnerRück_d_train_data, Porsche_d_train_data, 
  PorscheHolding_d_train_data, Qiagen_d_train_data, Rheinmetall_d_train_data, RWE_d_train_data, SAP_d_train_data, 
  Sartorius_d_train_data, Siemens_d_train_data, SiemensEnergy_d_train_data, SiemensHealthineers_d_train_data, 
  Symrise_d_train_data, Volkswagen_d_train_data, Vonovia_d_train_data, Zalando_d_train_data)

test_daten <- list(
  Adidas_d_test_data, Airbus_d_test_data, Allianz_d_test_data, BASF_d_test_data, Bayer_d_test_data, 
  Beiersdorf_d_test_data, BMW_d_test_data, Brenntag_d_test_data, Commerzbank_d_test_data, Continental_d_test_data, 
  Covestro_d_test_data, DaimlerTruck_d_test_data, DeutscheBank_d_test_data, DeutscheBörse_d_test_data, 
  DeutschePost_d_test_data, DeutscheTelekom_d_test_data, EON_d_test_data, Fresenius_d_test_data, 
  HannoverRück_d_test_data, HeidelbergMaterial_d_test_data, Henkel_d_test_data, Infineon_d_test_data, 
  Mercedes_d_test_data, Merck_d_test_data, MTU_d_test_data, MünchnerRück_d_test_data, Porsche_d_test_data, 
  PorscheHolding_d_test_data, Qiagen_d_test_data, Rheinmetall_d_test_data, RWE_d_test_data, SAP_d_test_data, 
  Sartorius_d_test_data, Siemens_d_test_data, SiemensEnergy_d_test_data, SiemensHealthineers_d_test_data, 
  Symrise_d_test_data, Volkswagen_d_test_data, Vonovia_d_test_data, Zalando_d_test_data)

#Leere Listen für die Ergebnisse, VIP und der Metriken
ergebnisse <- list()
wichtigkeit_liste <- list()
metriken_liste <- list()
vorhersagen_liste_rf <- list()

# Schleife über die verschiedenen Datensätze
for (i in 1:length(trainings_daten)) {
  
  # Modell erstellen
  modell <- randomForest(Kursbewegung_Folgeperiode ~ SMA_10+SMA_20+SMA_50+WMA_10+WMA_20+WMA_50+EMA_10+EMA_20+EMA_50+MACD_Linie+ADX+Williams_R_14+Stoch_K+ROC+RSI+OBV+CMF+VWAP_10+VWAP_20+VWAP_50,
                         ntree = 500,  #hier einfach die gewünschten Hyperparameter eintragen. Zum Beispiel ntree (Anzahl der Bäume) = 100
                         mtry = 4, 
                         nodesize = 1,
                         maxnodes = 28,
                         data = trainings_daten[[i]])
  
  # Vorhersagen
  vorhersagen <- predict(modell, test_daten[[i]])
  
  # Konfusionsmatrix berechnen
  konfusionsmatrix <- confusionMatrix(vorhersagen, test_daten[[i]]$Kursbewegung_Folgeperiode)
  
  # Variable Importance (VIP) berechnen
  wichtigkeit <- vi(modell,scale = TRUE)
  
  # Ergebnisse speichern
  aktien_name <- aktien_namen[i]
  ergebnisse[[aktien_name]] <- list(modell = modell, konfusionsmatrix = konfusionsmatrix)
  wichtigkeit_liste[[aktien_name]] <- wichtigkeit
  
  # Metriken berechnen
  metriken <- tibble(
    Genauigkeit = konfusionsmatrix$overall['Accuracy'],
    Fehlerrate = 1 - konfusionsmatrix$overall['Accuracy'],
    Präzision = konfusionsmatrix$byClass['Pos Pred Value'],  # Pos Pred Value = Präzision
    Sensitivität = konfusionsmatrix$byClass['Sensitivity'],
    Spezifität = konfusionsmatrix$byClass['Specificity'],
    F1_Wert = 2 * (konfusionsmatrix$byClass['Pos Pred Value'] * konfusionsmatrix$byClass['Sensitivity']) / 
      (konfusionsmatrix$byClass['Pos Pred Value'] + konfusionsmatrix$byClass['Sensitivity'])
  )
  
  #speichern der metriken
  metriken_liste[[aktien_namen[i]]] <- metriken
  
  #Vorhersagen speichern
    vorhersage_ergebnis <- tibble(
    Aktienname = aktien_namen[i],
    Tatsächlicher_Wert = test_daten[[i]]$Kursbewegung_Folgeperiode,
    Vorhergesagter_Wert = vorhersagen
  )
  
  vorhersage_ergebnis <- test_daten[[i]] %>%
    select(date, open, high, low, close, Rendite_Folgetag, Kursbewegung_Folgeperiode) %>%  #spalten für den Vorhersage Datensatz
    mutate(
      Aktienname = aktien_namen[i],              
      Vorhergesagter_Wert = vorhersagen         
    )
  
  #Vorhersagen in einer Liste speichern
  vorhersagen_liste_rf[[aktien_namen[i]]] <- vorhersage_ergebnis
}


#Die Metriken in einem Datensatz zusammenfügen
gesamtmetriken <- do.call(rbind, metriken_liste)

# Gesamtmetriken anzeigen lassen + Durchschnittswerte der Metriken berechnen + Aktiennamen in der ersten Spalte anzeigen lassen + zur Sicherheit in Tibble-Format bringen
gesamtmetriken_rf <- gesamtmetriken %>%
  mutate(Aktienname = aktien_namen, .before = 1)

gesamtmetriken_rf$Klassifikator <- "Random Forest" #Spalte "Klassifikator" erzeugen mit 
gesamtmetriken_rf <- as.tibble(gesamtmetriken_rf)

#Berechnung der Durchschnittsmetriken
durchschnitt_metriken_rf <- gesamtmetriken %>%
  summarise(
    Durchschnitt_Genauigkeit = mean(Genauigkeit ),
    Durchschnitt_Fehlerrate = mean(Fehlerrate),
    Durchschnitt_Präzision = mean(Präzision),
    Durchschnitt_Sensitivität = mean(Sensitivität),
    Durchschnitt_Spezifität = mean(Spezifität),
    Durchschnitt_F1_Wert = mean(F1_Wert)
  )

#Ergebnisse der Metriken anzeigen
print(gesamtmetriken_rf, n = 40) #hier alle 40 anzeigen lassen
view(durchschnitt_metriken_rf)
view(gesamtmetriken_rf)
print(durchschnitt_vip_rf)


#Die einzelnen Vorhersagen in einem Datensatz speichern -> wird später für die Handelsstrategie gebraucht!
vorhersagen_gesamt <- bind_rows(vorhersagen_liste)
print(vorhersagen_liste)
view(vorhersagen_gesamt)

vorhersagen_gesamt_rf <- vorhersagen_gesamt
vorhersagen_gesamt_rf$Klassifikator <- "Random Forest"
print(vorhersagen_gesamt_rf)
view(vorhersagen_gesamt_rf)
vorhersagen_gesamt_rf


#Daten speichern -> zur Sicherung
write.xlsx(vorhersagen_gesamt_rf, "vorhersagen_gesamt_rf.xlsx")
write.xlsx(gesamtmetriken_rf, "gesamtmetriken_rf.xlsx")
