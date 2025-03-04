# logistische regression

# Leere Aktiennamen für liste
aktien_namen <- c("Adidas_d", "Airbus_d", "Allianz_d", "BASF_d", "Bayer_d", "Beiersdorf_d", "BMW_d", 
                  "Brenntag_d", "Commerzbank_d", "Continental_d", "Covestro_d", "DaimlerTruck_d", 
                  "DeutscheBank_d", "DeutscheBörse_d", "DeutschePost_d", "DeutscheTelekom_d", 
                  "EON_d", "Fresenius_d", "HannoverRück_d", "HeidelbergMaterial_d", "Henkel_d", 
                  "Infineon_d", "Mercedes_d", "Merck_d", "MTU_d", "MünchnerRück_d", "Porsche_d", 
                  "PorscheHolding_d", "Qiagen_d", "Rheinmetall_d", "RWE_d", "SAP_d", "Sartorius_d", 
                  "Siemens_d", "SiemensEnergy_d", "SiemensHealthineers_d", "Symrise_d", 
                  "Volkswagen_d", "Vonovia_d", "Zalando_d")

#Liste mit allen Trainingsdatensätzen
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

#Liste mit allen 
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

# Leere Listen zunm füllen
results <- list()
vip_liste <- list()
metriken_liste <- list()
vorhersagen_liste_lr <- list()

for (i in 1:length(trainings_daten)) {
  
 # Modelle glm = weil binomial, alle 
 model <- glm(Kursbewegung_Folgeperiode ~  SMA_10+SMA_20+SMA_50+WMA_10+WMA_20+WMA_50+
                 EMA_10+EMA_20+EMA_50+MACD_Linie+ADX+Williams_R_14+Stoch_K+ROC+RSI+OBV+CMF+VWAP_10+VWAP_20+VWAP_50,
                 data = trainings_daten[[i]], family = "binomial")

  vorhersagen <- predict(model, test_daten[[i]], type = "response")   # Vorhersagen werden hier in Klassen umgewandelt -> ab 50 % wird 1 vorhersagt, sonst 0 
  vorhersagen <- ifelse(vorhersagen > 0.5, 1, 0)
  
  # Koinfusionsmatrix und variablen importance 
  cm <- confusionMatrix(as.factor(vorhersagen), as.factor(test_daten[[i]]$Kursbewegung_Folgeperiode)) 
  vi <- (model, method = "model", scale = TRUE, type = "raw")
  
  #metriken berechnen
  metriken <- data.frame(
    Genauigkeit = cm$overall['Accuracy'],
    Fehlerrate = 1 - cm$overall['Accuracy'],
    Präzision = cm$byClass['Pos Pred Value'],  
    Sensitivität = cm$byClass['Sensitivity'],
    Spezifität = cm$byClass['Specificity'],
    F1_Wert = 2 * (cm$byClass['Pos Pred Value'] * cm$byClass['Sensitivity']) / 
      (cm$byClass['Pos Pred Value'] + cm$byClass['Sensitivity'])
  )
  
  #Ergebnis mit dem entsprechenden Namen speichern
  aktien_name <- aktien_namen[i]
  results[[aktien_name]] <- list(model = model, confusion_matrix = cm)
  vip_liste[[aktien_name]] <- vi
  metriken_liste[[aktien_name]] <- metriken
  
  #Vorhersagen speichern, wird später gebraucht für die trading strategie
  vorhersage_ergebnis <- tibble(
    Aktienname = aktien_namen[i],
    Tatsächlicher_Wert = test_daten[[i]]$Kursbewegung_Folgeperiode,
    Vorhergesagter_Wert = vorhersagen
  )
  
  vorhersage_ergebnis <- test_daten[[i]] %>%
    select(date, open, high, low, close, Rendite_Folgetag, Kursbewegung_Folgeperiode) %>% 
    mutate(
      Aktienname = aktien_namen[i],              
      Vorhergesagter_Wert = vorhersagen         
    )
  
  #wird später für die Trading strategie gebraucht!!
  vorhersagen_liste_lr[[aktien_namen[i]]] <- vorhersage_ergebnis
  
}


#Alle Metriken in der liste speichern
gesamtmetriken <- do.call(rbind, metriken_liste)

#Durchschnittswerte der Metriken berechnen
durchschnitt_metriken_lr <- gesamtmetriken %>%
  summarise(
    Durchschnitt_Genauigkeit = mean(Genauigkeitvorhersagen),
    Durchschnitt_Fehlerrate = mean(Fehlerratevorhersagen),
    Durchschnitt_Präzision = mean(Präzisionvorhersagen),
    Durchschnitt_Sensitivität = mean(Sensitivitätvorhersagen),
    Durchschnitt_Spezifität = mean(Spezifitätvorhersagen),
    Durchschnitt_F1_Wert = mean(F1_Wertvorhersagen)
  )


# Datensatz erstellen für die gesamtmetriken
gesamtmetriken_lr <- gesamtmetriken %>%
  mutate(Aktienname = aktien_namen, .before = 1)

gesamtmetriken_lr$Klassifikator <- "Logistische Regression" #spalte erstellen, um später filtern zu können
gesamtmetriken_lr <- as_tibble(gesamtmetriken_lr)

durchschnitt_metriken_lr <- gesamtmetriken %>%
  summarise(
    Durchschnitt_Genauigkeit = mean(Genauigkeitvorhersagen),
    Durchschnitt_Fehlerrate = mean(Fehlerratevorhersagen),
    Durchschnitt_Präzision = mean(Präzisionvorhersagen),
    Durchschnitt_Sensitivität = mean(Sensitivitätvorhersagen),
    Durchschnitt_Spezifität = mean(Spezifitätvorhersagen),
    Durchschnitt_F1_Wert = mean(F1_Wertvorhersagen)
  )


# Ergebnisse anzeigen (Vairable importance und metriken :) Die Feature importance wird anhand der ß1 Werte berechnet
print(durchschnitt_vip_lr)
print(durchschnitt_metriken_lr)
print(gesamtmetriken_lr, n = 40)
view(durchschnitt_metriken_lr)

#Datensatz der Ergebnisse speichern, zur Sicherheit
write.xlsx(gesamtmetriken_lr, "gesamtmetriken_lr.xlsx")




