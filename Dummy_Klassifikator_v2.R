################################################################################
#Dummy Klassifikator
################################################################################

# Übersicht über Arten eines Dummy Classifiers
# most_frequent: Immer die häufigste Klasse vorhersagen.
# stratified: Zufällige Vorhersagen entsprechend der Klassenverteilung.
# uniform: Gleichverteilte, zufällige Vorhersagen.
# constant: Immer eine festgelegte Klasse vorhersagen.

###################################################################################################
#############--------------------- Tägliche Ergebnisse -------------------------###################
###################################################################################################
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

#leere listen zum fükllen
results <- list()
metriken_liste <- list()
vorhersagen_liste_dummy <- list()

for (i in 1:length(trainings_daten)) {
  model <- dummy_classifier(trainings_daten[[i]]$Kursbewegung_Folgeperiode, strategy = "stratified") #erstellt das dummy modell
  vorhersagen <- predict_dummy_classifier(model, test_daten[[i]]) #erzeugt vorhersagen auf basis der modelle für die testdatensätze
  cm <- confusionMatrix(vorhersagen, test_daten[[i]]$Kursbewegung_Folgeperiode)#konfusionsmatrix für alle testdatensätze
  aktien_name <- aktien_namen[i] #aktienname aus der liste ganz oben gezogen!! 
  
#Ergebnisse speichern als liste
  results[[aktien_name]] <- list(model = model, confusion_matrix = cm)
  
 #das berechnet die metriken aus den confusionsmatrix (NEONEONEO)
  metriken <- data.frame(
    Genauigkeit = cm$overall['Accuracy'],
    Fehlerrate = 1 - cm$overall['Accuracy'],
    Präzision = cm$byClass['Pos Pred Value'],  #Pos Pred Value = Präzision
    Sensitivität = cm$byClass['Sensitivity'],
    Spezifität = cm$byClass['Specificity'],
    F1_Wert = 2 * (cm$byClass['Pos Pred Value'] * cm$byClass['Sensitivity']) / 
      (cm$byClass['Pos Pred Value'] + cm$byClass['Sensitivity'])
  )
  
 #das speichert die metriken auf bsais der liste
  metriken_liste[[aktien_name]] <- metriken
  
 #Vorhersagen speichern
  vorhersage_ergebnis <- tibble(
    Aktienname = aktien_namen[i],
    Tatsächlicher_Wert = test_daten[[i]]$Kursbewegung_Folgeperiode,
    Vorhergesagter_Wert = vorhersagen
  )
  
#diese liste für später für die trading strategie gebraucht, da diese die vorhersagen beinhaltet, erzeugt eine liste auf basis der vorhersagen oben!
  vorhersage_ergebnis <- test_daten[[i]] %>%
    select(date, open, high, low, close, Rendite_Folgetag, Kursbewegung_Folgeperiode) %>%
    mutate(
      Aktienname = aktien_namen[i],              
      Vorhergesagter_Wert = vorhersagen         
    )
  
  vorhersagen_liste_dummy[[aktien_namen[i]]] <- vorhersage_ergebnis 
  
}

########################
# Gesamtmetriken
########################
#Das ist eine liste mit den erzielten metriken als datensatz!!
gesamtmetriken_dummy <- do.call(rbind, metriken_liste)

#erzeugt eine spalte mit den Titel "Aktienname" auf basis der Aktienname liste oben
gesamtmetriken_dummy <- gesamtmetriken_dummy %>%
  mutate(Aktienname = aktien_namen, .before = 1)

gesamtmetriken_dummy$Klassifikator <- "Dummy" #erzeugt wie bei der lr eine Spalte in der über alle zeilen dummy steht, damit später gefiltert werden kann nach Fusion aller metriken
gesamtmetriken_dummy <- as.tibble(gesamtmetriken_dummy) #zum tibble machen (Format vom tidyverse)

# Durchschnittswerte der Metriken berechnen
durchschnitt_metriken_dummy_täglich <- gesamtmetriken %>%
  summarise(
    Durchschnitt_Genauigkeit = mean(Genauigkeit, na.rm = TRUE),
    Durchschnitt_Fehlerrate = mean(Fehlerrate, na.rm = TRUE),
    Durchschnitt_Präzision = mean(Präzision, na.rm = TRUE),
    Durchschnitt_Sensitivität = mean(Sensitivität, na.rm = TRUE),
    Durchschnitt_Spezifität = mean(Spezifität, na.rm = TRUE),
    Durchschnitt_F1_Wert = mean(F1_Wert, na.rm = TRUE)
  )

#Ergebnisse anzeigen
print(gesamtmetriken_dummy)
view(gesamtmetriken_dummy) #datensatz anzeigen für den ganzen
view(durchschnitt_metriken_dummy_täglich)

# Datensatz speichern, als sicherung
write.xlsx(gesamtmetriken_dummy, "gesamtmetriken_dummy.xlsx")







