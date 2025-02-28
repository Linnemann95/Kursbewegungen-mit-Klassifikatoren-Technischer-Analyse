#Mögliche diagrammfarben 
#Farben
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

"#e31a1c" "#2c3e50"
"#eb3c49" "#274a68" "#fe7070" "#c3f7c8"

# Erstellung eines Gesamtdatensatzes und ggf. abspeichern zur Sicherung
metriken_alle <- bind_rows(gesamtmetriken_rf, gesamtmetriken_nb, gesamtmetriken_lr, gesamtmetriken_dummy)
metriken_alle <- as.tibble(metriken_alle) #zum Tibble (SICHerheit geht vor)
write.xlsx(metriken_alle, "metriken_alle.xlsx")

# einlesen und umbenennen
metriken_alle <- read.xlsx(file.choose())

# Verkürzt die Namen in der Klassifikatorspalte. So ist die Weiterbearbeitung einfacher -> case_when praktisch wie eine wenn Funktion
metriken_alle  <- metriken_alle  %>%
  mutate(Klassifikator = case_when(
    Klassifikator == "Random Forest" ~ "RF",
    Klassifikator == "Logistische Regression" ~ "LR",
    Klassifikator == "Dummy" ~ "DU",
    Klassifikator == "Naive Bayes" ~ "NB" 
  ))



metriken_alle <- as_tibble(metriken_alle)
view(metriken_alle)
str(metriken_alle)

# Durschnittswerte berechnen -> falls nochmal gewünscht
metriken_durchschnitte <- metriken_alle %>%
  group_by(Klassifikator) %>%
  summarise(
    genauigkeit = mean(Genauigkeit),
    fehlerrate = mean(Fehlerrate),
    Präzision = mean(Präzision),
    sensitivitaet = mean(Sensitivität),
    spezifitaet = mean(Spezifität),
    f1score = mean(F1_Wert),
  )

# Ergebnisse ansehen und sabspeichern
view(metriken_quantile)
write.xlsx(metriken_quantile, "Durchschnittswerte_Metriken.xlsx")


#Quartile berechnen
metriken_quantile <- metriken_alle %>%
  group_by(Klassifikator) %>%
  summarise(
    q1_genauigkeit = quantile(Genauigkeit, 0.25),
    q2_genauigkeit = quantile(Genauigkeit, 0.50),
    q3_genauigkeit = quantile(Genauigkeit, 0.75),
    
    q1_fehlerrate = quantile(Fehlerrate, 0.25),
    q2_fehlerrate = quantile(Fehlerrate, 0.50), 
    q3_fehlerrate = quantile(Fehlerrate, 0.75),
    
    q1_Präzision = quantile(Präzision, 0.25),
    q2_Präzision = quantile(Präzision, 0.50), 
    q3_Präzision = quantile(Präzision, 0.75),
    
    q1_sensitivitaet = quantile(Sensitivität, 0.25),
    q2_sensitivitaet = quantile(Sensitivität, 0.50), 
    q3_sensitivitaet = quantile(Sensitivität, 0.75),
    
    q1_spezifitaet = quantile(Spezifität, 0.25),
    q2_spezifitaet = quantile(Spezifität, 0.50),
    q3_spezifitaet = quantile(Spezifität, 0.75),
    
    q1_f1score = quantile(F1_Wert, 0.25),
    q2_f1score = quantile(F1_Wert, 0.50), 
    q3_f1score = quantile(F1_Wert, 0.75)
    
  )

# Ergebnisse ansehen und sabspeichern
view(metriken_quantile)
write.xlsx(metriken_quantile, "Quartile_Metriken.xlsx")







