## Vorhersagen im Zeitverlauf -> TP, FP, TN sowie FN
vorhersagen_zeitverlauf <- strategie_gesamt_alle %>%
  select(date, symbol,Aktienname, Klassifikator, Vorhergesagter_Wert, 
         Portfolio_Rendite, Kursbewegung_Folgeperiode, Rendite_Folgetag, Kumulative_Rendite) %>%
  mutate(Konfusion = case_when(
    Vorhergesagter_Wert == 1 & Kursbewegung_Folgeperiode == 1 ~ "TP",  #True Positives #-> vorhersage = 1 und tatsächliche Bewegung = 1
    Vorhergesagter_Wert == 1 & Kursbewegung_Folgeperiode == 0 ~ "FP",  #False Negative #-> vorhersage = 1 und tatsächliche Bewegung = 0
    Vorhergesagter_Wert == 0 & Kursbewegung_Folgeperiode == 1 ~ "FN",  #False Positive #-> ....
    Vorhergesagter_Wert == 0 & Kursbewegung_Folgeperiode == 0 ~ "TN"   #True Negatives #-> ....
---  ), 
  Rendite_KL_OP = case_when(
    Konfusion %in% c("TP", "FP") ~ Portfolio_Rendite,
    Konfusion %in% c("FN", "TN") ~ -Rendite_Folgetag #hier die Umkehrung der Vorzeichen, um Opportunitätskosten abzubilden
  ))

table(vorhersagen_zeitverlauf$Konfusion)

#kumulierte Ergebnisse der Konfusions-Matrix sowie die Anteile im Zeitverlauf!
vorhersagen_zeitverlauf <- vorhersagen_zeitverlauf %>%
  group_by(Klassifikator, Aktienname) %>%
  mutate(
    kumulierte_TP = cumsum(Konfusion == "TP"),
    kumulierte_FN = cumsum(Konfusion == "FN"),
    kumulierte_FP = cumsum(Konfusion == "FP"),
    kumulierte_TN = cumsum(Konfusion == "TN"),
    gesamt = kumulierte_TP + kumulierte_FN + kumulierte_FP + kumulierte_TN, 
    Anteil_TP = kumulierte_TP / gesamt,
    Anteil_FN = kumulierte_FN / gesamt,
    Anteil_FP = kumulierte_FP / gesamt,
    Anteil_TN = kumulierte_TN / gesamt,
    Anteil_korrekt = Anteil_TP+Anteil_TN,
    Anteil_falsch = Anteil_FP+Anteil_FN
  ) %>%
  ungroup()

view(vorhersagen_zeitverlauf)
str(vorhersagen_zeitverlauf)
vorhersagen_zeitverlauf$Konfusion <- as.character(vorhersagen_zeitverlauf$Konfusion)

#abspeichern falls gewünscht
write.xlsx(vorhersagen_zeitverlauf, "vorhersagen_zeitverlauf.xlsx")

#Durchschnittliche Rendite pro Trade 
durchschnittliche_rendite_konfusion <- vorhersagen_zeitverlauf %>%
  group_by(Klassifikator) %>%
  summarise(
    Rendite_pro_trade = mean(Portfolio_Rendite)*100,
    .groups = "drop"
  )


view(durchschnittliche_rendite_konfusion)
mean(vorhersagen_zeitverlauf$Rendite_Folgetag)*100 # in %
durchschnittliche_rendite_konfusion


#Berechnung der Rendite je Trade für die Klassifatoren je Aktie
opportunität_aktien <- vorhersagen_zeitverlauf %>%
  group_by(Klassifikator, Aktienname) %>%
  summarise(
    
    OP_positiv = mean(Rendite_Folgetag[Vorhergesagter_Wert == "0" & Rendite_Folgetag > 0], na.rm = TRUE)*100,
    OP_negativ = mean(Rendite_Folgetag[Vorhergesagter_Wert == "1" & Rendite_Folgetag < 0], na.rm = TRUE)*100,
    OP_total = OP_positiv + abs(OP_negativ)/2,
    Rendite_pro_trade = mean(Portfolio_Rendite)*100,
    Rendite_pro_Trade_OP = mean(Rendite_KL_OP)*100,
    
    #Renditen je KLassifikation im Durchschnitt
    Rendite_TP = mean(Portfolio_Rendite[Konfusion == "TP"])*100,
    Verlust_FP = mean(Rendite_Folgetag[Konfusion == "FP"])*100,
    Rendite_TN = -mean(Rendite_Folgetag[Konfusion == "TN"])*100, #durch True negative verhinderte Verluste
    Verlust_FN = -mean(Rendite_Folgetag[Konfusion == "FN"])*100, #erlittene Verluste durch falsch negative
    
    
    Anzahl_TP = sum(Konfusion == "TP", na.rm = TRUE),
    Anzahl_FP = sum(Konfusion == "FP", na.rm = TRUE),
    Anzahl_TN = sum(Konfusion == "TN", na.rm = TRUE),
    Anzahl_FN = sum(Konfusion == "FN", na.rm = TRUE),
    Gesamt_Trades = Anzahl_TP + Anzahl_FP + Anzahl_TN + Anzahl_FN
  ) %>%
  mutate(
    Gewichtete_Rendite_pro_Trade_inklusive_OP = (
      (Rendite_TP * Anzahl_TP) + 
        (Verlust_FP * Anzahl_FP) + 
        (Rendite_TN * Anzahl_TN) + 
        (Verlust_FN * Anzahl_FN)
    ) / Gesamt_Trades,
    Profit_Faktor = (Rendite_TP + Rendite_TN) / abs(Verlust_FP + Verlust_FN),
    Gewichte_TP = Anzahl_TP/Gesamt_Trades,
    Gewichte_FP = Anzahl_FP/Gesamt_Trades,
    Gewichte_TN = Anzahl_TN/Gesamt_Trades,
    Gewichte_FN = Anzahl_FN/Gesamt_Trades,
    Gewichtete_Rendite_TP = Rendite_TP*Gewichte_TP,
    Gewichtete_Rendite_FP = Verlust_FP*Gewichte_FP,
    Gewichtete_Rendite_TN = Rendite_TN*Gewichte_TN,
    Gewichtete_Rendite_FN = Verlust_FN*Gewichte_FN,
    Summe_TP_FP_TN_FN_gewichtet = Gewichtete_Rendite_TP+Gewichtete_Rendite_FP+Gewichtete_Rendite_TN+Gewichtete_Rendite_FN
  )


# anschauen
print(opportunität_aktien)
view(opportunität_aktien )

#abspeichern, falls sicherung erwünscht ist
write.xlsx(opportunität_aktien, "Trade-Ergebnisse_alleModelle.xlsx")


opportunität_aktien_zusammenfassung <-opportunität_aktien %>%
  group_by(Klassifikator) %>%
  summarise(
    Rendite_pro_Trade = mean(Rendite_pro_trade, na.rm = TRUE),
    Rendite_pro_trade_inklusive_OP = mean(Rendite_pro_Trade_OP),
    Rendite_TP = mean(Rendite_TP, na.rm = TRUE),   
    Verlust_FP = mean(Verlust_FP, na.rm = TRUE),   
    Rendite_TN = mean(Rendite_TN, na.rm = TRUE),   
    Verlust_FN = mean(Verlust_FN, na.rm = TRUE),
    Rendite_pro_trade_inklusive_OP_alt = mean(Rendite_TP, na.rm = TRUE)+mean(Verlust_FP, na.rm = TRUE)+mean(Rendite_TN, na.rm = TRUE)+mean(Verlust_FN, na.rm = TRUE),
    Gewichtete_Rendite_pro_Trade_inklusive_OP = mean(Gewichtete_Rendite_pro_Trade_inklusive_OP),
  )

view(opportunität_aktien_zusammenfassung)

#Rendite pro Vorhersage für Buy and Hold Buy-and-Hold berechnen, relativ easy!
tägliche_rendite_bah <- vorhersagen_zeitverlauf %>%
  group_by(Aktienname) %>%
  summarize(
    tägliche_rendite = mean(Rendite_Folgetag, na.rm = TRUE) 

mean(tägliche_rendite_bah$tägliche_rendite)

view(tägliche_rendite_bah)

view(opportunität_aktien_zusammenfassung)
write.xlsx(opportunität_aktien_zusammenfassung, "Trade-Ergebnisse_Auf Aktien Basis.xlsx")



