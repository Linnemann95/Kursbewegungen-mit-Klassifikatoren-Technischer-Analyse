# Strategie für RF
##################
##################


#Strategie für jede Aktie berechnen
#Leere Liste erstelen -> map2 nimmt die vorhersagen aus den modellen, aber berechnung erfolgt natürlicch getrennt nach Aktie, da sonst nur gibberish rauskommt
strategie_ergebnisse_rf <- map2(vorhersagen_liste_rf, aktien_namen, function(daten, name) {
  
#mit mutate neue spalten erzeugen.Zum einen Position, die von vorhergesagter wert abhängig ist, die Rendite für den Tag der Klassifikatoren (portfolio_rendite) und die kumulative rendite 
daten <- daten %>%
mutate(
   Position = case_when(
   Vorhergesagter_Wert == 1 ~ 1,   #Postionierung erfolgt so, dass bei einer aufwärtsvorhersage ("1") die Postionierung auch "1" ist, und bei "0" genauso (case when braucht eine tilde und kein =)
   Vorhergesagter_Wert == 0 ~ 0,), #Im Endeffekt sind die Spatel Position Vorhergesager wert und Position exakt gleich, aber zur visuellen Trennung wird das trotzdem gemacht
      
      Portfolio_Rendite = if_else(Position == 1, Rendite_Folgetag, 0), #neue spalte "portofolio rendite" -> rendite wird jetzt nur genommen, wenn die Position auf "1" steht, wir also halten oder kaufen, 
								       #ansonsten ist die rendite = 0
      Wachstumsfaktor = 1 + Portfolio_Rendite, #das ist der Wachstumsfaktor für die kumulierte Rendite, also die tägliche Rendite +1, da sonst nicht multipliziert werden kann 
      Kumulative_Rendite = cumprod(Wachstumsfaktor) - 1  #multiplikation der Wachstumsfaktoren und zum schluss -1 dann hat man die rendite in % (im prinzip wie die formel aus den büchern)
    )
  
  # Aktienname hinzufügen, damit wir später wissen, zu welcher Aktie die Daten gehören
  daten$Aktie <- name

  return(daten) 
})

#Alle Aktien-Daten zusammenfügen und eine spalte "RF" erzeugen für Random forest
strategie_gesamt_rf <- bind_rows(strategie_ergebnisse_rf) %>%
  mutate(
    Klassifikator = "RF",  
    Vorhergesagter_Wert = as.factor(Vorhergesagter_Wert) #zum factor sicherhaltshalber umwandeln
  )

# Ergebnis ausgeben
print(strategie_gesamt_rf)




# Strategie für NB exakt wie oben daher keine kommentare
##################
##################
##################
##################
strategie_ergebnisse_nb <- map2(vorhersagen_liste_nb, aktien_namen, function(daten, name) {
  
daten <- daten %>%
mutate(
   Position = case_when(
   Vorhergesagter_Wert == 1 ~ 1,   
   Vorhergesagter_Wert == 0 ~ 0,), 
   Portfolio_Rendite = if_else(Position == 1, Rendite_Folgetag, 0), 
								      
      Wachstumsfaktor = 1 + Portfolio_Rendite, 
      Kumulative_Rendite = cumprod(Wachstumsfaktor) - 1  
    )
  
  daten$Aktie <- name

  return(daten) 
})

#Alle Aktien-Daten zusammenfügen und eine spalte "RF" erzeugen für Random forest
strategie_gesamt_nb <- bind_rows(strategie_ergebnisse_nb) %>%
  mutate(
    Klassifikator = "NB",  
    Vorhergesagter_Wert = as.factor(Vorhergesagter_Wert) #zum factor sicherhaltshalber umwandeln
  )

# Ergebnis ausgeben
print(strategie_gesamt_nb)




# Strategie für lr
##################
##################
##################
##################
strategie_ergebnisse_lr <- map2(vorhersagen_liste_lr, aktien_namen, function(daten, name) {
  
daten <- daten %>%
mutate(
   Position = case_when(
   Vorhergesagter_Wert == 1 ~ 1,   
   Vorhergesagter_Wert == 0 ~ 0,), 
   Portfolio_Rendite = if_else(Position == 1, Rendite_Folgetag, 0), 
								      
      Wachstumsfaktor = 1 + Portfolio_Rendite, 
      Kumulative_Rendite = cumprod(Wachstumsfaktor) - 1  
    )
  
  daten$Aktie <- name

  return(daten) 
})

#Alle Aktien-Daten zusammenfügen und eine spalte "RF" erzeugen für Random forest
strategie_gesamt_lr <- bind_rows(strategie_ergebnisse_lr) %>%
  mutate(
    Klassifikator = "LR",  
    Vorhergesagter_Wert = as.factor(Vorhergesagter_Wert) #zum factor sicherhaltshalber umwandeln
  )

print(strategie_gesamt_lr)


# Strategie für Dummy
##################
##################
##################
##################
strategie_ergebnisse_dummy <- map2(vorhersagen_liste_dummy, aktien_namen, function(daten, name) {
  
daten <- daten %>%
mutate(
   Position = case_when(
   Vorhergesagter_Wert == 1 ~ 1,   
   Vorhergesagter_Wert == 0 ~ 0,), 
   Portfolio_Rendite = if_else(Position == 1, Rendite_Folgetag, 0), 
								      
      Wachstumsfaktor = 1 + Portfolio_Rendite, 
      Kumulative_Rendite = cumprod(Wachstumsfaktor) - 1  
    )
  
  daten$Aktie <- name

  return(daten) 
})

#Alle Aktien-Daten zusammenfügen und eine spalte "RF" erzeugen für Random forest
strategie_gesamt_dummy <- bind_rows(strategie_ergebnisse_dummy) %>%
  mutate(
    Klassifikator = "DU",  
    Vorhergesagter_Wert = as.factor(Vorhergesagter_Wert)
  )

print(strategie_gesamt_dummy)



###################################################
#Zusammenführung der Datensätze der Klassifikatoren
###################################################

strategie_gesamt_alle <- bind_rows(strategie_gesamt_rf, strategie_gesamt_nb, 
                                   strategie_gesamt_lr, strategie_gesamt_dummy)
view(strategie_gesamt_alle)

#Jetzt fehlen noch die Rendite der Buy and Hold Strategie (BAH)
#Diese ist ungleich leichter zu berechnen  
strategie_gesamt_alle <- strategie_gesamt_alle %>%
  group_by(symbol, Klassifikator) %>%  # 
  mutate(
    Wachstumsfaktor_BAH = 1 + Rendite_Folgetag,  # Tageswachstumsfaktor
    Buy_and_Hold = cumprod(Wachstumsfaktor_BAH) - 1  # Kumulatives Produkt
  ) %>%
  mutate(Differenz = Kumulative_Rendite - Buy_and_Hold) %>%
  ungroup()  # Gruppierung aufheben

#nochmal anschauen um sicher zu gehen, ob alles stimmt
view(strategie_gesamt_alle)

#Der letzte Tag ist "na", da es keine folgerendite gibt. Daher werden diese entfernt
strategie_gesamt_alle <- strategie_gesamt_alle %>%
  na.omit()

#abspeichern des datensatzes
write.xlsx(strategie_gesamt_alle, "Ergebnisse_Trading.xlsx")

######################################################################################################################################################################
#kennzahlen berechnen -> mit hilfe von performacne analystics. Die Funktion ist praktisch wie bei den anderen Tidyverse paketen, also einfach mit summarise getrennt nach Klassifikatoren und Aktie berechnen lassen
#Die Kennzahlen werden jeweils getrennt berechnet -> summarise erzeugt die spalten mit den jeweiligen Formeln -> zum beispiel ist StdDev die Vola oder 
#die Abkürzung BAH steht für "buy and hold", "KL" für Klassifikatoren
#scale 252 ist einfach die anzahl an handelstagen die angenommen wird, das sind üblicherweise 252 
######################################################################################################################################################################

#paket laden
library(PerformanceAnalytics)

risikofreier_zins <- 0.024 # für sharpe ratio -> wird aber in der tatsächlichen Arbeit nicht berechnet, ist nur zum testen :)

kennzahlen <- strategie_gesamt_alle %>%
  group_by(symbol, Klassifikator) %>%
  summarise(
    Vola_Klassifikatoren = StdDev(Portfolio_Rendite),
    Vola_BAH = StdDev(Rendite_Folgetag),
    Vola_Klassifikatoren_annualisiert = StdDev.annualized(Portfolio_Rendite, scale = 252), 
    Vola_BAH_annulisiert = StdDev.annualized(Rendite_Folgetag, scale = 252),
    Annualisierte_Rendite_Kl = Return.annualized(Portfolio_Rendite, scale = 252), 
    Annualisierte_Rendite_BAH = Return.annualized(Rendite_Folgetag, scale = 252),
    Drawdown_Klassifikatoren = maxDrawdown(Portfolio_Rendite), 
    Drawdown_BAH = maxDrawdown(Rendite_Folgetag), 
    Kumulative_Rendite = last(Kumulative_Rendite), 
    Buy_and_Hold_Rendite = Buy_and_Hold,
    Buy_and_Hold_kumulierte_final = last(Buy_and_Hold),
    Risiko_Return_KL = Annualisierte_Rendite_Kl / Vola_Klassifikatoren_annualisiert,
    Risiko_Return_BAH = Annualisierte_Rendite_BAH / Vola_BAH_annulisiert,
    .groups = "drop"
  ) 

#anschauen, ob alles passt !!!
view(kennzahlen)

#abspeichern, falls code gebreakt wird 
write.xlsx(kennzahlen, "Kennzahlen.xlsx")


#berechnen von Durchschnittwerten und quantile respektive quartile
mean(kennzahlen$Annualisierte_Rendite_Kl)

mittelwert_rendite_Gruppe <- kennzahlen %>%
  group_by(Klassifikator) %>% 
  summarize(Mittelwert_Rendite = mean(Annualisierte_Rendite_Kl, na.rm = TRUE)) %>%
  print()

kennzahlen <- read.xlsx(file.choose())

#Quatile der Kennzahlen für alle Klassikfatoren 
quartile_kennzahlen_klassifikatoren <- kennzahlen %>%
  group_by(Klassifikator) %>% 
  summarize(
    Q1_kr = quantile(Kumulative_Rendite, 0.25, na.rm = TRUE),
    Q2_kr = quantile(Kumulative_Rendite, 0.5, na.rm = TRUE),
    Q3_kr = quantile(Kumulative_Rendite, 0.75, na.rm = TRUE),
    Q1_dd_kl = quantile(Drawdown_Klassifikatoren, 0.25, na.rm = TRUE),
    Q2_dd_kl = quantile(Drawdown_Klassifikatoren, 0.5, na.rm = TRUE),
    Q3_dd_kl = quantile(Drawdown_Klassifikatoren, 0.75, na.rm = TRUE),
    Q1_v_kl = quantile(Vola_Klassifikatoren_annualisiert, 0.25, na.rm = TRUE),
    Q2_v_kl = quantile(Vola_Klassifikatoren_annualisiert, 0.5, na.rm = TRUE),
    Q3_v_kl = quantile(Vola_Klassifikatoren_annualisiert, 0.75, na.rm = TRUE),
    Q1_ar_kl = quantile(Annualisierte_Rendite_Kl, 0.25, na.rm = TRUE),
    Q2_ar_kl = quantile(Annualisierte_Rendite_Kl, 0.5, na.rm = TRUE),
    Q3_ar_kl = quantile(Annualisierte_Rendite_Kl, 0.75, na.rm = TRUE),
    Q1_rr_kl = quantile(Risiko_Return_KL, 0.25, na.rm = TRUE),
    Q2_rr_kl = quantile(Risiko_Return_KL, 0.5, na.rm = TRUE),
    Q3_rr_kl = quantile(Risiko_Return_KL, 0.75, na.rm = TRUE),
    Q1_dd_bah = quantile(Drawdown_BAH, 0.25, na.rm = TRUE),
    Q2_dd_bah = quantile(Drawdown_BAH, 0.5, na.rm = TRUE),
    Q3_dd_bah = quantile(Drawdown_BAH, 0.75, na.rm = TRUE),
    Q1_kr_bah = quantile(Buy_and_Hold_Rendite, 0.25, na.rm = TRUE),
    Q2_kr_bah = quantile(Buy_and_Hold_Rendite, 0.5, na.rm = TRUE),
    Q3_kr_bah = quantile(Buy_and_Hold_Rendite, 0.75, na.rm = TRUE),
    Q1_ar_bah = quantile(Annualisierte_Rendite_BAH, 0.25, na.rm = TRUE),
    Q2_ar_bah = quantile(Annualisierte_Rendite_BAH, 0.5, na.rm = TRUE),
    Q3_ar_bah = quantile(Annualisierte_Rendite_BAH, 0.75, na.rm = TRUE),
    Q1_v_bah = quantile(Vola_BAH_annulisiert, 0.25, na.rm = TRUE),
    Q2_v_bah = quantile(Vola_BAH_annulisiert, 0.5, na.rm = TRUE),
    Q3_v_bah = quantile(Vola_BAH_annulisiert, 0.75, na.rm = TRUE),
    Q1_rr_bah = quantile(Risiko_Return_BAH, 0.25, na.rm = TRUE),
    Q2_rr_bah = quantile(Risiko_Return_BAH, 0.5, na.rm = TRUE),
    Q3_rr_bah = quantile(Risiko_Return_BAH, 0.75, na.rm = TRUE)
  ) %>%
  print()

#abspeichern und anschauen
write.xlsx(quartile_kennzahlen_klassifikatoren, "quartile_kennzahlen.xlsx")
view(quartile_kennzahlen_klassifikatoren)


mittelwert_vola_Gruppe <- kennzahlen %>%
  group_by(Klassifikator) %>% 
  summarize(Mittelwert_Vola = mean(Vola_Klassifikatoren_annualisiert, na.rm = TRUE)) %>%
  print()

quartile_vola_gruppe <- kennzahlen %>%
  group_by(Klassifikator) %>% 
  summarize(
    Q1 = quantile(Vola_Klassifikatoren_annualisiert, 0.25, na.rm = TRUE),
    Q2 = quantile(Vola_Klassifikatoren_annualisiert, 0.5, na.rm = TRUE),
    Q3 = quantile(Vola_Klassifikatoren_annualisiert, 0.75, na.rm = TRUE),
    Skewness = skewness(Vola_Klassifikatoren_annualisiert, na.rm = TRUE)
  ) %>%
  print()


#mittelwerte risk return
mittelwert_risk_return<- kennzahlen %>%
  group_by(Klassifikator) %>% 
  summarize(Risiko_Return_KL = mean(Risiko_Return_KL, na.rm = TRUE)) %>%
  print()









