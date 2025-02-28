# VIP Random Forest -> abrufen der Liste!
vip_gesamt <- do.call(rbind, wichtigkeit_liste)

# Durchschnitt  berechnen
durchschnitt_vip_rf <- vip_gesamt %>%
  group_by(Variable) %>%
  summarise(Durchschnitt_Wichtigkeit = mean(Importance)) %>% 
  arrange(desc(Durchschnitt_Wichtigkeit))

#Daten anschauen
view(vip_gesamt)

#als excel speichern
write.xlsx(vip_gesamt, "vip_rf.xlsx")



# VIP der logisitischen Regression -> gleiches Vorgehen wie beim random forest
vip_kombiniert <- do.call(rbind, vip_list)  

durchschnitt_vip_lr <- vip_kombiniert %>% # name andes damit keine Dublette entsteht! Daten sonst überschrieben
  group_by(Variable) %>%
  summarise(Durchschnitt_Wichtigkeit = mean(Importance))%>%
  arrange(desc(Durchschnitt_Wichtigkeit))

view(vip_kombiniert)
write.xlsx(vip_kombiniert, "vip_lr.xlsx")

# Variablen Importance -> Spalte Klassifikator hinzufügen -> so kann zwischen RF und LR sortiert/gefiltert werden werden
durchschnitt_vip_rf$Klassifikator <- "RF"
durchschnitt_vip_lr$Klassifikator <- "LR"

# Datensätze fusionieren. 
vip_alle <- bind_rows(durchschnitt_vip_rf, durchschnitt_vip_lr)
view(vip_alle)

# Datensatz speichern
write.xlsx(vip_alle, "vip_alle.xlsx")




