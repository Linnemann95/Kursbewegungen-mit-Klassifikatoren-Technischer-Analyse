# Benötigte Pakete laden
library(tidyverse)
library(quantmod)
library(cowplot)
# Schritt 1: Aktienkurse abrufen
getSymbols("AAPL", src = "yahoo", from = "2020-01-01", to = "2021-12-31")

# Schritt 2: Daten in einen DataFrame umwandeln und Spaltennamen anpassen
stock_data <- AAPL %>%
  data.frame(Date = index(.)) %>%
  rename(Open = AAPL.Open,
         High = AAPL.High,
         Low = AAPL.Low,
         Close = AAPL.Close,
         Volume = AAPL.Volume,
         Adjusted = AAPL.Adjusted)

# Schritt 3: Daten vorbereiten und bereinigen
stock_data <- stock_data %>%
  arrange(Date)

# Schritt 4: Renditen berechnen
stock_data <- stock_data %>%
  mutate(Next_Day_Return = (lead(Close) - Close) / Close)

# Schritt 5: Kategoriale Variable erstellen
stock_data <- stock_data %>%
  mutate(Next_Day_Up = if_else(Next_Day_Return > 0, 1, 0))

# Schritt 6: Ergebnis prüfen und speichern
print(head(stock_data))

# Daten speichern
write_csv(stock_data, "stock_data_with_returns.csv")

ggplot(stock_data, aes(x = Date, y = Next_Day_Return)) +
  geom_line(col = "darkred") +
  labs(x = "Datum", y = "Tägliche Rendite") +
  theme_cowplot(12)

mean(stock_data$Next_Day_Return)
     