# Statistischer Test
install.package("psych") # paket zum durch des tests
library(psych) 

#Wilcox Test kumulierterreturn -> jeweils die Klassifikatoren (Logistische Regression = "LR", Random Forest = "RF", Naive Bayes = "NB) eintragen zum Test. Beim risiko return wird greater getestet
test_kr <- read.xlsx(file.choose())

str(test_kr)
view(test_kr)

wilcox.test(test_kr$LR, test_kr$Dummy, data = test_kr, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_kr$NB, test_kr$Dummy, data = test_kr, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_kr$RF, test_kr$Dummy, data = test_kr, paired = TRUE, conf.int = TRUE,alternative = "greater")

#Wilcox Test maximaler drawdown -> testen von "less"
test_md <- read.xlsx(file.choose())

str(test_md )
view(test_md )

wilcox.test(test_md$LR, test_md$Dummy, data = test_md, paired = TRUE, conf.int = TRUE,alternative = "less")
wilcox.test(test_md$NB, test_md$Dummy, data = test_md, paired = TRUE, conf.int = TRUE,alternative = "less")
wilcox.test(test_md$RF, test_md$Dummy, data = test_md, paired = TRUE, conf.int = TRUE,alternative = "less")


#Wilcox Test return (annualisiert) -> jeweils die Klassifikatoren eintragen zum Test. Beim risiko return wird greater getestet
test_r <- read.xlsx(file.choose())

str(test_r)
view(test_r)

wilcox.test(test_r$LR, test_r$Dummy, data = test_r, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_r$NB, test_r$Dummy, data = test_r, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_r$RF, test_r$Dummy, data = test_r, paired = TRUE, conf.int = TRUE,alternative = "greater")


#Wilcox Test annualisierte Vola-> jeweils die Klassifikatoren eintragen zum Test. Bei der Vola wird "less" getestet
test_v <- read.xlsx(file.choose())

str(test_v )
view(test_v )

wilcox.test(test_v$LR, test_v$Dummy, data = test_v, paired = TRUE, conf.int = TRUE,alternative = "less")
wilcox.test(test_v$NB, test_v$Dummy, data = test_v, paired = TRUE, conf.int = TRUE,alternative = "less")
wilcox.test(test_v$RF, test_v$Dummy, data = test_v, paired = TRUE, conf.int = TRUE,alternative = "less")

# Risiko Return
test_rr <- read.xlsx(file.choose())

str(test_rr)
view(test_rr)

wilcox.test(test_rr$LR, test_rr$Dummy, data = test_rr, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_rr$NB, test_rr$Dummy, data = test_rr, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_rr$RF, test_rr$Dummy, data = test_rr, paired = TRUE, conf.int = TRUE,alternative = "greater")


##############################################
##### Tests für die Klassifikationsmetriken
##############################################

#1. Genauigkeit
test_g <- read.xlsx(file.choose())

str(test_g)
view(test_g)

wilcox.test(test_g$LR, test_g$Dummy, data = test_g, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_g$NB, test_g$Dummy, data = test_g, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_g$RF, test_g$Dummy, data = test_g, paired = TRUE, conf.int = TRUE,alternative = "greater")

# Fehlerrate
test_fr <- read.xlsx(file.choose())

str(test_fr)
view(test_fr)

wilcox.test(test_fr$LR, test_fr$Dummy, data = test_fr, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_fr$NB, test_fr$Dummy, data = test_fr, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_fr$RF, test_fr$Dummy, data = test_fr, paired = TRUE, conf.int = TRUE,alternative = "greater")


# Präzision
test_p <- read.xlsx(file.choose())

str(test_p)
view(test_p)

wilcox.test(test_p$LR, test_p$Dummy, data = test_p, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_p$NB, test_p$Dummy, data = test_p, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_p$RF, test_p$Dummy, data = test_p, paired = TRUE, conf.int = TRUE,alternative = "greater")

# Sensitivität
test_s <- read.xlsx(file.choose())

str(test_s)
view(test_s)

wilcox.test(test_s$LR, test_s$Dummy, data = test_s, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_s$NB, test_s$Dummy, data = test_s, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_s$RF, test_s$Dummy, data = test_s, paired = TRUE, conf.int = TRUE,alternative = "greater")

# Spezifität
test_sp <- read.xlsx(file.choose())

str(test_sp)
view(test_sp)

wilcox.test(test_sp$LR, test_sp$Dummy, data = test_sp, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_sp$NB, test_sp$Dummy, data = test_sp, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_sp$RF, test_sp$Dummy, data = test_sp, paired = TRUE, conf.int = TRUE,alternative = "greater")

#F1 Score
test_f1 <- read.xlsx(file.choose())

str(test_f1)
view(test_f1)

wilcox.test(test_f1$LR, test_f1$Dummy, data = test_f1, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_f1$NB, test_f1$Dummy, data = test_f1, paired = TRUE, conf.int = TRUE,alternative = "greater")
wilcox.test(test_f1$RF, test_f1$Dummy, data = test_f1, paired = TRUE, conf.int = TRUE,alternative = "greater")
