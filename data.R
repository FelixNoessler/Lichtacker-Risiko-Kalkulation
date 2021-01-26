source('function.R')

# Einlesen der Daten ---------------------------------
spar <- read.csv2("Sparbuchzinsen_1975_2017.csv", header=FALSE)
sdb <- read.csv2("Standarddeckungsbeitraege.csv")

#auf niedrig leichte böden *0.75
wertrag <- leichterBoden(sdb$Ertrag.dt.ha[1:18]) 
gertrag <- leichterBoden(sdb$Ertrag.dt.ha[19:36])
rertrag <- leichterBoden(sdb$Ertrag.dt.ha[37:54])
hertrag <- leichterBoden(sdb$Ertrag.dt.ha[55:72])

wvc <- sdb$Variable.Kosten...ha[1:18]
gvc <- sdb$Variable.Kosten...ha[19:36]
rvc <- sdb$Variable.Kosten...ha[37:54]
hvc <- sdb$Variable.Kosten...ha[55:72]

wp <- sdb$Preis...dt[1:18]
gp <- sdb$Preis...dt[19:36]
rp <- sdb$Preis...dt[37:54]
hp <- sdb$Preis...dt[55:72]


#Minderung der varibalen Kosten bei Lichtackerbewirtschaftung
weizenvcm = 0.529285238737432
gerstevcm = 0.449514970618413
hafervcm = 0.366274892956694

# Setzen der Variablen--------------------------------- 
ertragsm = 0.6        #Minderung Ertrag bei Lichtackerbewirtschaftung
simulations = 25000     #Zahl an Simulation
startyear = 2000      #Jahr, in dem die Zeitreihe anfängt
jahre = 20            #Anzahl an Simulatinsjahren
ha = 10               #Anzahl Hektar
order = c(0,0,1)      #Arima-Modell für alle Variablen
factor = 1.3
factorNatur = factor
factorLand = factor

# Berechnen der Simulation--------------------------------- 
par(mfrow=c(1,1), mar=c(5,3,1,2)+0.1)
z = simuliere(spar, 1990, order, simulations, "Zinsentwicklung", "Zins %", F, F)

par(mfrow=c(1,3))       #Einstellen des Fensters für plot
y1 = "Marktpreis €/dt"  #ylab für simuliere()
y2 = "Ertrag dt/ha"
y3 = "Variable Kosten €/ha"

#weizen
weizenpreis = simuliere(wp, startyear, c(0,1,1), simulations, "P Weizen", y1, T, F)
weizenertrag = simuliere(wertrag, startyear, c(0,0,1), simulations, "Q Weizen", y2, T, F)
weizenvc = simuliere(wvc, startyear, c(0,1,1), simulations, "VC Weizen", y3, T, F)

weizenleistung = leistung(weizenertrag, weizenpreis)
weizendb = deckungb(weizenleistung, weizenvc)

#weizen pik
wem = minderung(weizenertrag, ertragsm)
weizenleistungm = leistung(wem, weizenpreis)
weizendbm = deckungb(weizenleistungm, minderung(weizenvc, weizenvcm))

#gerste
gerstep = simuliere(gp, startyear, c(0,1,1), simulations, "P Gerste", y1, T, F)
gerstee = simuliere(gertrag, startyear, c(0,0,1), simulations, "Q Gerste", y2, T, F)
gerstevc = simuliere(gvc, startyear, c(0,1,1), simulations, "VC Gerste", y3, T, T)

gersteleistung = leistung(gerstee, gerstep)
gerstedb = deckungb(gersteleistung, gerstevc)

#gerste pik
gem = minderung(gerstee, ertragsm)
gersteleistungm = leistung(gem, gerstep)
gerstedbm = deckungb(gersteleistungm, minderung(gerstevc, gerstevcm))

#raps
rapsp = simuliere(rp, startyear, c(0,1,1), simulations, "P Raps", y1, T, F)
rapse = simuliere(rertrag, startyear, c(0,1,1), simulations, "Q Raps", y2, F, F)
rapsvc = simuliere(rvc, startyear, c(0,1,1), simulations, "VC Raps", y3, T, F)

rapsleistung = leistung(rapse, rapsp)
rapsdb = deckungb(rapsleistung, rapsvc)

#hafer
haferp = simuliere(hp, startyear, c(0,1,1), simulations, "P Hafer", y1, T, F)
hafere = simuliere(hertrag, startyear, order, simulations, "Q Hafer", y2, T, F)
hafervc = simuliere(hvc, startyear, c(0,1,1), simulations, "VC Hafer", y3, T, F)

haferleistung = leistung(hafere, haferp)
haferdb = deckungb(haferleistung, hafervc)

#hafer pik
hem = minderung(hafere, ertragsm)

haferleistungm = leistung(hem, haferp)
haferdbm = deckungb(haferleistungm, minderung(hafervc, hafervcm))


# Berechnen der einzelnen Szenarien--------------------------------- 
normal = durchschnitt(weizendb, gerstedb, rapsdb)
pik = durchschnitt(weizendbm, gerstedbm, haferdbm)
kosten = differenz(normal, pik)
kostenGesamteFläche = flaeche(kosten, ha)

# Berechnen der 3 Modelle
eingriffRisiko = anfangsWert(kostenGesamteFläche, z, jahre)

#fixPreis = berechneFixPreis(kostenGesamteFläche, jahre) # ohne Zinsen
fixPreis = median(eingriffRisiko)
fixPreisN = fixPreis * factorNatur      
fixPreisL = fixPreis * factorLand   

naturRisiko = risikoNatur(kostenGesamteFläche, fixPreisN, z)
landRisiko = risikoLandwirt(kostenGesamteFläche, fixPreisL, jahre, z)

# Diagramme--------------------------------- 
par(mfrow=c(1,1), mar = c(4, 4, 4, 2) + 0.1, family = "serif") # Zürücksetzen der Anzahl plots pro Bild

#hist risiko eingriff mit %
svg('plots/risiko_verursacher.svg', width=12)
qE = quantile(eingriffRisiko, probs = c(0.05, 0.5, 0.95))
histEingriff = hist(eingriffRisiko, plot=F, breaks=35)
histEingriff$density = histEingriff$counts/sum(histEingriff$counts)*100
plot(histEingriff, freq = F, xaxt="n",
     main = "Risikoträger Eingriffsverursacher: Kosten der Lichtackerbewirtschaftung", ylim = c(0,12), 
     xlab = "Kosten in €", ylab = "Dichte %", col = "lightgrey")
    print(paste("Risiko Eingriff: Vergütung des Landwirtes nach progn. Marktlage,", simulations, "Simulationen"))
    
    abline(v=qE[2], lwd=3, lty=3, col="blue")
    abline(v=qE[1], lwd=3, lty=2, col="red")
    abline(v=qE[3], lwd=3, lty=2, col="red")
    abline(v=max(eingriffRisiko), lwd=1, lty=1, col="azure4")
    abline(v=min(eingriffRisiko), lwd=1, lty=1, col="azure4")
    
    legend("topright", legend = c(expression(tilde(x)), expression(paste("Grenzen des ",tilde(x)[0.9])), "min/max"),
           col=c("blue", "red", "grey"), lty=c(3,2,1), 
           bg = "white", box.col = "white")
    
    text(max(eingriffRisiko)-3000, 3, labels = paste(round(max(eingriffRisiko)), "€"), srt = 90)
    text(min(eingriffRisiko)-3000, 3, labels = paste(round(min(eingriffRisiko)), "€"), srt = 90)
    text(qE[1]-3000, 6, labels = paste(round(qE[1]), "€"), srt = 90, col = "red")
    text(qE[3]-3000, 6, labels = paste(round(qE[3]), "€"), srt = 90, col = "red")
    text(qE[2]-3000, 11, labels = paste(round(qE[2]), "€"), srt = 90, col = "blue")
    axis(1, at = seq(0, 160000, by = 20000))
dev.off()

#hist risiko natur mit % 
svg('plots/risiko_naturschutz.svg', width=12)
qN = quantile(naturRisiko, probs = c(0.05, 0.5, 0.95))
histNatur = hist(naturRisiko, plot=F, breaks=35)
histNatur$density = histNatur$counts/sum(histNatur$counts)*100
plot(histNatur, freq = F, main = "Risikoträger Naturschutz: Bewirtschaftungsjahre", 
     xlab = "Jahre", ylab = "Dichte %", col = "lightgrey", xaxt="n")
print(paste("Risiko natur: fixe Zahlung:",round(fixPreisN),"€ (Faktor:", factorNatur, "), Vergütung nach progn. Marktlage,", 
            simulations, "Simulationen"))
    abline(v=qN[2], lwd=3, lty=3, col="blue")
    abline(v=qN[1], lwd=3, lty=2, col="red")
    abline(v=qN[3], lwd=3, lty=2, col="red")
    abline(v=max(naturRisiko), lwd=1, lty=1, col="azure4")
    abline(v=min(naturRisiko), lwd=1, lty=1, col="azure4")
    legend("topright", legend = c(expression(tilde(x)), expression(paste("Grenzen des ",tilde(x)[0.9])), "min/max"),
           col=c("blue", "red", "grey"), lty=c(3,2,1), 
           bg = "white", box.col = "white")
    axis(1, at = seq(12, 50, by = 2))
    
    #text(max(naturRisiko)-1, 2, labels = paste(round(max(naturRisiko)), "J."), srt = 90)
    #text(min(naturRisiko)-1, 2, labels = paste(round(min(naturRisiko)), "J."), srt = 90)
    #text(qN[1]-1, 5, labels = paste(round(qN[1]), "J."), srt = 90, col = "red")
    #text(qN[3]-1, 5, labels = paste(round(qN[3]), "J."), srt = 90, col = "red")
    #text(qN[2]-1, 10, labels = paste(round(qN[2]), "J."), srt = 90, col = "blue")

dev.off()

#hist landwirt
svg('plots/risiko_landwirt.svg', width=12)
par(family = "serif")
qL = quantile(landRisiko, probs = c(0.05, 0.5, 0.95))
histLand = hist(landRisiko, plot=F, breaks = 50)
histLand$density = histLand$counts/sum(histLand$counts)*100
plot(histLand,freq=FALSE, col= "grey", xlab = "Betrag €", ylim = c(0,10), xaxt="n",
     ylab = "Dichte %", main ="Risikoträger Landwirt, Abweichung von Vergütung") 
    print(paste("Risiko Landwirt:",round(fixPreisL), "€ Vergütung - Faktor:", factorLand, ", Zeitraum:", jahre, "Jahre,", 
                simulations, "Simulationen"))
    abline(v=qL[2], lwd=3, lty=3, col="blue")
    abline(v=qL[1], lwd=3, lty=2, col="red")
    abline(v=qL[3], lwd=3, lty=2, col="red")
    abline(v=max(landRisiko), lwd=1, lty=1, col="azure4")
    abline(v=min(landRisiko), lwd=1, lty=1, col="azure4")
    legend("topright", legend = c(expression(tilde(x)), expression(paste("Grenzen des ",tilde(x)[0.9])), "min/max"),
      col=c("blue", "red", "grey"), lty=c(3,2,1), 
      bg = "white", box.col = "white")
    text(max(landRisiko)-4000, 1.5, labels = paste(round(max(landRisiko)), "€"), srt = 90)
    text(min(landRisiko)-4000, 1.5, labels = paste(round(min(landRisiko)), "€"), srt = 90)
    text(qL[1]-4000, 5, labels = paste(round(qL[1]), "€"), srt = 90, col = "red")
    text(qL[3]-4000, 5, labels = paste(round(qL[3]), "€"), srt = 90, col = "red")
    text(qL[2]-4000, 9, labels = paste(round(qL[2]), "€"), srt = 90, col = "blue")
    axis(1, at = seq(-100000, max(landRisiko), by = 10000))

dev.off()

