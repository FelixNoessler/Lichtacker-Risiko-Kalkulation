library('forecast')

leichterBoden <- function(ertrag){ #SDB für Durchschnitt der Böden, PIK auf ertragsärmeren Böden
  for (i in 1:length(ertrag)) {
      ertrag[i] = ertrag[i] *0.75
  }
  return(ertrag)
}

simuliere <- function(timeseries, startyear, Order, simulations, name, ylab, drift, damped){
  years = 50
  ts1 <- ts(timeseries, frequency = 1, start = startyear)
  
  if(drift == TRUE) {
    arima1 <- Arima(ts1, include.drift = TRUE, order = Order)
    
  } else{
    arima1 <- Arima(ts1, include.drift = F, order = Order)
  }
  
  f = forecast(arima1, h = years, level = c(0.75, 0.9))
 
  subt = paste(name, toString(f$method), sep = ": ")
  leg = paste("BIC:", round(f$model$bic, 2), "AIC:", round(f$model$aic, 2), sep = " ")

  plot(f, main = "",  sub = subt)
    legend("topleft", legend = leg, bty = "n")
      title(ylab = ylab, line= 2.2)
      title(xlab = "Jahr", line = 2)
  
  sim = matrix(data = 0, ncol = simulations, nrow = years)
  
  for (j in 1:simulations) {
       sim[,j]= simulate(arima1, nsim = years, future=T)
  }
  
  return(sim)
}


winterSim = function(timeseries, startyear, order,simulations, name, ylab, drift, damped){
  ts1 <- ts(timeseries, frequency = 1, start = startyear)
  years = 50
  
  if(drift == TRUE && damped == FALSE){ 
    hw <- ets(ts1, model = "AAN", damped = FALSE, opt.crit = "mse")
  }else if(damped == TRUE){
    hw <- ets(ts1, model = "AAN", damped = TRUE)
  }else{
    hw <- ets(ts1, model = "ANN")
  }
  
  f <- forecast(hw, h = years, level = c(0.75, 0.9))
  
  abc = paste(name, toString(f$method), sep = ": ")
  unter = paste("BIC:", round(f$model$bic, 2), "AIC:", round(f$model$aic, 2), sep = " ")
  plot(f, main = abc, xlab = "Jahr", ylab = ylab, sub = unter)
  
  sim = matrix(data = 0, ncol = simulations, nrow = years)
  
  for (j in 1:simulations) {
    sim[,j]= simulate(hw, nsim = years, future=T)
  }
  
  return(sim)
}


leistung = function(ertrag, preis){
  leistung = matrix(ncol = ncol(ertrag), nrow = nrow(ertrag))
  
  for (j in 1:ncol(ertrag)) {
    for (i in 1:nrow(ertrag)) {
        leistung[i,j] = ertrag[i,j] * preis[i,j]
    }
  }
  return(leistung)
}

deckungb = function(leistung, vc){
  db = matrix(ncol = ncol(vc), nrow = nrow(vc))
  
  for (j in 1:ncol(vc)) {
    for (i in 1:nrow(vc)) {
      db[i,j] = leistung[i,j] - vc[i,j]
    }
  }
  return(db)
}

anfangsWert = function(kosten, zins, jahre){
  anfang = vector(length = ncol(kosten))
  x = vector(length = nrow(kosten))
  
  for (j in 1:ncol(kosten)) {
    for (i in jahre:1) {
        x[i] = kosten[i,j] - ((zins[i,j]/100) * (kosten[i,j] + x[i+1])) + x[i+1]
        if(i == 1) anfang[j] = x[i]
    }
  }
  return(anfang)
}


minderung = function(ertrag, minderung){  
  for (j in 1:ncol(ertrag)) {
    for (i in 1:nrow(ertrag)) {
      ertrag[i,j] = ertrag[i,j] *(1-minderung)          
    }
  }
  return(ertrag)
}

differenz = function(db, dbpik){
  dbdif = matrix(ncol = ncol(db), nrow = nrow(db))
  
  for (j in 1:ncol(db)) {
    for (i in 1:nrow(db)) {
      dbdif[i,j] = db[i,j] - dbpik[i,j]            
    }
  }
  return(dbdif)
}

durchschnitt = function(db1,db2,db3){
  mittel = matrix(ncol = ncol(db1), nrow = nrow(db1))
  
  for (j in 1:ncol(db1)) {
    for (i in 1:nrow(db1)) {
      mittel[i,j] = (db1[i,j] + db2[i,j] + db3[i,j])/3           
    }
  }
  return(mittel)
}

flaeche = function(db, g){
  for (j in 1:ncol(db)) {
    for (i in 1:nrow(db)) {
      db[i,j] = db[i,j] *g           
    }
  }
  return(db)
}

risikoNatur = function(kostenMatrix, fixBetrag, zins){
  anzahlJahre = vector(length=ncol(kostenMatrix))
  
   for (j in 1:ncol(kostenMatrix)) {
     a = fixBetrag
     
    for (i in 1:nrow(kostenMatrix)) {
      a = a*(1+(0.01*zins[i,j])) - kostenMatrix[i,j]
      if (a > 0) {anzahlJahre[j] =  anzahlJahre[j]+1}
    }
  }
  return(anzahlJahre)
}

risikoLandwirt = function(kostenMatrix, fixBetrag, jahre, z){
  abweichung = vector(length = ncol(kostenMatrix))
  if(jahre > nrow(kostenMatrix)) {jahre = nrow(kostenMatrix)}
  
  for (j in 1:ncol(kostenMatrix)) {
    rest = fixBetrag
    
    for (i in 1:jahre) {
      if(rest>0){
        rest = rest*(1+(0.01*z[i,j])) - kostenMatrix[i,j]
      } else{
          rest = rest- kostenMatrix[i,j]
        }
    }
    abweichung[j] = rest
  }
  return(abweichung)
}

