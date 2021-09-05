# Skrypt ten ma za zadanie w sposób ca³oœciowy analizowaæ dane empiryczne w postaci próby pobranej z populacji.

pdf(file = "wykresy.pdf")# eksport wykresów do pliku

cat('\nDane kszta³tuj¹ siê nastêpuj¹co:')
# # wczytywanie danych z pliku csv (plik w postaci jednej kolumny z naglowkiem)
# file_name <- 'dane.csv'
# plik <- read.csv(file = file_name, sep = ';', dec = ',')
# dane <- rep(0, length(plik[,1]))
# for (i in 1:length(plik[,1])) {
#   dane[i] <- as.integer(plik[i,1])
# }
# (dane)

# wczytanie danych
(dane <- c(2, 3, 5, 6, 10, 6, 10, 7, 10, 4, 9, 10, 11, 11, 12, 13, 15, 18, 17, 10, 2, 4, 6, 7, 7, 8, 8, 9, 18, 9, 10, 12, 14, 9, 14, 19, 16, 20, 8, 11))

cat('\n\nSZEREG SZCZEGÓ£OWY (wyliczaj¹cy)\n')

# dane posortowane
dane <- sort(dane)
cat('\nPosortowane dane:', dane)

# opis danych
N <- length(dane)
cat('\nZbiór danych zawiera', N, 'elementów.')

cat('\n\nMIARY PO£O¯ENIA')

cat('\nWartoœæ minimalna:', min(dane))
cat('\nWartoœæ maksymalna:', max(dane))
cat('\nMediana:', median(dane))

# œrednia arytmetyczna
srednia <- mean(dane)
cat('\nŒrednia arytmetyczna wynosi:', mean(dane))

# œrednia wa¿ona (wagi dla ka¿dego elementu s¹ takie same i wynosz¹ 1)
cat('\nŒrednia wa¿ona wynosi:', weighted.mean(dane, rep(1,N)))

# œrednia harmoniczna
cat('\nŒrednia harmoniczna wynosi:', N / (sum(1 / dane)))

# œrednia geometryczna
cat('\nŒrednia geometryczna wynosi:', (prod(dane)) ^ (1 / N))

# kwantyle
Q1 <- quantile(dane, 0.25)
Q2 <- quantile(dane, 0.50)
Q3 <- quantile(dane, 0.75)
cat('\nWartoœæ kwantyla 5% wynosi:', quantile(dane, 0.05))
cat('\nWartoœæ kwantyla 25% wynosi:', Q1)
cat('\nWartoœæ kwantyla 50% wynosi:', Q2)
cat('\nWartoœæ kwantyla 75% wynosi:', Q3)
cat('\nWartoœæ kwantyla 95% wynosi:', quantile(dane, 0.95))

# posortowane unikalne wartoœæi
unikalneWartosci <- sort(unique(dane))

# iloœæ wyst¹pieñ poszczególnych wartoœci
liczebnoscNi <- rep(0, length(unikalneWartosci))
for (i in 1:length(unikalneWartosci)) {
  liczebnoscNi[i] = length(which(dane == unikalneWartosci[i]))
} 
dominanta <- unikalneWartosci[which(max(liczebnoscNi) == liczebnoscNi)]
cat('\nDominanta:', dominanta)

asymetria1 <- 0
asymetria2 <- 0
if (srednia < Q2 & Q2 < dominanta){
  cat('\nNa podstawie porównañ wartoœci œredniej, mediany i dominanty stwierdzamy, ¿e œrednia < mediana < dominanta, co wskazuje na asymetriê lewostronn¹ rozkladu.')
  asymetria1 <- -1
}
if (srednia > Q2 & Q2 > dominanta){
  cat('\nNa podstawie porównañ wartoœci œredniej, mediany i dominanty stwierdzamy, ¿e œrednia > mediana > dominanta, co wskazuje na asymetriê prawostronn¹ rozkladu.')
  asymetria1 <- 1
}

cat('\n\nMIARY ZMIENNOŒCI (ZRÓ¯NICOWANIA, ROZPROSZENIA, DYSPRESJI)')

cat('\nPOZYCYJNE')
cat('\nEmpiryczny obszar zmiennoœci:', max(dane) - min(dane))
cat('\nRozstêp miêdzykwartylowy:', Q3 - Q1)
Q <- (Q3 - Q1) / 2
cat('\nOdchylenie æwiartkowe:', Q)
cat('\nPozycyjny typowy obszar zmiennoœci:', Q2 - Q, '-', Q2 + Q)
cat('\nPozycyjny wspó³czynnik zmiennoœci:', (Q / Q2) * 100)
cat('\nKLASYCZNE')
wariancja <- sum((dane - srednia) ^ 2) / N
cat('\nWariancja:', wariancja)
s <- sqrt(wariancja)
cat('\nOdchylenie standardowe:', s)
cat('\nTypowy klasyczny obszar zmiennoœci:', srednia - s, '-', srednia + s)
cat('\nKlasyczny wspó³czynnik zmiennoœci:', (s / srednia) * 100)

cat('\n\nMIARY ASYMETRII')

cat('\nWspó³czynnik Pearsona:', (srednia - dominanta) / s)
cat('\nWspó³czynnik pozycyjny:', (Q3 + Q1 - 2 * Q2) /  (2 * Q))
m3 <- sum((dane - srednia) ^ 3) / N
cat('\n3-ci moment centralny:', m3)
WA <- m3 / (s ^ 3)
cat('\nWspó³czynnik asymetrii:', WA)
if (WA>0){
  cat('\nNa podstawie wspólczynnika asymetrii mo¿na stwierdziæ, ¿e wystêpuje asymetria prawostronna.')
  asymetria2 <- 1
}
if (WA<0){
  cat('\nNa podstawie wspólczynnika asymetrii mo¿na stwierdziæ, ¿e wystêpuje asymetria lewostronna.')
  asymetria2 <- -1
}
if (WA==0){
  cat('\nNa podstawie wspólczynnika asymetrii mo¿na stwierdziæ, ¿e rozklad jest symetryczny.')
  asymetria2 <- 0
}
absWA <- abs(WA)
if (absWA>0 & absWA<=0.2){
  cat('\nAsymetriê mo¿na okreœliæ jako bardzo slab¹.')
}
if (absWA>0.2 & absWA<=0.4){
  cat('\nAsymetriê mo¿na okreœliæ jako slab¹.')
}
if (absWA>0.4 & absWA<=0.6){
  cat('\nAsymetriê mo¿na okreœliæ jako umiarkowan¹.')
}
if (absWA>0.6 & absWA<=0.8){
  cat('\nAsymetriê mo¿na okreœliæ jako siln¹.')
}
if (absWA>0.8 & absWA<=1){
  cat('\nAsymetriê mo¿na okreœliæ jako bardzo siln¹.')
}
if (asymetria1 != asymetria2 & asymetria1 != 0){
  cat('\nRó¿nica we wskazaniach 2 mierników asymetrii mo¿e wynikaæ z niewielkiej iloœci danych lub te¿ ze zbli¿onych wartoœci œredniej, mediany i dominanty.')
}

cat('\n\nMIARY KURTOZY')

m4 <- sum((dane - srednia) ^ 4) / N
cat('\n4-ty moment centralny:', m4)
kurtoza <- m4 / (s ^ 4)
cat('\nKurtoza:', kurtoza)
cat('\nWspó³czynnik ekscesu:', kurtoza - 3)
cat('\nWartoœci kurtozy oraz wspó³czynnkika ekscesu wstazuj¹ na to, ¿e rozk³ad jest ')
if (kurtoza > 3){
  cat('wysmuk³y.')
}
if (kurtoza == 3){
  cat('normalny.')
}
if (kurtoza < 3 & kurtoza > -3){
  cat('umiarkowanie sp³aszczony.')
}
if (kurtoza < -3){
  cat('sp³aszczony.')
}

cat('\n\n\nSZEREG ROZDZIELCZY PUNKTOWY\n\n')

# iloœæ wyst¹pieñ wartoœci w stosunku do ca³kowitej liczby danych
czestosciWzgledne <- liczebnoscNi / length(dane)

liczebnosciSkumulowane <- cumsum(liczebnoscNi)
czestosciWzgledneSkumulowane <- cumsum(czestosciWzgledne)

iloczynXiNi <- unikalneWartosci * liczebnoscNi
iloczynXiWi <- unikalneWartosci * czestosciWzgledne

czestosciWzgledneLaczne <- iloczynXiNi / sum(iloczynXiNi)
czestosciWzgledneLaczneSkumulowane <- cumsum(czestosciWzgledneLaczne)

data.frame(unikalneWartosci, liczebnoscNi)

cat('\nMIARY PO£O¯ENIA')

# œrednia arytmetyczna
srednia <- sum(iloczynXiNi) / N
cat('\nŒrednia arytmetyczna wynosi:', srednia)

# œrednia wa¿ona (wagi dla ka¿dego elementu s¹ takie same i wynosz¹ 1)
cat('\nŒrednia wa¿ona wynosi:', weighted.mean(unikalneWartosci, czestosciWzgledne))

# œrednia harmoniczna
cat('\nŒrednia harmoniczna wynosi:', N / (sum(liczebnoscNi / unikalneWartosci)))

# œrednia geometryczna
cat('\nŒrednia geometryczna wynosi:', (prod(unikalneWartosci^liczebnoscNi))^(1 / N))

# dominanta
dominanta <- unikalneWartosci[which(max(liczebnoscNi)==liczebnoscNi)]
cat('\nDominanta to:', dominanta)

# kwantyle
cat('\nWartoœæ kwantyla 5% wynosi:', quantile(dane, 0.05))
cat('\nWartoœæ kwantyla 25% wynosi:', quantile(dane, 0.25))
cat('\nWartoœæ kwantyla 50% wynosi:', quantile(dane, 0.50))
cat('\nWartoœæ kwantyla 75% wynosi:', quantile(dane, 0.75))
cat('\nWartoœæ kwantyla 95% wynosi:', quantile(dane, 0.95))

asymetria1 <- 0
asymetria2 <- 0
if (srednia < Q2 & Q2 < dominanta){
  cat('\nNa podstawie porównañ wartoœci œredniej, mediany i dominanty stwierdzamy, ¿e œrednia < mediana < dominanta, co wskazuje na asymetriê lewostronn¹ rozkladu.')
  asymetria1 <- -1
}
if (srednia > Q2 & Q2 > dominanta){
  cat('\nNa podstawie porównañ wartoœci œredniej, mediany i dominanty stwierdzamy, ¿e œrednia > mediana > dominanta, co wskazuje na asymetriê prawostronn¹ rozkladu.')
  asymetria1 <- 1
}

cat('\n\nMIARY ZMIENNOŒCI (ZRÓ¯NICOWANIA, ROZPROSZENIA, DYSPRESJI)')

cat('\nPOZYCYJNE')
cat('\nEmpiryczny obszar zmiennosci:', max(unikalneWartosci) - min(unikalneWartosci))
cat('\nRozstêp miêdzykwartylowy:', Q3 - Q1)
Q <- (Q3 - Q1) / 2
cat('\nOdchylenie æwiartkowe:', Q)
cat('\nPozycyjny typowy obszar zmiennoœci:', Q2 - Q, '-', Q2 + Q)
cat('\nPozycyjny wspó³czynnik zmiennoœci:', (Q / Q2) * 100)
cat('\nKLASYCZNE')
wariancja <- sum(((unikalneWartosci - sum(iloczynXiWi)) ^ 2) * liczebnoscNi) / N
cat('\nWariancja:', wariancja)
s <- sqrt(wariancja)
cat('\nOdchylenie standardowe:', s)
cat('\nTypowy klasyczny obszar zmiennoœci:', srednia - s, '-', srednia + s)
cat('\nKlasyczny wspó³czynnik zmiennoœci:', (s / srednia) * 100)

cat('\n\nMIARY ASYMETRII')

cat('\nWspó³czynnik Pearsona:', (srednia - dominanta) / s)
cat('\nWspó³czynnik pozycyjny:', (Q3 + Q1 - 2 * Q2) /  (2 * Q))
m3 <- sum(((unikalneWartosci - sum(iloczynXiWi)) ^ 3) * liczebnoscNi) / N
cat('\n3-ci moment centralny:', m3)
WA <- m3 / (s ^ 3)
cat('\nWspó³czynnik asymetrii:', WA)
if (WA>0){
  cat('\nNa podstawie wspólczynnika asymetrii mo¿na stwierdziæ, ¿e wystêpuje asymetria prawostronna.')
  asymetria2 <- 1
}
if (WA<0){
  cat('\nNa podstawie wspólczynnika asymetrii mo¿na stwierdziæ, ¿e wystêpuje asymetria lewostronna.')
  asymetria2 <- -1
}
if (WA==0){
  cat('\nNa podstawie wspólczynnika asymetrii mo¿na stwierdziæ, ¿e rozklad jest symetryczny.')
  asymetria2 <- 0
}
absWA <- abs(WA)
if (absWA>0 & absWA<=0.2){
  cat('\nAsymetriê mo¿na okreœliæ jako bardzo slab¹.')
}
if (absWA>0.2 & absWA<=0.4){
  cat('\nAsymetriê mo¿na okreœliæ jako slab¹.')
}
if (absWA>0.4 & absWA<=0.6){
  cat('\nAsymetriê mo¿na okreœliæ jako umiarkowan¹.')
}
if (absWA>0.6 & absWA<=0.8){
  cat('\nAsymetriê mo¿na okreœliæ jako siln¹.')
}
if (absWA>0.8 & absWA<=1){
  cat('\nAsymetriê mo¿na okreœliæ jako bardzo siln¹.')
}
if (asymetria1 != asymetria2 & asymetria1 != 0){
  cat('\nRó¿nica we wskazaniach 2 mierników asymetrii mo¿e wynikaæ z niewielkiej iloœci danych lub te¿ ze zbli¿onych wartoœci œredniej, mediany i dominanty.')
}

cat('\n\nMIARY KURTOZY')

m4 <- sum(((unikalneWartosci - sum(iloczynXiWi)) ^ 4) * liczebnoscNi) / N
cat('\n4-ty moment centralny:', m4)
kurtoza <- m4 / (s ^ 4)
cat('\nKurtoza:', kurtoza)
cat('\nWspó³czynnik ekscesu:', kurtoza - 3)
cat('\nWartoœci kurtozy oraz wspó³czynnkika ekscesu wstazuj¹ na to, ¿e rozk³ad jest ')
if (kurtoza > 3){
  cat('wysmuk³y.')
}
if (kurtoza == 3){
  cat('normalny.')
}
if (kurtoza < 3 & kurtoza > -3){
  cat('umiarkowanie sp³aszczony.')
}
if (kurtoza < -3){
  cat('sp³aszczony.')
}

# wykresy
barplot(liczebnoscNi, 
        names.arg = unikalneWartosci, 
        main = "Histogram liczebnoœci unikalnych wartoœci")

barplot(liczebnosciSkumulowane, 
        names.arg = unikalneWartosci, 
        main = "Skumulowany histogram liczebnoœci unikalnych wartoœci")

plot(y = czestosciWzgledneSkumulowane, 
     x = unikalneWartosci, 
     xlab = '',
     ylab = 'Czêstoœci skumulowane',
     type = "b",
     main = "Dystrybuanta empiryczna dla szeregu punktowego",
     col="blue")

plot(y = czestosciWzgledneLaczneSkumulowane * 100, 
     x = czestosciWzgledneSkumulowane * 100, 
     xlim = c(0, 100),
     ylim = c(0, 100),
     xlab = '',
     ylab = '',
     type = "b",
     main = "Krzywa Lorenz'a dla szeregu punktowego",
     col="red")
lines(y = c(1:100), 
      x = c(1:100), 
      type = "l",
      col="blue")

cat('\n\n\nSZEREG ROZDZIELCZY PRZEDZIA£OWY\n')

# wyznaczenie interwa³u oraz iloœci przedzia³ów
k1 <- sqrt(N)
k2 <- 0.75 * k1
cat('\nIloœæ przedzia³ów k powinna byæ liczb¹ ca³kowit¹ oraz zawieraæ siê w przedziale od', k2, 'do', k1)
k <- floor((k1 + k2) / 2)
cat('\nW naszym przypadku bêdzie to', k, 'przedzia³ów.')
interwal <- ceiling((max(dane) - min(dane)) / k)
cat('\nInterwa³ wyra¿ony wzorem (Xmax - Xmin)/k wynosi po zaokr¹gleniu', interwal, '\n')

dolnaGranicaPrzedzialu <- seq(median(dane) - (k / 2) * interwal, median(dane) + (k / 2) * interwal - interwal, by = interwal)
gornaGranicaPrzedzialu <- seq(median(dane) - (k / 2) * interwal + interwal, median(dane) + (k / 2) * interwal, by = interwal)

przedzialy <- rep(0, k)
for (i in 1:length(przedzialy)) {
  przedzialy[i] <- paste('(', toString(dolnaGranicaPrzedzialu[i]), '-', toString(gornaGranicaPrzedzialu[i]), ']')
}

liczebnoscNiNaPrzedzialach <- rep(0, length(dolnaGranicaPrzedzialu))
for (i in 1:length(dolnaGranicaPrzedzialu)) {
  liczebnoscNiNaPrzedzialach[i] <- length(which(dane > dolnaGranicaPrzedzialu[i] & dane <= gornaGranicaPrzedzialu[i]))
}
czestosciWzgledneNaPrzedzialach <- liczebnoscNiNaPrzedzialach / length(dane)

liczebnoscNiNaPrzedzialachSkumulowane <- cumsum(liczebnoscNiNaPrzedzialach)
czestosciWzgledneNaPrzedzialachSkumulowane <- cumsum(czestosciWzgledneNaPrzedzialach)

srodekPrzedzilu <- (dolnaGranicaPrzedzialu + gornaGranicaPrzedzialu) / 2

iloczynSrodekPrzedzialuNi <- srodekPrzedzilu * liczebnoscNiNaPrzedzialach
iloczynSrodekPrzedzialuWi <- srodekPrzedzilu * czestosciWzgledneNaPrzedzialach

czestosciWzglesneLaczneNaPrzedzialach <- iloczynSrodekPrzedzialuNi / sum(iloczynSrodekPrzedzialuNi)
czestosciWzglesneLaczneNaPrzedzialachSkumulowane <- cumsum(czestosciWzglesneLaczneNaPrzedzialach)

data.frame(przedzialy, liczebnoscNiNaPrzedzialach)

cat('\nMIARY PO£O¯ENIA')

# œrednia arytmetyczna
srednia <- sum(iloczynSrodekPrzedzialuWi)
cat('\nŒrednia arytmetyczna wynosi:', srednia)

# œrednia wa¿ona (wagi dla ka¿dego elementu s¹ takie same i wynosz¹ 1)
cat('\nŒrednia wa¿ona wynosi:', weighted.mean(srodekPrzedzilu, czestosciWzgledneNaPrzedzialach))

# œrednia harmoniczna
cat('\nŒrednia harmoniczna wynosi:', N / (sum(liczebnoscNiNaPrzedzialach[1:length(liczebnoscNiNaPrzedzialach)] / srodekPrzedzilu[1:length(liczebnoscNiNaPrzedzialach)])))

# œrednia geometryczna
cat('\nŒrednia geometryczna wynosi:', (prod(srodekPrzedzilu^liczebnoscNiNaPrzedzialach))^(1 / N))

# dominanta
xD <- dolnaGranicaPrzedzialu[which(max(liczebnoscNiNaPrzedzialach)==liczebnoscNiNaPrzedzialach)]
nD <- max(liczebnoscNiNaPrzedzialach)
nDminus1 <- liczebnoscNiNaPrzedzialach[which(max(liczebnoscNiNaPrzedzialach)==liczebnoscNiNaPrzedzialach) - 1]
nDplus1 <- liczebnoscNiNaPrzedzialach[which(max(liczebnoscNiNaPrzedzialach)==liczebnoscNiNaPrzedzialach) + 1]
iD <- interwal
dominanta <- xD + ((nD-nDminus1)/((nD-nDminus1)+(nD-nDplus1)))*iD
cat('\nDominanta to:', dominanta, '\n')

# kwantyle
kwantyle <- c(0.05, 0.25, 0.5, 0.75, 0.95)
for (i in 1:length(kwantyle)) {
  miejscewN <- N * kwantyle[i]
  przedzialKwantyla <- min(which(miejscewN <= liczebnoscNiNaPrzedzialachSkumulowane))
  xQ <- dolnaGranicaPrzedzialu[przedzialKwantyla]
  nQ <- liczebnoscNiNaPrzedzialach[przedzialKwantyla]
  Q <- xQ + (((miejscewN - (liczebnoscNiNaPrzedzialachSkumulowane[przedzialKwantyla] - liczebnoscNiNaPrzedzialach[przedzialKwantyla])) / nQ) * interwal)
  if(kwantyle[i] == 0.25){
    Q1 <- Q
  }
  if(kwantyle[i] == 0.5){
    Q2 <- Q
  }
  if(kwantyle[i] == 0.75){
    Q3 <- Q
  }
  cat('Wartoœæ kwantyle', kwantyle[i], 'wynosi', Q, '\n')
}

asymetria1 <- 0
asymetria2 <- 0
if (srednia < Q2 & Q2 < dominanta){
  cat('\nNa podstawie porównañ wartoœci œredniej, mediany i dominanty stwierdzamy, ¿e œrednia < mediana < dominanta, co wskazuje na asymetriê lewostronn¹ rozkladu.')
  asymetria1 <- -1
}
if (srednia > Q2 & Q2 > dominanta){
  cat('\nNa podstawie porównañ wartoœci œredniej, mediany i dominanty stwierdzamy, ¿e œrednia > mediana > dominanta, co wskazuje na asymetriê prawostronn¹ rozkladu.')
  asymetria1 <- 1
}

cat('\nMIARY ZMIENNOŒCI (ZRÓ¯NICOWANIA, ROZPROSZENIA, DYSPRESJI)')

cat('\nPOZYCYJNE')
cat('\nEmpiryczny obszar zmiennoœci:', max(gornaGranicaPrzedzialu) - min(dolnaGranicaPrzedzialu))
cat('\nRozstêp miêdzykwartylowy:', Q3 - Q1)
Q <- (Q3 - Q1) / 2
cat('\nOdchylenie æwiartkowe:', Q)
cat('\nPozycyjny typowy obszar zmiennoœci:', Q2 - Q, '-', Q2 + Q)
cat('\nPozycyjny wspó³czynnik zmiennoœci:', (Q / Q2) * 100)
cat('\nKLASYCZNE')
wariancja <- sum(((srodekPrzedzilu - sum(iloczynSrodekPrzedzialuWi)) ^ 2) * liczebnoscNiNaPrzedzialach) / N
cat('\nWariancja:', wariancja)
s <- sqrt(wariancja)
cat('\nOdchylenie standardowe:', s)
cat('\nTypowy klasyczny obszar zmiennoœci:', srednia - s, '-', srednia + s)
cat('\nKlasyczny wspó³czynnik zmiennoœci:', (s / srednia) * 100)

cat('\n\nMIARY ASYMETRII')

cat('\nWspó³czynnik Pearsona:', (srednia - dominanta) / s)
cat('\nWspó³czynnik pozycyjny:', (Q3 + Q1 - 2 * Q2) /  (2 * Q))
m3 <- sum(((srodekPrzedzilu - sum(iloczynSrodekPrzedzialuWi)) ^ 3) * liczebnoscNiNaPrzedzialach) / N
cat('\n3-ci moment centralny:', m3)
WA <- m3 / (s ^ 3)
cat('\nWspó³czynnik asymetrii:', WA)
if (WA>0){
  cat('\nNa podstawie wspólczynnika asymetrii mo¿na stwierdziæ, ¿e wystêpuje asymetria prawostronna.')
  asymetria2 <- 1
}
if (WA<0){
  cat('\nNa podstawie wspólczynnika asymetrii mo¿na stwierdziæ, ¿e wystêpuje asymetria lewostronna.')
  asymetria2 <- -1
}
if (WA==0){
  cat('\nNa podstawie wspólczynnika asymetrii mo¿na stwierdziæ, ¿e rozklad jest symetryczny.')
  asymetria2 <- 0
}
absWA <- abs(WA)
if (absWA>0 & absWA<=0.2){
  cat('\nAsymetriê mo¿na okreœliæ jako bardzo slab¹.')
}
if (absWA>0.2 & absWA<=0.4){
  cat('\nAsymetriê mo¿na okreœliæ jako slab¹.')
}
if (absWA>0.4 & absWA<=0.6){
  cat('\nAsymetriê mo¿na okreœliæ jako umiarkowan¹.')
}
if (absWA>0.6 & absWA<=0.8){
  cat('\nAsymetriê mo¿na okreœliæ jako siln¹.')
}
if (absWA>0.8 & absWA<=1){
  cat('\nAsymetriê mo¿na okreœliæ jako bardzo siln¹.')
}
if (asymetria1 != asymetria2 & asymetria1 != 0){
  cat('\nRó¿nica we wskazaniach 2 mierników asymetrii mo¿e wynikaæ z niewielkiej iloœci danych lub te¿ ze zbli¿onych wartoœci œredniej, mediany i dominanty.')
}

cat('\n\nMIARY KURTOZY')

m4 <- sum(((srodekPrzedzilu - sum(iloczynSrodekPrzedzialuWi)) ^ 4) * liczebnoscNiNaPrzedzialach) / N
cat('\n4-ty moment centralny:', m4)
kurtoza <- m4 / (s ^ 4)
cat('\nKurtoza:', kurtoza)
cat('\nWspó³czynnik ekscesu:', kurtoza - 3)
cat('\nWartoœci kurtozy oraz wspó³czynnkika ekscesu wstazuj¹ na to, ¿e rozk³ad jest ')
if (kurtoza > 3){
  cat('wysmuk³y.')
}
if (kurtoza == 3){
  cat('normalny.')
}
if (kurtoza < 3 & kurtoza > -3){
  cat('umiarkowanie sp³aszczony.')
}
if (kurtoza < -3){
  cat('sp³aszczony.')
}

# wykresy
barplot(liczebnoscNiNaPrzedzialach, 
        names.arg = przedzialy, 
        main = "Histogram liczebnoœci na przedzia³ach")

barplot(liczebnoscNiNaPrzedzialachSkumulowane, 
        names.arg = przedzialy, 
        main = "Skumulowany histogram liczebnoœci na przedzia³ach")

plot(y = czestosciWzgledneNaPrzedzialachSkumulowane, 
     x = gornaGranicaPrzedzialu, 
     xlab = '',
     ylab = 'Czêstoœci skumulowane',
     type = "b",
     main = "Dystrybuanta empiryczna dla szeregu przedzia³owego",
     col="blue")

plot(y = czestosciWzglesneLaczneNaPrzedzialachSkumulowane * 100, 
     x = czestosciWzgledneNaPrzedzialachSkumulowane * 100, 
     xlim = c(0, 100),
     ylim = c(0, 100),
     xlab = '',
     ylab = '',
     type = "b",
     main = "Krzywa Lorenz'a dla szeregu przedzia³owego",
     col="red")
lines(y = c(1:100), 
      x = c(1:100), 
      type = "l",
      col="blue")

cat('\n\nWykresy zosta³y automatycznie zapisane w pliku pdf w folderze roboczym.\n')
dev.off()









