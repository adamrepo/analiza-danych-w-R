# Skrypt ten ma za zadanie w sposób ca³oœciowy analizowaæ Y empiryczne w postaci próby pobranej z populacji.

#pdf(file = "wykresy.pdf")# eksport wykresów do pliku

cat('\nDane kszta³tuj¹ siê nastêpuj¹co:')
# wczytanie danych
# dane <- read.csv("dane_zadanie_projektowe.csv", header = TRUE, sep = ";", dec = ",")
# y <- dane$Y
# x1 <- dane$X1
# x2 <- dane$X2
# t <- dane$ï.¿t

y <- c(42, 24, 44, 11, 41, 53, 24, 24, 53, 72, 39, 59, 66, 64, 62, 41, 53, 59)
x1 <- c(44.15, 23.88, 38.46, 12.07, 35.94, 51.74, 24.79, 30.43, 57.64, 67.9, 33.3, 57.75, 59.79, 73.75, 66.83, 36.29, 48.14, 65.06)
x2 <- c(54.2, 59.47, 56.8, 67.23, 45.1, 47.04, 63.89, 55.5, 44.39, 46.33, 62.91, 58.39, 58.71, 43.61, 43.35, 60.84, 53.93, 56.5)
t <- c(1:18)

data.frame(t, y, x1, x2)

# Opis podstawowych statystyk zmiennej Y

# Y posortowane
y_sort <- sort(y)

# opis danych
N <- length(y_sort)
cat('\nZbiór danych zawiera', N, 'elementów.')
cat('\nMediana:', median(y_sort))

# posortowane unikalne wartoœæi
unikalneWartosci <- sort(unique(y_sort))

# iloœæ wyst¹pieñ poszczególnych wartoœci
liczebnoscNi <- rep(0, length(unikalneWartosci))
for (i in 1:length(unikalneWartosci)) {
  liczebnoscNi[i] = length(which(y_sort == unikalneWartosci[i]))
} 

data.frame(unikalneWartosci, liczebnoscNi)

# iloœæ wyst¹pieñ wartoœci w stosunku do ca³kowitej liczby danych
czestosciWzgledne <- liczebnoscNi / length(y_sort)

liczebnosciSkumulowane <- cumsum(liczebnoscNi)
czestosciWzgledneSkumulowane <- cumsum(czestosciWzgledne)

iloczynXiNi <- unikalneWartosci * liczebnoscNi
iloczynXiWi <- unikalneWartosci * czestosciWzgledne

czestosciWzgledneLaczne <- iloczynXiNi / sum(iloczynXiNi)
czestosciWzgledneLaczneSkumulowane <- cumsum(czestosciWzgledneLaczne)

# œrednia arytmetyczna
srednia <- sum(iloczynXiNi) / N
cat('\nŒrednia arytmetyczna wynosi:', srednia)

# dominanta
dominanta <- unikalneWartosci[which(max(liczebnoscNi) == liczebnoscNi)]
if (length(dominanta) == 1){
  cat('\nDominanta to:', dominanta)
}
if (length(dominanta) != 1){
  cat('\nNie mo¿na wyznaczyæ dominanty, gdy¿ jest wiêcej jest wiêcej ni¿ jedna wartoœæ z najwiêksz¹ liczebnoœci¹.')
}

Q1 <- quantile(y, 0.25)
Q2 <- quantile(y, 0.50)
Q3 <- quantile(y, 0.75)

# kwantyle
cat('\nWartoœæ kwantyla 25% wynosi:', quantile(y, 0.25))
cat('\nWartoœæ kwantyla 50% wynosi:', quantile(y, 0.50))
cat('\nWartoœæ kwantyla 75% wynosi:', quantile(y, 0.75))

# kszta³t i typ rozk³adu zmiennej Y

asymetria1 <- 0
asymetria2 <- 0
if (length(dominanta) == 1){
  if (srednia < Q2 & Q2 < dominanta){
    cat('\nNa podstawie porównañ wartoœci œredniej, mediany i dominanty stwierdzamy, ¿e œrednia < mediana < dominanta, co wskazuje na asymetriê lewostronn¹ rozkladu.')
    asymetria1 <- -1
  }
  if (srednia > Q2 & Q2 > dominanta){
    cat('\nNa podstawie porównañ wartoœci œredniej, mediany i dominanty stwierdzamy, ¿e œrednia > mediana > dominanta, co wskazuje na asymetriê prawostronn¹ rozkladu.')
    asymetria1 <- 1
  }
}

cat('\nEmpiryczny obszar zmiennosci:', max(unikalneWartosci) - min(unikalneWartosci))
cat('\nRozstêp miêdzykwartylowy:', Q3 - Q1)
Q <- (Q3 - Q1) / 2
cat('\nOdchylenie æwiartkowe:', Q)
cat('\nPozycyjny typowy obszar zmiennoœci:', Q2 - Q, '-', Q2 + Q)
cat('\nPozycyjny wspó³czynnik zmiennoœci:', (Q / Q2) * 100)
wariancja <- sum(((unikalneWartosci - sum(iloczynXiWi)) ^ 2) * liczebnoscNi) / N
cat('\nWariancja:', wariancja)
s <- sqrt(wariancja)
cat('\nOdchylenie standardowe:', s)
cat('\nTypowy klasyczny obszar zmiennoœci:', srednia - s, '-', srednia + s)
cat('\nKlasyczny wspó³czynnik zmiennoœci:', (s / srednia) * 100)

if (length(dominanta) == 1){
  cat('\nWspó³czynnik Pearsona:', (srednia - dominanta) / s)
}
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

# dystrybuantê szeregu punktowego Y oraz histogram
barplot(liczebnoscNi, 
        names.arg = unikalneWartosci, 
        main = "Histogram liczebnoœci unikalnych wartoœci")

barplot(liczebnosciSkumulowane, 
        ylim = c(0, 20),
        names.arg = unikalneWartosci, 
        main = "Skumulowany histogram liczebnoœci unikalnych wartoœci")

plot(y = czestosciWzgledneSkumulowane, 
     x = unikalneWartosci, 
     xlab = '',
     ylab = 'Czêstoœci wzglêdne skumulowane',
     type = "b",
     main = "Dystrybuanta empiryczna dla szeregu punktowego",
     col = "blue")

library(corrplot)
#macierz kowariancji
x <- cbind(y, x1, x2, t)
xkor <- cor(x)
corrplot(xkor, method = "number")

plot(t, y, type = "b", main = "Wartoœci zmiennej Y w czasie t", col="blue")
plot(x1, y, main = "Zale¿noœæ zmiennej Y od X1")
plot(x2, y, main = "Zale¿noœæ zmiennej Y od X2")

#modele liniowe w ró¿nych kombinacjach
lin1 <- lm(y ~ x1 + x2 + t)
summary(lin1)
lin2 <- lm(y ~ x1)
summary(lin2)
lin3 <- lm(y ~ x2)
summary(lin3)
lin4 <- lm(y ~ t)
summary(lin4)

# widzimy ¿e cechy x2 i t nie s¹ istotne, nie wp³ywaj¹ na wyjaænienie Y
# do dalszej analiwy wybieramy wiêc tylko cechê x1
a0 <- lin2$coefficients[1]
a1 <- lin2$coefficients[2]
yteorLin = a0 + a1 * x1

# model hiperboliczny
x11 <- 1 / x1
hip <- lm(y ~ x11)
summary(hip)

a0 <- hip$coefficients[1]
a1 <- hip$coefficients[2]

yteorHip = a0 + a1 / x1

#model potêgowy
pot <- lm(log(y) ~ log(x1))
summary(pot)

a0 <- exp(pot$coefficients[1])
a1 <- pot$coefficients[2]

yteorPot = a0 * x1 ^ a1

# model wyk³adniczy
wyk <- lm(log(y) ~ x1)
summary(wyk)

a0 <- exp(wyk$coefficients[1])
a1 <- exp(wyk$coefficients[2])

yteorWyk = a0 * a1 ^ x1

# zestawienie zmiennej Y i ró¿nych modeli na wykresie
plot(y, main = "Porównanie modeli")
lines(yteorLin, type = "l", col = "green")
lines(yteorWyk, type = "l", col = "blue")
lines(yteorHip, type = "l", col = "black")
lines(yteorPot, type = "l", col = "red")
legend(12, 25, legend = c("Model liniowy", "Model wyk³adniczy", "Model hiperboliczny", "Model potêgowy"),
       col=c("green", "blue", "black", "red"), lty=1, cex=0.8)

# wyliczenie wspó³czynnika R^2 dla ka¿dego modelu
RKyteorLin = 1 - (sum((y - yteorLin) ^ 2) / sum((y - srednia) ^ 2))
RKyteorWyk = 1 - (sum((y - yteorWyk) ^ 2) / sum((y - srednia) ^ 2))
RKyteorHip = 1 - (sum((y - yteorHip) ^ 2) / sum((y - srednia) ^ 2))
RKyteorPot = 1 - (sum((y - yteorPot) ^ 2) / sum((y - srednia) ^ 2))

data.frame(RKyteorLin, RKyteorWyk, RKyteorHip, RKyteorPot)
# Widzimy ¿e zmienna Y najlepiej opisuje model liniowy Y = f(X1)
# ten te¿ model wybieramy do dalszej analizy

# ponownie oszacowane parametry modelu ostatecznie wybranego na podstawie obserwacji od t1 do tn-2,
x1_test <- x1[1:16]
y_test <- y[1:16]
lin <- lm(y_test ~ x1_test)
summary(lin)
a0 <- lin$coefficients[1]
a1 <- lin$coefficients[2]
yteorLin_test = a0 + a1 * x1_test
RKyteorLin_test = 1 - (sum((y_test - yteorLin_test) ^ 2) / sum((y_test - srednia) ^ 2))

faktyczna_nminus1 <- y[N - 1]
faktyczna_n <- y[N]

# prognozy dla czasów tn-1 i tn
prognoza_t_nminus1 <- a0 + a1 * x1[N - 1]
prognoza_t_n <- a0 + a1 * x1[N]

# ró¿nice miêdzy wartoœciami prognoz, a faktycznymi
roznica_prognoza_faktyczna_nminus1 <- faktyczna_nminus1 - prognoza_t_nminus1
roznica_prognoza_faktyczna_n <- faktyczna_n - prognoza_t_n

df <- data.frame(faktyczna_nminus1, 
                 prognoza_t_nminus1,
                 roznica_prognoza_faktyczna_nminus1, 
                 faktyczna_n,
                 prognoza_t_n,
                 roznica_prognoza_faktyczna_n, 
                 RKyteorLin_test)

transpose_df <- as.data.frame(t(as.matrix(df)))
transpose_df
cat('\nPrognoza ró¿ni siê od wartoœci faktycznej dla czasu n-1 o: ', roznica_prognoza_faktyczna_nminus1)
cat('\nCo procentowo daje nam róznicê rzêdu ', roznica_prognoza_faktyczna_nminus1 / faktyczna_nminus1 * 100, '%')
cat('\nPrognoza ró¿ni siê od wartoœci faktycznej dla czasu n o: ', roznica_prognoza_faktyczna_n)
cat('\nCo procentowo daje nam róznicê rzêdu ', roznica_prognoza_faktyczna_n / faktyczna_n * 100, '%')

#cat('\n\nWykresy zosta³y automatycznie zapisane w pliku pdf w folderze roboczym.\n')
#dev.off()