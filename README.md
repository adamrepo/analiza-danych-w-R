# analiza-danych-w-R
Podstawowe charakterystyki rozkładu zmiennych oraz dopasowanie modelu predykcyjnego.


Celem pewnego badania statystycznego jest analiza 3 cech: Y, X1 oraz X2 pochodzących z pewnej populacji. W przypadku każdej z tych cech wylosowano n-elementową próbę. Na podstawie wylosowanych danych empirycznych oceniono parametry podstawowych statystyk Y, jej trend oraz zbadano potencjalną współzależność Y od pozostałych cech.

Na podstawie danych przygotowano skrypt R zawierający:

1.a) opis podstawowych statystyk zmiennej Y: średnią arytmetyczną, medianę, dominantę oraz 1-, 2- i 3-ci kwartyl,

1.b) kształt i typ rozkładu zmiennej Y,

1.c) dystrybuantę szeregu punktowego Y oraz histogram,

2.konstrukcję modelu Y=f(X1,X2), czyli opisującego zależności między cechami: Y, X1, X2 za pomocą podstawowych miar jakości dopasowania modelu do danych empirycznych (np. R2) oraz wybór ostatecznej postaci modelu po ewentualnej redukcji tej zmiennej Xk (k=1,2), która w najmniejszym stopniu wpływa na wyjaśnienie Y i/lub nie jest istotna,

3.a) ponownie oszacowane parametry modelu ostatecznie wykonanego/wybranego w punkcie 2. na podstawie obserwacji od t1 do tn-2,

3.b) wyznaczone prognozy Y na okresy: tn-1, tn bazując na wartościach odpowiedniej/odpowiednich zmiennych Xk z okresów: tn-1, tn,

3.c) ocenę jakości wyznaczonych prognoz.
