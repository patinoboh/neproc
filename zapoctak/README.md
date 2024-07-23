# Bludiská

Patrik Broček

## RNG

### randomR/Pomocou IO Int

```hs
rng :: Int -> Int -> IO Int
rng min max = getStdRandom (randomR (min, max))

rngList :: Int -> Int -> Int -> IO [Int]
rngList min max n = replicateM n (rng min max)
```

__Problém__ : Celý kód by musel byť v IO



## Algoritmus

### Naivný

Vygenerovať náhodné súradnice bodov a prepojiť ich cestami

__Problém__ : 

### Vylepšený naivný

1. Vygenerovať náhodné súradnice bodov tak, aby vytvorili cestu, ktorá bude riešením. 
2. Tieto cesty následne doplniť o slepé cesty

__Problém__ : Cesta sa bude väčšinou pretínať a preto bude riešenie bludiska jednoduché

### Slepé cesty

Nájsť rohy/križovatky na hlavnej ceste a z nich nejak generovať cestičky.
Ako generovať cestičký?

