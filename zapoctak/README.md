# Bludiská

Patrik Broček

## Zadanie

Zadním bolo vytvoriť generátor bludísk. Tento problém sa môže zdať pomerne jednoduchý, pretože sa dá uchopiť rôznymi spôsobmi. Spôsob riešenie závisí hlavne na parametroch, ktoré musí bludisko spĺňať. Ja som si vybral parametre, ktoré si myslím, že bludisko charakterizujú dostatočne a tými sú :

- Výška
- Šírka
- Počet zákrut správnej ceste
- Maximálny počet zákrut na vedľajších cestách

Deterministický algoritmus pre vytvorenie bludiska som nebol schopný vytvoriť a tak som sa rozhodol používať náhodné čísla. Bludiská ktoré budem generovať budú bez cyklov, pretože väčšina ak nie všetky bludiská spĺňajú túto vlastnosť.

## Program

Program zostáva z troch časti :

- MazeUtils.hs
- RandomUtils.hs
- MazeGenerator.hs

Prvé dva sú moduly ktoré sa importujú v `MazeGenerator.hs`.

## Spustenie

Program sa dá spustiť ako v ineraktívnom móde, tak sa dá aj skompilovať. Ak je cieľom len generovať bludiská, odporúčam program skompilovať a len sa hrať s parametrami.

### Interaktívny režim

- Interaktívny režim sa spúšťa jednoducho pomocou `ghci MazeGenerator.hs`
- Bludisko sa inicializuje pomocou `maze = initializeMaze height width`
- Body hlavnej sa získajú pomocou `points <- generatePath (0, height -1), (0, width -1) (startRow, startCol) length`
- Ak chcete tieto body pridať do cesty : `mazeWithPath = setFree points maze`
- Pre vygenerovanie slepej cesty z bodu je príkaz : `deadEndPoints <- (0, height -1), (0, width -1) (startRow, startCol) (generatePathPoints points) True/False length`
  - Táto funkcia vráti len križovatky slepej cesty, pre vytvorenie cesty z nich treba opäť použiť funkciu `generatePathPoints`
- Tieto body môže následne opäť vykresliť do bludiska

### Kompilácia

Kompilácia programu je tiež veľmi jednoduchá, stačí použiť príkaz :

```bash
ghc --make MazeGenerator.hs -o NazovBinarky
./NazovBinarky height width startColumn mainPathLength deadEndMaxLength
```

Príklad :

```bash
ghc --make MazeGenerator.hs -o maze
./maze 21 31 6 10 6
```

## RNG

Veľká časť celého programu závisí na náhodnosti a tak potrebujem generovať náhodné čísla. To je v neprocedurálnom jazyku pomerne náročné a existujú pre to rôzne spôsoby.

### randomR/Pomocou IO Int

```hs
rng :: Int -> Int -> IO Int
rng min max = getStdRandom (randomR (min, max))

rngList :: Int -> Int -> Int -> IO [Int]
rngList min max n = replicateM n (rng min max)
```

__Problém__ : Celý kód by musel byť v IO

__Riešenie__ : Vygenerovať ich veľa naraz a následne len posielať. Toto riešenie som však nemohol použiť, pretože potrebujem často generovať čísla a zoznamy z iných rozsahov.

### Posielať si generátor

```hs
randomNumberState :: StatefulGen g => g -> (Int, g)
randomNumberState gen = runState (uniformR (1, 100)) gen
```

Toto riešenie je fajn, avšak pre moje využitie mi nakoniec prišlo príliš otravné, pretože už tak si posielam do funkcií príliš veľa parametrov.

## Algoritmus

Hlavnou myšlienkou môjho algoritmu je rozdeliť ho na dve časti :

- Vygenerovanie správnej cesty
- Doplnenie slepých ciest do bludiska

### Generovanie hlavnej cesty

#### Naivne

Na začiatku som si myslel, že bude stačiť vygenerovať náhodné body pre križovatky (samozrejme tak, aby boli prepojené rovnými čiarami).

__Problém__ : Generuje cykly

#### Vylepšený naivný

Generovať tieto body kým výsledná cesta nebude bez cyklu. Toto riešenie sa ukázalo ako nefunkčné, keďže sa aj pre malé čísla často nedočkáme riešenia. Súbor `Nepodarky.hs`

#### DFS

Nakoniec sa mi ukázalo, že treba križovatky generovať postupne a zachovávať invariant aby sa nepretínali. Križovatky teda generujem backtrackovaním s tým, že vždy strieda vertikálny a horizontálny posun. V každom kroku v náhodnom poradí vygenerujem všetky body, kam by sa dala priradiť nová križovatka tak, aby nevznikol cyklus. Následne sa zarekurzím a rekurzívny výsledok nemá riešenie, skúsim ďaľšie. Posledný bod musí byť na hrane a tak ho kôli efektivite skúsim prepojiť na každú hranu. Kôli nedeterminizmu tohto kroku preto optimálna cesta môže obsahovať o jednu križovatku viac ako je požadované.

__Správnosť__ : Tento algoritmus vždy nájde riešenie, pretože v najhoršom prípade vyskúša všetky možnosti a ak nie je počeť križovatiek násobne väčší ako veľkosť ihriska, tak takýto "hadík" vždy existuje.

__Konečnosť__ : Algoritmus sa nezacyklí, v každom kroku vyskúša maximálne `max(výška, šírka) // 2` bodov.

__Efektivita__ : Efektivita tohto algoritmu je náročná. Keďže skúšam všetky možnosti tak je algoritmus exponenciálny s počtom križovatiek. Prakticky sa ale faktor vetvenia so zvyšujúcim počtom križovatiek znižuje a tak je algoritmus prakticky využiteľný. Jeho zložitosť veľmi závisí na výbere prvých križovatiek. Zo skúsenosti buď zbehne veľmi rýchlo alebo počíta veľmi dlho. Preto pri dlšom výpočte odporúčam program reštartovať.

#### Slepé cesty

Z každej križovatky na "hlavnej" ceste potrebujem vytvoriť slepé cesty. Pre ich vytvorenie používam veľmi podobný algoritmus ako pre generovanie hlavnej cesty. Avšak pre slepé cesty nemôžem backtrackovať a skúšať ďaľšie možnosti, pretože tu už sa mi môže ľahko stať, že riešenie s presným počtom ciest neexistuje. 
Ďaľším rozdielom od predošlého algoritmu je, že pri vytváraní slepých ciest ich už nemôžem prepájať na kraje.
Pre každú križovatky teda generujem možné následné body a jednoducho ich pridávam k ceste kým sa dá.

## Možné rozšírenia

V momentálnom stave program generuje pomerne pekné bludiská. Program by sa však dal jednoducho rozšíriť aby generoval ešte zložitejšie bludiská :

- Na každej križovatke generovať slepú cestu aj v horizontálnom aj vo vertikálon smere
- Generovať slepé cesty aj na náhodných bodoch cesty ; či už hlavnej alebo vedľajších
  - Potom by už ale program nespĺňal parametre ktoré mu zadávam, pretože by hlavnú, či vedľajšie cesty mohlo tvoriť viac križovatiek
