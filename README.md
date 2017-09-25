
# ocaml-polynomials
(readme in polish)

Projekt ```ocaml-polynomials``` implementuje wielomiany rzeczywiste jednej zmiennej w OCamlu,
a także wprowadza wiele działających na nich funkcji.

## Kompilacja modułu, korzystanie z funkcji i uruchamianie własnych programów
Program składa się z jednego pliku ```polynomial.ml``` zawierającego definicję sygnatury ```POLYNOMIAL```
oraz implementującego ją modułu ```Polynomial```.
Moduł definiuje abstrakcyjny typ wielomianu rzeczywistego oraz szereg funkcji na nim operujących.

Aby skorzystać z danej funkcji modułu w swoim programie, należy w wybranym pliku odwołać się do niej instrukcją
```Polynomial.Polynomial.[nazwa funkcji]``` lub na początku pliku napisać ```open Polynomial.Polynomial```

Moduł kompiluje się poleceniem ```ocamlc -c polynomial.ml```.
Po wykonaniu instrukcji powstaną pliki ```polynomial.cmo``` oraz ```polynomial.cmi```.

Aby skompilować plik korzystający z modułu (zakładając, że jest w tym samym folderze, co ```polynomial.ml```),
należy najpierw skompilować sam moduł (instrukcja powyżej), a następnie użyć polecenia
```ocamlc graphics.cma polynomial.cmo [nazwa_pliku] -o [nazwa_wyjściowego_pliku_wykonywalnego]```

Na końcu własny program można uruchomić poleceniem
```ocamlrun [nazwa_wyjściowego_pliku_wykonywalnego]```

## Przykłady i uruchamianie testów
W pliku ```tests.ml``` znajduje się przykładowy kod wykorzystujący funkcje z modułu ```Polynomial```. 
Pełni on jednocześnie rolę testów sprawdzających poprawność funkcji.
Aby włączyć testy, należy uruchomić skrypt ```run_tests.sh```.

## Opis funkcji z modułu ```Polynomial```

- ```create``` - tworzy wielomian z podanej listy współczynników (od najwyższej potęgi do najniższej)

- ```parse``` - parsuje podany string i tworzy z niego wielomian (znak "do potęgi" to **^**)

- ```to_string``` - zwraca podany wielomian w postaci stringa

- ```print``` - wypisuje podany wielomian na standardowe wyjście

- ```deg``` - zwraca stopień podanego wielomianu

- ```calc``` - wylicza wartość wielomianu w podanym punkcie

- ```equal``` - sprawdza czy dwa wielomiany są sobie równe

- ```get_factor``` - zwraca współczynnik wielomianu przy podanej potędze

- ```set_factor``` - zmienia współczynnik przy wybranej potędze

- ```add``` - dodaje do siebie dwa wielomiany (tak samo działa operator infiksowy **+@**)

- ```sub``` - odejmuje od siebie dwa wielomiany (tak samo działa operator infiksowy **-@**)

- ```mult``` - mnoży dwa wielomiany (tak samo działa operator infiksowy ***@**)

- ```div``` - dzieli dwa wielomiany, zwracając wynik oraz resztę (tak samo działa operator infiksowy **/@**)

- ```add_float``` - dodaje wartość do wielomianu

- ```sub_float``` - odejmuje wartość od wielomianu

- ```mult_float``` - mnoży wielomian przez wartość

- ```div_float``` - dzieli wielomian przez wartość

- ```pow``` - podnosi wielomian do potęgi (tak samo działa operator infiksowy __**@__)

- ```mult_fft``` - mnoży dwa wielomiany wykorzystując algorytm FFT (szybkie mnożenie)

- ```deriv``` - wylicza pochodną wielomianu

- ```integral``` - wylicza całkę nieoznaczoną z wielomianu

- ```definite_integral``` - wylicza całkę oznaczoną z wielomianu na podanym przedziale

- ```draw``` - rysuje wielomian na podanym przedziale (domyślnie **[-1.,1.]**)

- ```draw_polynomials``` - dla podanej listy par (przedział, wielomian) rysuje kolejne wielomiany (po naciśnięciu dowolnego klawisza znika stary wielomian i pojawia się nowy)

## Wymagania
- Zainstalowany OCaml

## Licencja
Projekt posiada licencję MIT - zobacz plik [LICENSE](LICENSE)

## Autor
**Tomasz Jurkiewicz**
