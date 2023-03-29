## FLP 2023 Projekt 1 -- ECDSA 
###  Autor: Vojtěch Fiala \<xfiala61\>
## Obsah adresáře:

src/ - zdrojové soubory
tests/ - soubory se vstupy, které jsou dále testovány jednoduchým shell a python skriptem
doc/ - složká s dokumentací, kterou reprezentuje tento soubor

Pro překlad je nutné v kořenovém adresáři použít **make**.
Pro spuštění testů je možné použít **make test**, nejprve je ovšem nutné program přeložit příkazem make.

## Informace k řešení:
Řešení předpokládá, že vstupy budou *KOREKTNÍ* a v jednotném formátu, konkrétně tom, jaký je uveden v zadání  -- tedy že např. u klíče na vstupu budou jeho části označeny vždy jako *d* a *Q* pro privátní, respektive veřejný klíč a hodnoty samotné budou taktéž validní.

V případě, že formát vstupu korektní není, tedy buď je úplně špatně a nebo v jiném než očekáváném formátu, program končí s chybou. Hodnoty program až na výjimky nekontroluje -- pokud ovšem budou v jiném formátu než je uveden v zadání (např. v desítkové soustavě namísto hexadecimální), dojde k chybě.

Konkrétní vstupy, na kterých byla implementace testována, se nacházejí ve složce tests/
V případě zadání neplatného množství argumentů či neznámého argumentu, dojde k vypsání nápovědy, kterou je taktéž možné vypsat s parametrem *-h*.