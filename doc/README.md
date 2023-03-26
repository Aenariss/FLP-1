## FLP Projekt 1 - ECDSA
## 2023
## Autor: Vojtech Fiala <xfiala61>

### Obsah adresare:

src/ - zdrojove soubory
tests/ - soubory se vstupy, ktere jsou dale testovany jednoduchym shell a python skriptem
doc/ - slozka s dokumentaci obsahujici tento soubor

Pro preklad je nutne v korenovem adresi pouzit **make**.
Pro spusteni testu je mozne pouzit **make test**

### Informace k reseni:
Reseni predpoklada, ze vstupy budou KOREKTNI a v jednotnem formatu - tedy ze hodnoty napr. u klice budou na vstupu vzdy oznaceny jako d a Q pro privatni, respektive verejny a jejich hodnoty budou taktez validni.
Ocekavany format vstupu vychazi ze zadani, konkretni vstupy, na kterych byla implementace testovana, se nachazeji ve slozce tests/


V pripade neplatneho argumentu ci parametru -h se vypise napoveda.