#  Logický projekt - Prolog (Hamiltonovská kružnice)

- **Jméno**: Filip Jahn
- **Login**: xjahnf00
- **Akademický rok**: 2024/2025
- **Název projektu**: Hamiltonovská kružnice

## Úvod
Tato aplikace je určena k průzkumu neorientovaných grafů za účelem hledání Hamiltonovských cyklů. Hamiltonovský cyklus (nebo také kružnice) prochází každým vrcholem grafu právě jednou, přičemž začíná a končí ve stejném vrcholu.

## Funkce
- **Očekávaný formát vstupu**: Jednotlivé hrany sepsány vždy na novém řádku. Uzly odděleny mezerou. Na konci souboru není nový řádek.
- **Hledání Hamiltonovských Cyklů**: Aplikace umožňuje procházet neorientované grafy a hledat v nich Hamiltonovské cykly, které procházejí každým vrcholem právě jednou.
- **Formátování Výstupu**: Výstup aplikace je přehledně formátován v souladu se specifikacemi zadání. Tedy po řádcích jednotlivé cesty, seřazeny abecedně jak v hranách mezi uzly (kupříkladu hrana `[B - A]` je seřazena vzestupně do podoby `[A - B]`), tak mezi hranami (ukázka na trojuhelníku: `[A - C], [C - B], [B - A]` by bylo seřazeno jako: `[A - B], [A - C], [B - C]`).

## Použití
1. **Kompilace Projektu**:
   - Zajistěte, že máte nainstalovaný kompilátor pro jazyk Prolog.
   - Spusťte příkaz `make` v kořenovém adresáři projektu.

2. **Spouštění Aplikace**:
   - Spusťte aplikaci s vstupními daty pomocí následujícího příkazu:
     ```
     ./pruzkumnik_grafu < tests/input/zadani.txt
     ```

## Testování
- Testování aplikace bylo provedeno pomocí základních testovacích příkladů poskytnutých od kamaráda a spolužáka Davida Miholy. Tímto mu velice děkuji za jeho vypracování testů.
- Testy byly upraveny, aby odpovídaly abecednímu pořadí, které vrací program.
- Výstupy testů byly ověřeny a porovnány s očekávanými výstupy.
- Všechny testy byly úspěšné, což potvrzuje správnou funkčnost aplikace.
- Testování proběhlo na lokálním počítači i na referenčním serveru Merlin.
- Pro spuštění testů je možno zadat příkaz `./test_all.sh` v kořenovém adresáři, který spustí veškeré testy a vypíše výsledky do konzole. 

## Implementační Detaily
- Program je implementován v jazyce Prolog.
- Zpracování vstupu i výstupu je realizováno s ohledem na požadavky specifikace za využití poskytnutého skriptu `input2.pl`.
- Hlavní algoritmus pro hledání Hamiltonovských cyklů je implementován ve funkci `hamiltonian_cycle`, která postupně rekurzivně prochází graf, dokud nejsou veškeré uzly projité. Cestu si postupně ukládá a na konci vrací.
Funkce využívá pomocnou funkci `are_all_nodes_visited` pro zjištení, jestli už byly veškeré uzly projité. Pokud ne, pak zjistí z databáze hran (edge/2) následující uzel, který pokud již nebyl použit, přidá se do výsledné cesty.
- Funkce `hamiltonian_cycle` je volána z funkce `main` s využitím interní funkce `findall`, která zajistí, že budou prohledány veškeré cesty. Interně jsou projité hrany ukládány jako dvojice `[V1, V2]`. Poslední hrana je zajištěna předáváním si počáteční hrany - při zjištění, že existuje hrana z posledního uzlu do počátečního, je hrana přidána jako finální a pro tuto cestu je řešení konečné.

## Reference
Při rešerši problému byly velice nápomocny následující zdroje:
- [Wikipedia - Hamiltonian Path](https://en.wikipedia.org/wiki/Hamiltonian_path) - Základní přehled co je to hamiltonovská kružnice
- [GitHub - Hamiltonian Cycle in Graph](https://github.com/praeducer/prolog/blob/master/hamiltonian-cylce-in-graph) - Algoritmus Hamiltonovské kružnice napsán v prologu. Bylo vymyšleno odlišné řešení, ale pro počáteční porozumnění velice vhodné
- [YouTube - Removing Duplicates](https://youtu.be/rToAEX5L6bk?si=Tt5D_NUqXK5swjTJ) - Vysvětlení problému a možné řešení

## Dodatek
Pro porozumnění problému byly čerpány určité zdroje, ale autor se snažil přijít s lehce odlišným řešením v určitých oblastech. Ukládání projitých uzlů je zajištěno dvojicí `[V1, V2]`; podmínka, jestli jsou veškeré hrany projité vyžadovalo implementaci nové funkce; řazení uzlů a hran finálně bylo zajištěno přidáním patřičných funkcí, které skýtalo vyřešení problémů s duplikáty, porovnávání a využití znalostí algoritmu `bubble sort`, který byl prakticky implementován pro řazen.
Konečný výpis zahrnoval vytvoření dalších funkcí, které tuto funkcionalitu zaštiťují.