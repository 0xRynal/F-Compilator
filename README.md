# AstraLang

Compilateur fonctionnel en F# (niveau OCaml-lite) avec inférence Hindley–Milner, IR, optimisations, bytecode et VM avec GC.

## Architecture

- **AstraLang.Core** : AST, types, schémas, IR, bytecode
- **AstraLang.Lexer** : tokenisation avec positions
- **AstraLang.Parser** : parsing récursif vers AST non typé
- **AstraLang.TypeInference** : unification + Algorithm W (généralisation, instanciation)
- **AstraLang.IR** : ANF, lambda lifting, closure conversion
- **AstraLang.Optimizer** : constant folding, dead code elimination
- **AstraLang.Compiler** : IR → bytecode
- **AstraLang.Runtime** : valeurs, heap, GC mark-and-sweep
- **AstraLang.VM** : machine à pile, closures, frames
- **AstraLang.CLI** : exécution de fichiers, REPL, flags

## Utilisation

```bash
# Exécuter un fichier .astra
dotnet run --project src/AstraLang.CLI -- examples/example.astra

# REPL
dotnet run --project src/AstraLang.CLI

# Afficher IR et bytecode
dotnet run --project src/AstraLang.CLI -- --show-ir --show-bytecode examples/example.astra
```

## Exemple

```ocaml
let id = fun x -> x
let compose = fun f g x -> f (g x)
let square = fun x -> x * x
let result = compose square id 5
result
```

## Image de preview

Une image type « terminal » (`preview.png`) peut être générée à partir de la page `docs/preview.html` :

```bash
cd scripts
npm install
npm run preview
```

Le script utilise Puppeteer pour ouvrir la page et enregistrer une capture dans `preview.png` à la racine du repo. Tu peux aussi ouvrir `docs/preview.html` dans un navigateur et faire une capture d’écran manuelle.

## Tests

```bash
dotnet test AstraLang.sln
```

C# : 5 tests (unification). F# : ~90 tests (unification, inférence, VM) compilés ; non découverts par xUnit sous .NET 10. Vérif manuelle : `dotnet run --project src/AstraLang.CLI -- examples/example.astra`

## Spécification

- **Types** : int, bool, unit, tuples, flèche, variables de type (polymorphisme let)
- **Fonctionnalités** : let polymorphe, fonctions anonymes, application partielle, pattern matching, récursion, let rec, if/then/else, opérateurs arith/logique/comparaison, portée lexicale
