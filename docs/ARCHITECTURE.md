# AstraLang — Diagramme d'architecture

## Flux de compilation (pipeline)

```
  Source (.astra)
       │
       ▼
  ┌─────────┐
  │  Lexer  │  tokenize
  └────┬────┘
       │ Token list
       ▼
  ┌─────────┐
  │ Parser  │  parse
  └────┬────┘
       │ UntypedAst
       ▼
  ┌──────────────────┐
  │ TypeInference    │  Algorithm W, unify, generalize
  └────┬─────────────┘
       │ TypedAst
       ▼
  ┌─────────┐
  │   IR    │  ANF, lambda lift, closure conversion
  └────┬────┘
       │ IR.Program
       ▼
  ┌─────────────┐
  │ Optimizer   │  constant fold, beta, DCE, inline
  └────┬────────┘
       │ IR.Program
       ▼
  ┌─────────────┐
  │  Compiler   │  IR → Bytecode
  └────┬────────┘
       │ Bytecode.Program
       ▼
  ┌─────────┐     ┌──────────┐
  │   VM    │────▶│ GC (M&S) │
  └────┬────┘     └──────────┘
       │
       ▼
  Result (Value)
```

## Dépendances entre projets

```
                    ┌─────────────┐
                    │ AstraLang   │
                    │    .Core    │
                    └──────┬──────┘
           ┌───────────────┼───────────────┐
           │               │               │
           ▼               ▼               ▼
    ┌──────────┐    ┌──────────┐    ┌──────────┐
    │  Lexer   │    │  Parser  │    │ Runtime  │
    └────┬─────┘    └────┬─────┘    └────┬─────┘
         │               │               │
         └───────┬───────┘               │
                 ▼                       │
         ┌──────────────┐                │
         │ TypeInference│                │
         └──────┬───────┘                │
                │                        │
                ▼                        │
         ┌──────────────┐                │
         │     IR       │                │
         └──────┬───────┘                │
                │                        │
                ▼                        │
         ┌──────────────┐                │
         │  Optimizer   │                │
         └──────┬───────┘                │
                │                        │
                ▼                        │
         ┌──────────────┐                │
         │   Compiler   │────────────────┤
         └──────┬───────┘                │
                │                        │
                ▼                        ▼
         ┌──────────────┐         ┌──────────────┐
         │     VM       │────────▶│ Runtime/GC   │
         └──────┬───────┘         └──────────────┘
                │
                ▼
         ┌──────────────┐
         │     CLI      │
         └──────────────┘
```

## Système de types (résumé)

- **Type** : int | bool | unit | TypeVar | Tuple of Type list | Arrow of Type * Type
- **TypeScheme** : Forall of TypeVar list * Type
- **Unification** : résolution des contraintes, occurs-check pour récursion infinie.
- **Généralisation** : au let, généraliser les variables libres non liées par l’env.
- **Instanciation** : à chaque utilisation d’une valeur polymorphe, créer des copies fraîches.

## Runtime

- **Value** : VInt | VBool | VUnit | VTuple of Value list | VClosure of int (heap ref)
- **Heap** : map Adress → Value (tuples, closures).
- **GC** : marquage depuis stack + registres + champs des valeurs ; balayage des non marqués.

## Bytecode (stack-based)

- Pile d’opérandes : valeurs temporaires.
- Call stack : (return IP, env, arity).
- Instructions : Push*, Load/Store (env), Call/TailCall/Return, MakeClosure, Jump/JumpIfFalse.
