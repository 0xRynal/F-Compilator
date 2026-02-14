# AstraLang — Plan détaillé par phases

## Vue d'ensemble

Compilateur fonctionnel type OCaml-lite en F#, avec inférence Hindley–Milner, IR, optimisations, bytecode et VM avec GC.

---

## Phase 1 : Fondations (Core)

**Objectif** : Définir les structures de données partagées.

- **AST non typé** : expressions, patterns, déclarations (let, let rec).
- **Types** : `Type` (int, bool, unit, tuple, arrow, var), `TypeVar` (génération fraîche), `TypeScheme` (∀α.τ).
- **Position source** : `SourceSpan` pour messages d'erreur.
- **IR** : définitions des formes intermédiaires (ANF, closures) dans Core pour référence.

**Livrables** : `AstraLang.Core` compilable avec DU exhaustives.

---

## Phase 2 : Lexer

**Objectif** : Tokenisation avec positions.

- Tokens : identifiants, littéraux (int, bool), mots-clés (let, rec, fun, if, then, else, match, with, true, false), symboles (->, *, +, -, =, <, >, (, ), [, ], ,, ;).
- Sortie : liste de `Token` avec `SourceSpan`.
- Pas de mutabilité ; lexer pur.

**Livrables** : `AstraLang.Lexer`, fonction `tokenize : string -> Result<Token list, LexError>`.

---

## Phase 3 : Parser

**Objectif** : Parser récursif descendant (ou combinator) vers AST non typé.

- Grammaire : expressions (app, binaire, unaire), let/let rec, fun, if, match, tuples.
- Priorités opérateurs : comparaison < arithmétique < logique.
- Associativité : gauche pour +, -, *, / ; droite pour -> (type et expr).
- Erreurs avec position.

**Livrables** : `AstraLang.Parser`, `parse : Token list -> Result<UntypedAst, ParseError>`.

---

## Phase 4 : Inférence de types (Algorithm W)

**Objectif** : AST typé + contraintes.

- **Unification** : `unify : Type * Type -> Result<Subst, UnifyError>` avec occurs-check.
- **Substitution** : application sur types et schémas.
- **Généralisation** : `generalize : TypeEnv -> Type -> TypeScheme` (variables libres → ∀).
- **Instanciation** : `instantiate : TypeScheme -> Type` (variables fraîches).
- **Algorithm W** : `infer : TypeEnv -> Expr -> Result<Type * Subst, InferError>` pour chaque forme d’AST.
- Environnement : map nom → TypeScheme.

**Livrables** : `AstraLang.TypeInference`, `inferProgram : UntypedAst -> Result<TypedAst, InferError>`.

---

## Phase 5 : IR (intermédiaire)

**Objectif** : AST typé → IR simplifiée.

- **ANF** : toute expression est soit une valeur atomique, soit let x = atom in rest.
- **Lambda lifting** : remonter les lambdas au top-level, passer les free vars en paramètres.
- **Closure conversion** : chaque fonction devient une closure (code + env) ; représentation explicite.

**Livrables** : `AstraLang.IR`, `astToIR : TypedAst -> IR.Program`.

---

## Phase 6 : Optimisations

**Objectif** : Passes sur IR.

- Constant folding : 1+2 → 3 dans IR.
- Beta-reduction simple : (fun x -> e) v → e[x:=v] quand pas d’effets.
- Dead code elimination : supprimer branches/blocs jamais atteints.
- Inline : fonctions simples (une seule occurrence ou petit corps).
- Unused variable elimination : supprimer let x = e quand x inutilisé.

**Livrables** : `AstraLang.Optimizer`, `optimize : IR.Program -> IR.Program`.

---

## Phase 7 : Compilation bytecode

**Objectif** : IR → bytecode.

- Instructions : PushInt, PushBool, Load, Store, Call, TailCall, Return, MakeClosure, Jump, JumpIfFalse, etc.
- Tables : constantes, noms de variables/indices, labels.
- Tail call : détecter position tail et émettre TailCall.
- Frames : sauvegarde IP et env pour les appels.

**Livrables** : `AstraLang.Compiler`, `compile : IR.Program -> Bytecode.Program`.

---

## Phase 8 : Runtime + VM + GC

**Objectif** : Exécution du bytecode.

- **Valeurs** : Int, Bool, Unit, Tuple(list), Closure(env, code).
- **Heap** : stockage des tuples et closures ; indices stables.
- **Stack** : pile de valeurs ; call stack (return address, frame).
- **GC** : mark-and-sweep sur heap (marquer depuis stack + champs des valeurs).
- **VM** : fetch-decode-execute ; gestion Load/Store (env), Call/TailCall/Return, MakeClosure.

**Livrables** : `AstraLang.Runtime` (valeurs, heap), `AstraLang.VM` (machine + GC).

---

## Phase 9 : CLI + REPL

**Objectif** : Interface utilisateur.

- Fichier `.astra` : lex → parse → infer → IR → optimize → compile → run.
- REPL : boucle read → parse (éventuellement multi-decl) → infer → afficher type → compile → run → afficher résultat.
- Flags : `--show-ir`, `--show-bytecode`, `--show-types`.

**Livrables** : `AstraLang.CLI`, exécutable `astralang`.

---

## Phase 10 : Qualité

- Tests unitaires : unification, inférence, VM. C# (AstraLang.Tests.CSharp) exécutés par xUnit ; F# (AstraLang.Tests) compilés.
- Messages d’erreur avec ligne/colonne.
- Documentation XML sur modules publics.

---

## Ordre des dépendances

```
Core
  ↑
Lexer → Parser (dépendent de Core)
  ↑
TypeInference (Core + Parser)
  ↑
IR (Core + TypeInference)
  ↑
Optimizer (IR)
  ↑
Compiler (IR → Bytecode défini dans Core)
  ↑
Runtime + VM (Core, Bytecode)
  ↑
CLI (tout)
```
