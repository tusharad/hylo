# AGENTS.md — hylo

This file is the primary orientation document for AI coding agents and contributors working on this codebase.
Read this fully before making any change. Update it when architectural decisions are made.

---

## What This Project Is

`hylo` is a type-safe Haskell ORM and query composition library exploring recursion schemes (hylomorphisms, anamorphisms, catamorphisms) and advanced type-level guarantees for SQL query synthesis, data mapping, and relational composition.

- **Language / Ecosystem**: Haskell (GHC 9.8+ / GHC 9.10+, Stack & Cabal)
- **Author**: Tushar Adhatrao (<tusharadhatrao@gmail.com>)
- **License**: MIT

---

## Formal Verification Requirement

**Every new feature or bug fix MUST follow the `formal-spec-verifier` skill protocol:**

1. Read `.agents/skills/formal-spec-verifier/SKILL.md`
2. Fill a System Law Spec from `.agents/skills/formal-spec-verifier/resources/system-law-template.yaml`
3. Write HSpec / QuickCheck properties FIRST (leave implementation body as `undefined` until contract and tests are in place)
4. Implement minimum code to satisfy the oracle
5. Run: `./.agents/skills/formal-spec-verifier/scripts/verify_gate.sh --haskell`

---

## Branching Strategy & Git Protocol

All agents and contributors working on this codebase MUST follow this branching model without exception:

### 1. Branch Roles & Hierarchy
- **`main`**: Protected production branch. Represents verified, stable releases tagged with SemVer (`vX.Y.Z`). **NEVER commit or push directly to `main`.**
- **`develop`**: Primary integration branch and default development target. All feature and bugfix branches originate here and merge back here.
- **Supporting Branches**:
  - `feat/<feature-name>`: Cut from `develop`, merged back to `develop` via PR.
  - `fix/<bug-id-or-description>`: Cut from `develop`, merged back to `develop`.
  - `release/vX.Y.Z`: Release stabilization branches cut from `develop` when preparing milestone releases; merged into `main` (with release tag) and back into `develop`.
  - `hotfix/<description>`: Emergency production fixes cut from `main`, merged into both `main` and `develop`.
  - `chore/<task>`: Documentation, toolchain, or CI/CD updates cut from `develop`.

### 2. Mandatory Agent Workflow
1. **Always base work on up-to-date `develop`**:
   ```bash
   git checkout develop
   git pull origin develop
   git checkout -b feat/<feature-name>
   ```
2. **Never commit directly to `main`** or leave feature work unmerged on detached/ad-hoc branches.
3. **Execute Formatting, Linting & Verification Gate before committing/merging**:
   ```bash
   make format-check   # fourmolu -m check src/ app/ test/
   make lint           # hlint src/ app/ test/
   stack test --pedantic
   ./.agents/skills/formal-spec-verifier/scripts/verify_gate.sh --haskell
   ```
4. **Use Conventional Commits**:
   - `feat:` for new capabilities
   - `fix:` for bug fixes
   - `refactor:` for code restructuring without behavior change
   - `test:` for test additions or updates
   - `docs:` for documentation updates
   - `chore:` for build/dependency/tool updates

---

## Code Quality & Formatting Rules

1. **Compiler strictness**: All code must compile cleanly with `--pedantic` (`-Wall -Werror` and strict warning flags in `package.yaml`). Avoid partial functions (`head`, `tail`, `fromJust`, `error`, `undefined`).
2. **Fourmolu formatting**: Always run `make format` or `fourmolu -m check` matching `fourmolu.yaml` (2-space indent, 100-column limit, leading commas and leading import/export lists).
3. **HLint cleanliness**: Always run `make lint` and address all HLint hints before committing.
4. **Cabal & Stack parity**: Whenever dependencies or exposed modules change in `package.yaml`, regenerate `.cabal` files and verify both `stack build` and `cabal check`.
5. **Preserve public API backwards compatibility**: Keep existing exported functions and data constructors compatible unless a breaking change is explicitly approved for a major version bump.

---

## Testing Rules

1. **Pure invariants**: Add `QuickCheck` property tests for pure query AST transformations, SQL compilation, algebraic laws, and type-level assertions.
2. **Boundary tests**: Write `HSpec` unit tests for database interactions, execution planning, and error handling.
3. **Pre-commit**: Install and verify pre-commit hooks (`pre-commit install`) before pushing changes.

---

## Key Lessons Learned & Common GHC Pitfalls

1. **Custom `HasField` Instances & Record Selectors (`GHC-43406`):**
   Never define named record field selectors on newtypes or wrapper types that implement an overlapping polymorphic `HasField field (Wrapper a) (QExpr ...)` instance (such as `NullableTable`). GHC automatically generates built-in `HasField` instances for record field labels, which triggers `[GHC-43406]: type 'field' could unify with one of the field labels of 'Wrapper'`.
   * **Rule:** Always define wrapper types using plain positional constructors:
     ```haskell
     -- CORRECT
     newtype NullableTable exprRecord = NullableTable exprRecord
     unNullableTable :: NullableTable exprRecord -> exprRecord
     unNullableTable (NullableTable r) = r

     -- FORBIDDEN (triggers GHC-43406 with polymorphic HasField instance)
     newtype NullableTable exprRecord = NullableTable { unNullableTable :: exprRecord }
     ```

2. **Type Families & Ambiguous Type Variables (`GHC-83865`):**
   Non-injective open type families like `LiftMay a` and `Col ctx a` cannot determine ambiguous type variables when applied to polymorphic terms (like `colExpr "table" "col"`).
   * **Rule:** Always ensure expressions passed to type family operators have concrete type annotations (`colExpr ... :: QExpr Text`) or bind records to concrete type aliases (`type User = UserT 'Result`, `type UserExpr = UserT 'Expr`).

3. **HLint Functor Law Warning in QuickCheck Property Suites:**
   When writing algebraic tests verifying the Functor identity and composition laws (`fmap id == id` and `fmap (f . g) == fmap f . fmap g`), HLint flags them as redundant simplifications.
   * **Rule:** Add `{-# HLINT ignore "Functor law" #-}` to the specific test module so full algebraic verification is retained without breaking lint gates.

4. **CI Fourmolu Scope & Root Haskell Files (`Setup.hs`):**
   `run-fourmolu` in GitHub Actions and local `make format-check` must cover all tracked Haskell files, including root `Setup.hs` and example files.
   * **Rule:** Always format `Setup.hs` alongside `src/`, `app/`, and `test/` using `make format`. Do not leave root or script Haskell files unformatted.

5. **NoFieldSelectors & Field Name Collisions with Prelude (`GHC-87543`):**
   When defining records with common domain field names like `id`, `head`, `tail`, or `type`, GHC generates top-level record selector functions by default that collide with `Prelude` identifiers (e.g., `Prelude.id`).
   * **Rule:** Always enable `{-# LANGUAGE NoFieldSelectors #-}` and `{-# LANGUAGE DuplicateRecordFields #-}` on modules defining HKD table records. Use `OverloadedRecordDot` (`user.id`) rather than bare selector functions (`id user`).

6. **Standalone Deriving on Higher-Kinded Records (`GHC-48406`):**
   Using standalone deriving to instantiate higher-kinded records over type-level constants (e.g., `deriving instance Eq (UserT 'Result)`) triggers `GHC-48406: All instance types must be of the form (T a1 ... an) where a1 ... an are distinct type variables`.
   * **Rule:** Always add `{-# LANGUAGE FlexibleInstances #-}` to the module whenever using standalone deriving over instantiated type family or promoted data kind arguments.

7. **HSpec and Multiple QuickCheck Properties in a Single `it` Block (`GHC-83865`):**
   In HSpec, `Property` is not a monad (`m a`). Attempting to sequence multiple `property $ \x -> ...` calls inside a single `it "..." $ do` block causes GHC type mismatch errors (`Couldn't match expected type ‘m0 a0’ with actual type ‘Property’`).
   * **Rule:** Always place each QuickCheck `property` within its own distinct `it` block.

8. **Applicative Identity Law & HLint `Use <$>` Suggestion:**
   Verifying the Applicative identity law `pure id <*> v == v` causes HLint to suggest replacing `pure id <*>` with `(id <$>)`.
   * **Rule:** Add `{-# HLINT ignore "Use <$>" #-}` to algebraic test modules to preserve explicit verification of the Applicative `<*>` identity law without tripping CI lint gates.

9. **Breaking Circular Module Graphs via `PolyKinds`:**
   When defining diagnostic assertion type families in `TypeError` that reference phantom keys (`Id table a`) or expressions (`QExpr a`), avoid importing high-level modules (like `Context`) from low-level foundation modules (`Key`). By enabling `{-# LANGUAGE PolyKinds #-}` on `newtype Id table a = Id { unId :: a }`, `table` can quantify over `Type`, `Ctx -> Type`, or any kind without `Key.hs` needing to import `Context.hs`. This keeps the module dependency graph acyclic (`Key -> TypeError -> Expr / Query -> Core`).

10. **Rank-N Equality Constraints for Deferred Type Errors (`should-not-typecheck`):**
   When asserting custom compiler diagnostics at runtime with `-fdefer-type-errors`, GHC evaluates deferred type errors at the call site if passed to a plain function parameter `a`. To prevent premature uncaught `TypeError` crashes before entering the exception handler, test helper assertions must quantify over an equality constraint `(() ~ () => a)` (matching `Test.ShouldNotTypecheck`'s RankN signature: `shouldNotTypecheckWith :: NFData a => String -> (() ~ () => a) -> Expectation`).

11. **`PackageImports` for Local Wrapper Modules:**
   When authoring test helper modules with the same module name as an external Hackage dependency (e.g., `Test.ShouldNotTypecheck`), use `{-# LANGUAGE PackageImports #-}` and `import qualified "should-not-typecheck" Test.ShouldNotTypecheck as SNT` to avoid cyclical self-imports.

12. **Proxy Dictionaries for Custom TypeError Assertions:**
   To evaluate closed type families that emit `TypeError`, define companion typeclasses with `Proxy` dictionary methods (`comparableDict :: Proxy a -> Proxy b -> ()`, `writableDict :: Proxy mode -> ()`). Forcing these dictionary methods in operator AST constructors or test assertions triggers the GHC diagnostic deterministically without ambiguous type application issues.

13. **HSpec `shouldContain` Type Signature & Polymorphic SQL Rendering:**
   `Test.Hspec.shouldContain` requires `[a]` (`String`), and will not typecheck when applied directly to `Data.Text.Text`. Rendering functions (`renderSQL`) should be typed with `(Projectable a, IsString s) => Query db a -> s` to unify automatically with `String` in test assertions and `Text` in production adapters without requiring `T.unpack` at call sites.

14. **HLint Monad Law Warnings in Algebraic Property Suites:**
   Verifying monadic laws (`pure n >>= f == f n`, `m >>= pure == m`, `\x -> f x >>= g == f >=> g`) causes HLint to flag them as redundant simplifications. Add `{-# HLINT ignore "Monad law, left identity" #-}`, `{-# HLINT ignore "Monad law, right identity" #-}`, and `{-# HLINT ignore "Use >=>" #-}` to property test suites verifying monad instances.

15. **Duplicate Exports with Type Constructor Records (`GHC-47854`):**
   Exporting a data/newtype constructor with wildcards (e.g. `Query (..)`) alongside its individual record fields or methods (e.g. `runQuery`) triggers `[GHC-47854] [-Wduplicate-exports]`. If `Type (..)` is exported, omit standalone mentions of its field accessors.

16. **Ambiguous Backend Type Variable in Dialect Compilers (`GHC-39999`):**
   When defining backend-specific SQL compilers (e.g. `Hylo.PG.Compiler.SQL`), leaving `renderSQL` polymorphic over `Query db a` causes GHC to complain that `db` is ambiguous whenever backend capability constraints (e.g. `SupportsLateralSubqueries db`) are applied in query blocks without explicit type annotations.
   * **Rule:** Specialized dialect compilers must fix their backend type parameter concretely (e.g. `renderSQL :: (Projectable a, IsString s) => Query Postgres a -> s`).

17. **Monotonic Scope and Lateral Index Propagation Across Nested Subqueries:**
   When executing nested subqueries in `many`, allocating lateral aliases using the outer state's initial index (`latAlias = "lat_" <> show (qsNextLateral s)`) causes nested subqueries to collide with the outer query's alias (e.g. both using `"lat_0"`).
   * **Rule:** The outer lateral alias must be allocated using the lateral counter *after* the child subquery has completed (`latAlias = "lat_" <> show (qsNextLateral childSt)`), ensuring monotonic, collision-free lateral aliases across arbitrarily deep nested graphs.

18. **Redundant Constraint Warnings for Marker Classes:**
   Capability marker classes with no methods (e.g. `class SupportsLateralSubqueries db`) trigger `[-Wredundant-constraints]` when used in function signatures.
   * **Rule:** Include a `Proxy` dictionary method (`lateralSubqueriesDict :: Proxy db -> ()`) and evaluate it with `seq` in the guarded combinator (`many`), ensuring the capability constraint is genuinely enforced and eliminating compiler warnings.

19. **Record Update Syntax for Extensible State Structures (`QueryState`):**
   When updating state structures like `QueryState` with new fields (e.g. `qsGroupBy`, `qsHaving`), never construct `QueryState` using positional syntax (`QueryState s l r p lat`). Positional construction causes downstream compiler breaks whenever fields are added or rearranged.
   * **Rule:** Always use record update syntax based on baseline default state:
     ```haskell
     -- CORRECT
     let childInit = initialQueryState { qsNextScope = qsNextScope s, qsNextLateral = qsNextLateral s }
     ```

20. **Static Detection of Ungrouped Columns in Aggregations (`E004`):**
   In aggregate query blocks (`aggregate $ do { groupBy u.orgId; pure (...) }`), tracking group keys at the phantom type level (`AggQuery key a`) and validating projected elements with a multi-parameter type class (`ValidAggProjection key a`) allows static detection of ungrouped scalar column selections (`CheckGrouped` / `AssertGrouped` emitting `[Hylo Error E004]`), while allowing any aggregate expression (`AggExpr a`) or composite tuple combinations to pass without ambiguous type variables.

21. **Numeric Literal Formatting in SQL Assertions:**
   `Double` literals format via `show` as `"1000.0"`, whereas integer types (`Int64`) format as `"1000"`. When writing tests that assert exact SQL text matching (e.g., `HAVING (SUM(...) > 1000)`), ensure test column types and literal types align with expected test SQL assertions by using `Int64`.

22. **Window Function Safety & PostgreSQL Error 42P20 Elimination:**
   PostgreSQL error `42P20` (`window functions are not allowed in WHERE`) is eradicated statically at compile time. `over` constructs a distinct `WindowExpr a` type rather than a standard `QExpr a`. Because `where_` requires `QExpr Bool` and `WindowExpr` does not implement `ToQExpr` or standard boolean comparison operators (`==.`, `>.`, etc.), any attempt to filter by a window function directly inside `where_` fails GHC type checking at compile time.

23. **Capability Class Co-location & Orphan Prevention (`GHC-90177`):**
   Backend capability marker classes (such as `SupportsWindowFunctions db`) should be defined in the same dialect module as their primary instance (`instance SupportsWindowFunctions Postgres` in `Hylo.PG.Compiler.Window`), rather than the core agnostic query module. This prevents orphan instance warnings (`[-Worphans]`) under `-Wall -Werror`.

24. **Broad `Projectable` Tupling & Analytical Projections:**
   Analytical window queries often project wide result tuples (e.g., entity primary keys plus multiple ranked, lagged, and framed window expressions). Provide `Projectable` tuple instances up to 8-tuples (`(a, b, c, d, e, f, g, h)`) in `Hylo.Core.Query.AST` so that complex multi-window select projections compile seamlessly without requiring nested pairs or intermediate records.

25. **Ambiguous Type Variables with Overloaded Comparison Tokens (`GHC-18872`, `GHC-43085`):**
   When defining sentinel tokens for comparisons like `isNull` in `where_ (e.managerId ==. isNull)`, never type `isNull` as a polymorphic class method `forall a. IsNullPattern a => a` if the comparison class `EqOp e1 e2` relies on overlapping instances. When `e2` is polymorphic, GHC cannot resolve overlapping instances deterministically ("the choice depends on the instantiation of e2").
   * **Rule:** Give `isNull` a concrete singleton token type (`data IsNullToken = IsNullToken; isNull = IsNullToken`) and declare a dedicated instance `instance EqOp (QExpr (Maybe a)) IsNullToken where e ==. _ = UnaryOpExpr "IS NULL" e`.

26. **Discarded Polymorphic Action in Monadic Do-Notation (`GHC-81995` / `-Wunused-do-bind`):**
   When overloading a combinator like `orderBy` across different targets (`WindowDef -> WindowDef` and `Query db ()`), declaring `instance OrderBy (Query db ())` causes GHC inside do-notation blocks to type `orderBy [...]` as `m a` (with an unconstrained result type variable `a`), which triggers `[-Wunused-do-bind]: A do-notation statement discarded a result of type ‘GHC.Types.ZonkAny’`.
   * **Rule:** Constrain the instance with an equality constraint: `instance (a ~ ()) => OrderBy (Query db a)`. This forces GHC to immediately unify the discarded result with `()` in monadic do-blocks, eliminating `-Wunused-do-bind`.

27. **Monotonic Scope Allocation Across Recursive CTE Steps:**
   In recursive CTEs, scope IDs must be propagated monotonically from the base step into the recursive step and then into the outer query (`qsNextScope = qsNextScope prevStep`). Expecting the recursive step relation to start at alias index 0 (e.g. `"org_tree_0"`) fails because earlier relations in the base step and recursive step already consumed indices 0 and 1. Tests and SQL matchers must reflect the strictly monotonic scope allocation (`"org_tree_2"`, `"org_tree_3"`).

28. **Overloaded String Literal Ambiguity with Generic Operators (`GHC-18872`, `GHC-18042`):**
   When comparing a typed column (e.g. `job.status :: QExpr Text`) against an overloaded literal (`lit "pending"`), `lit :: Show a => a -> QExpr a` produces `(IsString a, Show a) => QExpr a`. In standard Haskell, defaulting rules only default `Num` classes; `IsString` remains unconstrained, causing GHC to fail with ambiguous type variable errors (`Couldn't match type 'CheckComparable Text b0' with 'True'`).
   * **Rule:** When authoring test modules with unadorned string literals inside `lit "..."` comparisons, enable `{-# LANGUAGE ExtendedDefaultRules #-}` and declare `default (Text, Int64, Double)` at top-level, alongside `{-# OPTIONS_GHC -Wno-type-defaults #-}` to allow GHC to default string literals to `Text` without triggering `-Werror=type-defaults`.

29. **Higher-Kinded Lambda Argument Inference in Mutation Combinators:**
   When designing higher-order mutation combinators like `insertOnConflict schema row target (\current excluded -> [...])`, avoid making the lambda argument polymorphic via a typeclass `OnConflictHandler table handler`. Because GHC cannot infer the types of lambda binders `current` and `excluded` before resolving the typeclass, GHC reports ambiguous type variables (`r0`, `r1`).
   * **Rule:** Concretely type the lambda callback in the combinator signature: `(table 'Expr -> table 'Expr -> [Assignment])`. This immediately supplies the types of `current` and `excluded` to GHC's bidirectional typechecker, eliminating binder ambiguity completely.

30. **Preserving Monomorphic Backend Parameter on Dialect SQL Compilers:**
   Attempting to overload dialect rendering functions like `renderSQL` across queries and mutation actions via an unconstrained typeclass (`class RenderSQL target`) destroys GHC's ability to propagate the concrete dialect marker (`Postgres`) down into local query bindings (e.g. `let q = ...; renderSQL q`). This leads to open-world type ambiguity errors on backend capabilities and spurious `-Wunused-do-bind` warnings on polymorphic statements.
   * **Rule:** Keep `renderSQL :: (Projectable a, IsString s) => Query Postgres a -> s` monomorphic over the dialect marker `Postgres`. Model mutation statements (like `insertOnConflict`) as monadic actions that execute within `Query db (UpsertAction table)` and record their AST representation in `QueryState (qsUpsert)`.

31. **Local Formatting Verification Prior to Git Push (`fourmolu`):**
   CI strictly enforces `fourmolu --mode check` across all Haskell source trees (`src`, `app`, `test`, `Setup.hs`). Always run `fourmolu --mode check $(git ls-files 'src/**/*.hs' 'test/**/*.hs' 'app/**/*.hs' 'Setup.hs')` locally before committing to prevent PR CI format check failures. If discrepancies arise (e.g. multiline comment style or binary operator placement), apply `fourmolu --mode inplace <files>` immediately.

32. **Exhaustive Base Cases for Recursive Property Simulation Helpers:**
   When defining recursive simulation helper functions in QuickCheck properties (such as simulating chunked pagination or stream traversal over arbitrary lists `paginate pool mCur`), failing to handle the empty pool case (`paginate [] _ = []`) when `mCur` is `Nothing` causes infinite recursion when QuickCheck tests empty inputs (`[]`), hanging the test runner indefinitely.
   * **Rule:** Always define an unconditional base case for empty collections (`paginate [] _ = []` or checking `if null page then [] else ...`) at the head of recursive simulation helpers before matching on parameter states.

33. **Prepositive vs. Postpositive Qualified Imports (`GHC-87491`):**
   Postpositive qualified imports (`import Data.Text qualified as T`) are rejected by GHC with `[GHC-87491]` unless the `ImportQualifiedPost` language extension is enabled.
   * **Rule:** Always use standard Haskell2010 prepositive qualified syntax (`import qualified Data.Text as T`) unless `{-# LANGUAGE ImportQualifiedPost #-}` is explicitly turned on at module header.

34. **HLint and Restricted Functions in Library Code (`errorWithoutStackTrace` vs `error`/`undefined`):**
   HLint enforces strict `Avoid restricted function` on `error` and `undefined`. While test suites can disable this via `{-# HLINT ignore "Avoid restricted function" #-}`, placing HLINT ignore pragmas in library modules (`src/`) triggers GHC's `[-Wunrecognised-pragmas, Werror=unrecognised-pragmas]` under `-Wall -Werror` / `--pedantic` unless `-Wno-unrecognised-pragmas` is enabled.
   * **Rule:** In library source code (`src/`), use `errorWithoutStackTrace` from `Prelude` instead of `error` or `undefined` for unreachable states or simulated AST branches, which satisfies HLint without requiring pragma ignores or compiler warning suppressions.

35. **Phantom Mode Type Ambiguity in Operational Transaction Do-Blocks (`AssertWritable` & Injective Deduction):**
   When mutation actions (`insert`, `update`, `delete`) in a transaction monad `Tx db (mode :: TxMode) a` are statically guarded by a closed type family assertion (`AssertWritable mode` via `CheckWritable mode ~ 'True`), do-blocks without top-level type signatures leave `mode` unconstrained. Because closed type families in GHC are not injective by default, GHC cannot infer that `CheckWritable mode ~ 'True` requires `mode = 'ReadWrite`, causing `[GHC-18872] The type variable 'mode' is ambiguous`.
   * **Rule:** When evaluating transactional expressions via pure test runners or runners parameterized by runtime modes (`runTx pool ReadWrite`), ensure transaction do-blocks or let-bindings have explicit type annotations `tx :: Tx Postgres 'ReadWrite a` so that GHC can immediately verify `CheckWritable 'ReadWrite ~ 'True` at compile time without ambiguity.

36. **Fourmolu Operator Placement and Dollar Chaining across Versions (0.17 vs 0.21):**
   Fourmolu 0.21 formats chained `$` operators across multiline expressions with leading `$` operators (`\n  $ ...`), whereas CI's Fourmolu 0.17 enforces trailing `$` operators (`... $\n  ...`), causing CI format check failures.
   * **Rule:** Avoid multi-operator `$` chaining over multiline expressions. Use single `$` applications or direct record constructor arguments (e.g. `withUniqueKey name $ emptyChangeset User { ... }` rather than `withUniqueKey name $ emptyChangeset $ User { ... }`), which format consistently across both Fourmolu 0.17 and 0.21.

37. **`package.yaml` Authority Over `hylo.cabal` (`hpack` Synchronization):**
   In projects managed by `hpack`, `hylo.cabal` is generated automatically from `package.yaml`. Adding new package dependencies (e.g. `stm >= 2.5 && < 3`) directly to `hylo.cabal` will be overwritten whenever `stack` or `hpack` runs.
   * **Rule:** Always add new package dependencies directly to the top-level `dependencies` section in `package.yaml` before running `stack build` or `stack test`.

38. **State Preservation Across Monadic Failures in Transaction Monads (`TxAction`):**
   When modeling state in a transaction monad `Tx` that supports rollback hooks, savepoints, or mutation logs, defining `TxAction` as `TxState -> Either TxError (a, TxState)` discards all accumulated state when an action fails (`Left err`). This prevents `executeRollbackHooks`, retry counters, or diagnostic logs registered before the failure from executing upon abort.
   * **Rule:** Define the state transition function as `TxState -> (Either TxError a, TxState)`. This ensures `executeTx` returns the final state alongside `Either TxError a`, allowing abort handlers to execute accumulated rollback hooks and outer retry loops to preserve attempt counters.

39. **`Show` Constraint on HSpec Assertion Helpers (`shouldSatisfy` vs `shouldBe`):**
   `shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> Expectation` requires a `Show` instance for the inspected expression. When verifying results of higher-kinded types or AST records that omit `Show` (such as `AccountT 'Expr`), `res `shouldSatisfy` isRight` fails compilation with `[GHC-39999] No instance for (Show (AccountT 'Expr))`.
   * **Rule:** Use `isRight res `shouldBe` True` or pattern-match explicitly instead of `shouldSatisfy` when testing values lacking `Show` instances.

40. **`MonoLocalBinds` with `AssertWritable mode` in Operational Monads (`GHC-62412`):**
   When an operational mutation combinator (such as `insertMany`) is constrained by `AssertWritable mode`, GHC emits `[-Wsimplifiable-class-constraints]` because `AssertWritable mode` matches an instance head with an equality constraint `(CheckWritable mode ~ 'True) => AssertWritable mode`. Under `-Wall -Werror` / `--pedantic`, this warning causes build failures.
   * **Rule:** Enable `{-# LANGUAGE MonoLocalBinds #-}` and `{-# LANGUAGE PolyKinds #-}` on any module declaring functions with `AssertWritable mode` signatures.

41. **`renderBulkInsertSQL` Over Polymorphic String Targets (`IsString s`):**
   When assertions like HSpec's `shouldContain` are invoked directly on the output of SQL renderers without explicit type signatures, typing the renderer as `TableSchema table -> [record] -> Text` leads to a type mismatch error (`Couldn't match expected type '[Char]' with actual type 'Text'`) because `shouldContain` operates on lists (`[a]`).
   * **Rule:** Type SQL rendering combinators with `(IsString s) => TableSchema table -> [record] -> s` so they infer `Text`, `String`, or `ByteString` seamlessly based on caller context.

42. **Monadic State Mutation Order in Bulk Operations (`foldl'` Prepend Symmetry):**
   In monadic transaction stores that prepend inserted keys (`(tName, key) : txStore st`), sequential insertion `insert r1 >> insert r2` accumulates records in reverse order (`[r2, r1]`). Bulk operations (`insertMany [r1, r2]`) must accumulate new records via `reverse [(tName, k) | k <- keys] ++ txStore st` to preserve the exact Law 1 FinalState list ordering invariant.

43. **TypeApplication Whitespace Parsing Edge Cases with Type Literals (`GHC-95781`):**
   In GHC 9.10+, writing `decodeBinaryVector @ "test-model" @3` with whitespace between `@` and the type string literal causes GHC's parser to interpret `@` as an infix operator application (`(@) "test-model" @3`), failing compilation with `[GHC-95781] Cannot apply expression of type 'a0' to a visible type argument '3'`.
   * **Rule:** Avoid whitespace after `@` when using type application with literal types (e.g. `@"test-model"`). Better yet, use explicit type annotations `(decodeBinaryVector bytes :: Either String (Embedding "test-model" 3))` in tests and call sites to ensure completely unambiguous type inference.

44. **`ConstraintKinds` Requirement for Constraint Type Synonyms (`GHC-75844`):**
   Defining type synonyms that alias constraint families or classes (such as `type AssertSameEmbedding m1 d1 m2 d2 = AssertMatchingVectors m1 d1 m2 d2`) requires the `{-# LANGUAGE ConstraintKinds #-}` extension. Without it, GHC rejects the declaration with `[GHC-75844] Illegal constraint synonym of kind: Constraint`.
   * **Rule:** Always enable `{-# LANGUAGE ConstraintKinds #-}` at the top of any module declaring type synonyms of kind `Constraint`.

45. **Fourmolu `it` Expression Formatting across Versions (`$ do` vs `$` Chaining):**
   In Fourmolu 0.21, writing `it "description" $ shouldNotTypecheckWith ...` without `do` formats the `$` operator on a newline (`it "description" \n $ shouldNotTypecheckWith ...`), which Fourmolu 0.17 (used in GitHub Actions CI) rejects as unformatted.
   * **Rule:** Always write `it "description" $ do \n shouldNotTypecheckWith ...` in HSpec test suites. Including `$ do` ensures identical, idempotent formatting behavior across all Fourmolu versions.

46. **Natural Representation in `GHC.TypeNats` (`Natural` vs `Integer`):**
   `GHC.TypeNats.natVal` returns `GHC.Natural.Natural`, not `Integer` or `Int`. Attempting to return `natVal` directly from functions typed as `embeddingDim :: Embedding m d -> Integer` fails with a type mismatch error.
   * **Rule:** Explicitly convert `Natural` via `toInteger (natVal proxy)` or `fromIntegral (natVal proxy)` when exposing integer dimensions.

47. **Fourmolu Infix Operator Sequences across Multiline Expressions (` `seq` ` Chaining):**
   Writing chained infix operators before a `let` block (e.g. `hybridSearchDict (Proxy @db) `seq` natVal (Proxy @dim) `seq` let ...`) triggers different indentation decisions between Fourmolu 0.21 and Fourmolu 0.17 in CI, leading to format check failures.
   * **Rule:** Avoid placing infix operators like `` `seq` `` in leading positions before `let` bindings. Instead, place dictionary evaluations inside the monadic return block `Query $ \s -> hybridSearchDict (Proxy @db) `seq` (res, s')`, keeping `let ... in` blocks clean and uniformly indented.

48. **Redundant Type Constraints on Phantom Types (`KnownNat`, `KnownSymbol`):**
   When phantom type parameters (such as `model :: Symbol` and `dim :: Nat` on `Embedding model dim`) are only used to guarantee compile-time type compatibility between a table column `table 'Expr -> QExpr (Embedding model dim)` and an input embedding `Embedding model dim`, adding `KnownSymbol model` or `KnownNat dim` to the combinator's signature is rejected under `-Wall -Werror` / `--pedantic` with `[-Wredundant-constraints]`.
   * **Rule:** Do not add `KnownSymbol` or `KnownNat` constraints to query combinators unless runtime reflection (`symbolVal`, `natVal`) is actually performed within the function body. Phantom type compatibility between two AST nodes is checked purely at compile time by GHC's unification without typeclass dictionary overhead.

49. **`MonoLocalBinds` Implication by `GADTs` in HLint:**
   When `{-# LANGUAGE GADTs #-}` is enabled on a module, HLint flags `{-# LANGUAGE MonoLocalBinds #-}` as redundant (`Note: Extension MonoLocalBinds is implied by GADTs`).
   * **Rule:** Omit `{-# LANGUAGE MonoLocalBinds #-}` when `{-# LANGUAGE GADTs #-}` is already enabled on the module to satisfy HLint.

50. **Explicit `Show` Instance for Key Wrappers (`Id table a`) in SQL AST Rendering:**
   Deriving `Show` automatically for wrapper types like `newtype Id (table :: (* -> *) -> *) a = Id { unId :: a }` produces output in Haskell record syntax (e.g. `Id {unId = 42}`). When values of type `Id` are passed to SQL literal combinators (`lit currentOrg`), rendered SQL queries or AST strings end up containing `"org_id" = Id {unId = 42}`, which fails SQL parsing or oracle equality checks.
   * **Rule:** Explicitly define `instance Show a => Show (Id table a) where showsPrec p (Id a) = showsPrec p a` so that identifier wrappers serialize directly as their underlying scalar literal values (`42` or `"org_123"`).

51. **`aeson-qq` Interpolation Syntax Limitation on JSON Object Keys:**
   The `aesonQQ` QuasiQuoter supports variable interpolation via `#{var}` only in JSON value positions, not in JSON object key positions (e.g. `[json| { #{colName}: val } |]` causes a compile-time quasi-quoter parsing error).
   * **Rule:** When generating JSON payloads with dynamic property names (such as QuickCheck property tests generating arbitrary column filter payloads), construct JSON objects programmatically using `Aeson.object [(Key.fromText colName, val)]` or `KeyMap.singleton` instead of attempting string interpolation inside the quasi-quoter.

52. **HLint Flagging of `for_` Traversing `Maybe` in Monadic AST Builders:**
   When building AST state conditionally based on optional fields (e.g. `queryLimit :: Maybe Int`), writing `for_ (queryLimit q) $ \l -> modify ...` triggers HLint's suggestion to use `case` matching or `whenJust`.
   * **Rule:** Use explicit pattern matching `case queryLimit q of Just l -> modify ...; Nothing -> pure ()` or `whenJust` rather than `for_` over `Maybe` in monadic AST manipulation functions to avoid HLint warnings under `--pedantic`.












