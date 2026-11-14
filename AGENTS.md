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







