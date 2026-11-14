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


