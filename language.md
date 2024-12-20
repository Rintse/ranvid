# Language
<style> th { display: none; } </style>

With $v$, $w$ values and $t$, $s$, $r$ terms.

## Values
- $v = n \in \mathbb{N}$ `::` **`Int`**
- $v = f \in \mathbb{R}$ `::` **`Double`**
- $v = b \in \{ \mathtt{true}, \mathtt{false} \}$ `::` **`Bool`**
- $v = (t, s)$ `::` **`t::A,s::B | A x B`**
- $v = Left t$ `::` **`t::A | A + B`**
- $v = Right t$ `::` **`t::B | A + B`**

## Terms
Arithmetic operators
||||||
|-|-|-|-|-|
| `-` $t$ | `sqrt` $t$ | `sin` $t$ | `cos` $t$ | `exp` $t$
| $t$ `*` $s$ | $t$ `/` $s$ | $t$ `%` $s$ | $t$ `+` $s$ | $t$ `-` $s$

Boolean operators
||||
|-|-|-|
$t$ `and` $s$ | $t$ `or` $s$ | `not` $t$

Products/coproduts

Other
- `(` $t$ `,` $s$ `)`
- `fst` $t$ 
- `snd` $t$ 
- `if` $t$ `then` $s$ `else` $r$

# Generator
Algorithm to generate terms of some requested type $Gen(T) \rightarrow t$.
Ask for term of type `Double` $\times$ (`Bool` $\times$ `Int`):
1. Parse type into type tree:  
    ```
    X ──> Double
    └───> + ──> Bool
          └───> Int
    ```
2. Find the term formers that create terms of the shape $A \times B$: `(t,s)`
3. Do $t = Gen($**`Double`**$)$, $s = Gen($**`Double`** $\times$ **`Bool`**$)$
4. Construct `(t, s)`


# Interpreter
TODO
