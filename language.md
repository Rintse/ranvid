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

## Generate (A)

Example value: `A = Double`

A term: `0.1`

**OR***

```
order :: (Double, Double) -> (Double, Double) =
lambda p -> 
    if fst p < snd p
        then (fst p, snd p)
        else (snd p, fst p)
```
A term: 
```
fst (
    order ( 
        ( lambda x -> 
            ( (x * x4) % x5, (x * x5) % x4 ) 
        ) 0.2
    ) 
)
```


## Generate (A -> B)

Example values: `A = Double, B = Double`

A term: `lambda x -> x + 1`  
B term: `0.2`

***OR***

A term: 
```
(lambda f -> 
    lamba x -> 
        if x4 > 0 
            then f x 
            else f (f x) 
) (lambda x -> x + 1)
```
Goes to:
```
lamba x -> 
    if x4 > 0 
        then (lambda x -> x + 1) x
        else (lambda x -> x + 1) ((lambda x -> x + 1) x) 
```
B term: `0.2`

# Interpreter
TODO
