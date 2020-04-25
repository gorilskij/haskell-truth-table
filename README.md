# Truth Table Generator

### Usage

Main.hs:
```haskell
main = do
    let s = "((a => b) & (b => a)) <=> (a <=> b)"
    putStrLn (table s)
    return ()
```

output:
```
a b - ((a => b) & (b => a)) <=> (a <=> b)
F F - T
F T - T
T F - T
T T - T
```

Main.hs:
```haskell
main = do
    let s = "a & b | c & you_can_have_long_names_too => with-underscores-or-dashes"
    putStrLn (table s)
    return ()
```

output (note automatic parenthesis wrapping):
```
a b c you_can_have_long_names_too with-underscores-or-dashes - ((a & b) | (c & you_can_have_long_names_too)) => with-underscores-or-dashes
F F F F  -   -   -   -   -   -    F  -   -   -   -   -   -   - T
F F F F  -   -   -   -   -   -    T  -   -   -   -   -   -   - T
F F F T  -   -   -   -   -   -    F  -   -   -   -   -   -   - T
F F F T  -   -   -   -   -   -    T  -   -   -   -   -   -   - T
F F T F  -   -   -   -   -   -    F  -   -   -   -   -   -   - T
F F T F  -   -   -   -   -   -    T  -   -   -   -   -   -   - T
F F T T  -   -   -   -   -   -    F  -   -   -   -   -   -   - F
F F T T  -   -   -   -   -   -    T  -   -   -   -   -   -   - T
F T F F  -   -   -   -   -   -    F  -   -   -   -   -   -   - T
F T F F  -   -   -   -   -   -    T  -   -   -   -   -   -   - T
F T F T  -   -   -   -   -   -    F  -   -   -   -   -   -   - T
F T F T  -   -   -   -   -   -    T  -   -   -   -   -   -   - T
F T T F  -   -   -   -   -   -    F  -   -   -   -   -   -   - T
F T T F  -   -   -   -   -   -    T  -   -   -   -   -   -   - T
F T T T  -   -   -   -   -   -    F  -   -   -   -   -   -   - F
F T T T  -   -   -   -   -   -    T  -   -   -   -   -   -   - T
T F F F  -   -   -   -   -   -    F  -   -   -   -   -   -   - T
T F F F  -   -   -   -   -   -    T  -   -   -   -   -   -   - T
T F F T  -   -   -   -   -   -    F  -   -   -   -   -   -   - T
T F F T  -   -   -   -   -   -    T  -   -   -   -   -   -   - T
T F T F  -   -   -   -   -   -    F  -   -   -   -   -   -   - T
T F T F  -   -   -   -   -   -    T  -   -   -   -   -   -   - T
T F T T  -   -   -   -   -   -    F  -   -   -   -   -   -   - F
T F T T  -   -   -   -   -   -    T  -   -   -   -   -   -   - T
T T F F  -   -   -   -   -   -    F  -   -   -   -   -   -   - F
T T F F  -   -   -   -   -   -    T  -   -   -   -   -   -   - T
T T F T  -   -   -   -   -   -    F  -   -   -   -   -   -   - F
T T F T  -   -   -   -   -   -    T  -   -   -   -   -   -   - T
T T T F  -   -   -   -   -   -    F  -   -   -   -   -   -   - F
T T T F  -   -   -   -   -   -    T  -   -   -   -   -   -   - T
T T T T  -   -   -   -   -   -    F  -   -   -   -   -   -   - F
T T T T  -   -   -   -   -   -    T  -   -   -   -   -   -   - T
```