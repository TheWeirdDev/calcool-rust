# Calcool-Rust

My calcool cli app and library re-written in rust.
Calcool uses a pratt parser to parse and evaluate expressions

# How to run
Simple. Run this command:
```console
cargo run
```

# Examples
Simple expressions:
```python
2 * 3 + 4 * 8
```

Function calls:
```python
sin(90deg) ^ cos(45deg) + tan(2rad)
```

Variables:
```python
result = 12 * abs(-42)
result + 10e+3
```

Define functions:
```python
def f2c(f) = (f - 32)/1.8
f2c(70)
```

# License
Calcool is free software and it is licensed under GPL-3 or newer. For more info see [LICENSE](./LICENSE).