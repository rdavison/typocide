<img alt="typocide" src="https://raw.githubusercontent.com/rdavison/typocide/main/typocide.png" width="200" height="239" />

# Typocide
Where Typos Meet Their Demise!

# Prerequisites
Requires opam: https://github.com/ocaml/opam

# Setup (only have to do this once)
```bash
cd typocide
opam switch create . 5.1.0 
eval $(opam env) # 
opam install . --deps-only
```

# Usage
Once you've done the setup, you can run by doing:
```bash
make run
```
or if you don't have make, you can also do
```bash
opam exec -- dune exec bin/main.exe
```
