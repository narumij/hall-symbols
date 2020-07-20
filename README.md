# hall-symbols

Haskell Hall Symbols Library

## Quickstart

Make new stack project and move to project directory.

```shell
% stack new hall_symbols_repl
% cd hall_symbols_repl
```

Edit extra-deps part of stack.yaml like below.

```
extra-deps:
- matrix-as-xyz-0.1.1.1
- hall-symbols-0.1.0.4
- symmetry-operations-symbols-0.0.1.0
```

Then start repl.

```shell
% stack repl
```

Setup packages and load modules.

```haskell
repl> :set -package hall-symbols
repl> :set -package matrix-as-xyz
repl> :m Data.Matrix.AsXYZ Data.Matrix.SymmetryOperationsSymbols Crystallography.HallSymbols
```

Use like below.

```haskell
-- print General Positions.
repl> prettyXYZ <$> fromHallSymbols' "C -2yc"
 ["x,y,z","x+1/2,y+1/2,z","x,-y,z+1/2","x+1/2,-y+1/2,z+1/2"]

-- print Generators
repl> prettyXYZ <$> fromHallSymbols'' "C -2yc"
["x,y,z","x+1/2,y+1/2,z","x,-y,z+1/2"]

```

```haskell
-- print General Positions.
repl> fromMatrix' <$> fromHallSymbols' "C -2yc"
 []

-- print Generators
repl> fromMatrix' <$> fromHallSymbols'' "C -2yc"
[]

```

## References

1. Concise Space-Group Symbols http://cci.lbl.gov/sginfo/hall_symbols.html , See also : https://github.com/rwgk/sginfo

2. Space-Group Notation with an Explicit Origin
   S.R. Hall; Space-Group Notation with an Explicit Origin ; Acta Cryst. (1981). A37, 517-525

3. ITVB 2001 Table A1.4.2.7 Hall symbols http://cci.lbl.gov/sginfo/itvb_2001_table_a1427_hall_symbols.html

## License

See the [LICENSE](https://raw.githubusercontent.com/narumij/hall-symbols/master/LICENSE)
file in the repository.
