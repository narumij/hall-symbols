# hall-symbols

Haskell Hall Symbols Library

## Quickstart

```shell
% stack repl
```

```haskell
-- prepare
repl> :set -package matrix-as-xyz
repl> :m Data.Matrix.AsXYZ
repl> :l Crystallography.HallSymbols

-- print general positions
repl> prettyXYZ <$> fromHallSymbols' "C -2yc"

 ["x,y,z","x+1/2,y+1/2,z","x,-y,z+1/2","x+1/2,-y+1/2,z+1/2"]

-- print generators
repl> prettyXYZ <$> fromHallSymbols'' "C -2yc"

["x,y,z","x+1/2,y+1/2,z","x,-y,z+1/2"]
```

## References

1. Concise Space-Group Symbols http://cci.lbl.gov/sginfo/hall_symbols.html , See also : https://github.com/rwgk/sginfo

2. Space-Group Notation with an Explicit Origin
   S.R. Hall; Space-Group Notation with an Explicit Origin ; Acta Cryst. (1981). A37, 517-525

3. ITVB 2001 Table A1.4.2.7 Hall symbols http://cci.lbl.gov/sginfo/itvb_2001_table_a1427_hall_symbols.html

## License

See the [LICENSE](https://raw.githubusercontent.com/narumij/hall-symbols/master/LICENSE)
file in the repository.
