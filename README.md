# hall-symbols

[![Continuous Integration status][status-png]][status]
[![Hackage page (downloads and API reference)][hackage-png]][hackage]
[![Hackage-Deps][hackage-deps-png]][hackage-deps]

Haskell Hall Symbols Library

## Quickstart

Make new stack project and move to project directory.

```shell
% stack new hmRepl
% cd hmRepl
```

Edit your stack.yaml and set the following:

```
resolver: lts-16.8
```

Edit your package.yaml and set the following:

```
dependencies:
- base >= 4.8 && < 5
- hall-symbols
- matrix-as-xyz
- symmetry-operations-symbols
```

Then start repl.

```shell
% stack repl
```

Load modules.

```haskell
repl> :m Data.Matrix.AsXYZ Data.Matrix.SymmetryOperationsSymbols Crystallography.HallSymbols
```

Use like below.

```haskell
-- print General Positions.
repl> prettyXYZ <$> fromHallSymbols' "C -2yc"
 ["x,y,z","x+1/2,y+1/2,z","x,-y,z+1/2","x+1/2,-y+1/2,z+1/2"]

repl> fromHallSymbols' "C -2yc" >>= fromMatrix'
[" 1 "," c  x,0,z"," t (1/2,1/2,0) "," n (1/2,0,1/2) x,1/4,z"]

```

Or use like below.

```haskell
-- print Generators
repl> prettyXYZ <$> generatorsOfHallSymbols "C -2yc"
["x,y,z","x+1/2,y+1/2,z","x,-y,z+1/2"]

repl> generatorsOfHallSymbols "C -2yc" >>= fromMatrix'
[" 1 "," t (1/2,1/2,0) "," c  x,0,z"]

```

## References

1. Concise Space-Group Symbols http://cci.lbl.gov/sginfo/hall_symbols.html , See also : https://github.com/rwgk/sginfo

2. Space-Group Notation with an Explicit Origin
   S.R. Hall; Space-Group Notation with an Explicit Origin ; Acta Cryst. (1981). A37, 517-525

3. ITVB 2001 Table A1.4.2.7 Hall symbols http://cci.lbl.gov/sginfo/itvb_2001_table_a1427_hall_symbols.html

## License

See the [LICENSE](https://raw.githubusercontent.com/narumij/hall-symbols/master/LICENSE)
file in the repository.

 [hackage]: http://hackage.haskell.org/package/hall-symbols
 [hackage-png]: http://img.shields.io/hackage/v/hall-symbols.svg
 [hackage-deps]: http://packdeps.haskellers.com/reverse/hall-symbols
 [hackage-deps-png]: https://img.shields.io/hackage-deps/v/hall-symbols.svg

 [status]: http://travis-ci.org/narumij/hall-symbols?branch=master
 [status-png]: https://api.travis-ci.org/narumij/hall-symbols.svg?branch=master