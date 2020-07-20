# hall-symbols

Haskell Hall Symbols Library

# Quickstart

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

## License

See the [LICENSE](https://github.com/narumij/hall-symbols/blob/master/LICENSE)
file in the repository.
