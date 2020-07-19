# hall-symbols

Haskell Hall Symbols Library

# Quickstart

```shell
shell> stack repl
```

```haskell
-- prepare
Prelude> :set -package matrix-as-xyz
Prelude> :m Data.Matrix.AsXYZ
Prelude> :l Crystallography.HallSymbols

-- print general positions
Prelude> fmap prettyXYZ $ fromHallSymbols' "C -2yc"

 ["x,y,z","x+1/2,y+1/2,z","x,-y,z+1/2","x+1/2,-y+1/2,z+1/2"]

-- print generators
Prelude> fmap prettyXYZ $ fromHallSymbols'' "C -2yc"

["x,y,z","x+1/2,y+1/2,z","x,-y,z+1/2"]
```



## License

See the [LICENSE](https://github.com/narumij/hall-symbols/LICENSE)
file in the repository.
