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
repl> fmap prettyXYZ $ fromHallSymbols' "C -2yc"

 ["x,y,z","x+1/2,y+1/2,z","x,-y,z+1/2","x+1/2,-y+1/2,z+1/2"]

-- print generators
repl> fmap prettyXYZ $ fromHallSymbols'' "C -2yc"

["x,y,z","x+1/2,y+1/2,z","x,-y,z+1/2"]
```



## License

See the [LICENSE](LICENSE)
file in the repository.
