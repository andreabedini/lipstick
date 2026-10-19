# lipstick-brick

Show [lipstick](https://hackage.haskell.org/package/lipstick) blocks in
[brick](https://hackage.haskell.org/package/brick) and
[vty](https://hackage.haskell.org/package/vty) applications, so the styles,
tables, lists and trees made with lipstick can be used in terminal user
interfaces.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Brick (Widget)
import Lipstick
import Lipstick.Brick

ui :: Widget ()
ui = blockWidget defaultRenderEnv (render (border roundedBorder <> padding 1 2) "Hello")
```

- `blockWidget` lays a block out in the width brick gives it. The widget is
  greedy horizontally when the block grows (`fillWidth`), and fixed
  otherwise.
- `blockImage` gives a vty image of a block, laid out in the width of the
  `RenderEnv`.
- `profileForColorMode` gives the lipstick colour profile matching a vty
  output's colour mode, so colours are resolved for what the terminal can
  show:

  ```haskell
  env = defaultRenderEnv {envProfile = profileForColorMode (outputColorMode output)}
  ```

vty measures text with its own width tables, which can disagree with
lipstick's for some emoji; layouts made of such characters may be off by a
column.

## Licence

MIT, see [LICENSE](https://github.com/andreabedini/lipstick/blob/main/lipstick-brick/LICENSE).
