# MAFBA - Driver

#### DSL based Split-flap display driver for MANtronic Annax display.


### Features:
- Declarative definition of display segments
- Custom lookup tables for each segments
- Text translation and validation for a given display (`text/1`)
- Assisted calibration based on segment definition (🚧 WIP)
- Loading and resetting of internal LUTs (🚧 WIP)

## Usage:

```ex
defmodule MyDisplay do
  use Mafba.Display,
    device: "/dev/..." # Serial port

  defsegment position: 0,
    address: 0x00,
    table: ~t|ABCDEF...|

  # ...
end
```
The generated module can also be imported into iex for interactive testing. Make sure that the mafba application is started.

```ex
iex(1)> MyDisplay.text "Hello World! :3" 
{:ok, :noreply}
iex(2)> MyDisplay.home
{:ok, :noreply}
```

> Happy flapping :3