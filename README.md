# color-comparator

A Haskell script used to compare a given hex color string to a set of colors.

## Setup
```
cd path/to/color-comparator
cabal install .
```
An executable will be placed in the `$HOME/.cabal/bin/`.

## Usage
In `/path/to/color-comparator`:
```
$ color-comparator <hex color string> <list of hex color strings> --file/-f <path/to/json> -n <num of comparisons> [--xterm256] [--merge]
```

### example
```
$ color-comparator "fff0f0" "123456 abc123, #eee800, 783bbe" --file test.json --xterm256 -n 5
```
outputs:
```
From:
███████      #fff0f0  (255, 240, 240)  0.00

Results compared to: 123456 abc123, #eee800, 783bbe
███████      #eee800  (238, 232,   0)  343.84
███████      #abc123  (171, 193,  35)  346.07
███████      #783bbe  (120,  59, 190)  431.86
███████      #123456  ( 18,  52,  86)  584.87

Results compared to: test.json
███████      #cac4b0  (202, 196, 176)  156.48
███████      #6d3f5b  (109,  63,  91)  483.56
███████      #4e3b31  ( 78,  59,  49)  547.40

Results compared to: xterm256
███████ 255  #eeeeee  (238, 238, 238)  29.67
███████ 231  #ffffff  (255, 255, 255)  36.74
███████ 230  #ffffd7  (255, 255, 215)  46.37
███████ 225  #ffd7ff  (255, 215, 255)  54.31
███████ 254  #e4e4e4  (228, 228, 228)  54.93
```
**Note**: ███████ will output the color if the terminal supports it. Input color requires terminals with RGB support.

## xterm256 Data
The accompanying `xterm_colors.json` was obtained from: https://www.ditig.com/256-colors-cheat-sheet

## License
In accordance with the license (**CC BY-NC-SA 4.0**) used in the JSON data noted in [xterm256 Data](#xterm256-Data), this project will be licensed under **CC BY-NC-SA 4.0** as well.  
