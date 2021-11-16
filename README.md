# color-comparator

A Haskell script used to compare a given hex color string with a list of term 256 colors.

## Setup
```
cd path/to/color-comparator
cabal build .
```

## Usage
In `/path/to/color-comparator`:
```
cabal run . <hex string>
```
or
```
/path/to/color-comparator/binary <hex string>
```

### example
```
$ cabal run . fff0f0
```
outputs:
```
Up to date
Input: 
███████ #fff0f0 [255,240,240] 0.0

Results: 
███████ #ffffd7 [255,255,215] 46.36809
███████ #ffd7ff [255,215,255] 54.313904
███████ #ffd7d7 [255,215,215] 61.237244
███████ #d7ffff [215,255,255] 76.99939
███████ #d7ffd7 [215,255,215] 82.40999
███████ #d7d7ff [215,215,255] 86.76927
███████ #ffffaf [255,255,175] 96.6954
███████ #ffd7af [255,215,175] 104.64225
███████ #d7ffaf [215,255,175] 120.63957
███████ #d7d7af [215,215,175] 127.098015
███████ #ffafff [255,175,255] 131.7194
███████ #ffafd7 [255,175,215] 134.72194
███████ #afffff [175,255,255] 136.36462
███████ #afffd7 [175,255,215] 139.71512
███████ #afd7ff [175,215,255] 142.11021
```
**Note**: ███████ will output the color if the terminal supports it. Input color requires terminals with RGB support.

## Term 256 Data
The accompanying term_256_colors.json was obtained from: https://www.ditig.com/256-colors-cheat-sheet

## License
In accordance with the license (**CC BY-NC-SA 4.0**) used in the JSON data noted in [Term 256 Data](#Term-256-Data), this project will be licensed under CC BY-NC-SA 4.0 as well.  
