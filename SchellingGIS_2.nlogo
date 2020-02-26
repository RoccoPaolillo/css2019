extensions [ matrix rnd gis ]
globals [ townshp ethnicities sess original-ethn-distr]
turtles-own [ id popdata ethnicity-counts ses-counts totalpop maxpop ]
; Data formats: popdata [ [whiteb_low whiteb_mid whiteb_high] [asian_low asian_mid asian_high] [black_low black_mid black_high] [other_low other_mid other_high] ]
;               ethnicity-counts [whiteb asian black other] (sums over lists in popdata)
;               ses-counts [low mid high] (sums over items in lists of popdata)
;               totalpop [all] (sum over all entries in popdata)
; The latter three turtles variables can all be computed again from popdata, they are stored in agents to increase speed

;; SETUP PROCEDURES

to setup
  clear-all
  ask patches [set pcolor white]
  set townshp gis:load-dataset (word "shp_NetLogo/" town "/" town ".shp")
  gis:set-world-envelope (gis:envelope-union-of (gis:envelope-of townshp))
  let vars [ "WHTB_HG" "WHTB_MD" "WHTB_LW" "ASN_HGH" "ASIN_MD" "ASIN_LW" "BLCK_HG" "BLCK_MD" "BLCK_LW" "OTHRTH_H" "OTHRTH_M" "OTHRTH_L" ]
  set ethnicities [ "WHITEB" "ASIAN" "BLACK" "OTHER" ]
  set sess [ "LOW" "MID" "HIGH" ]
  foreach gis:feature-list-of townshp [ x ->
    let centroid gis:location-of gis:centroid-of x
    crt 1 [
      setxy item 0 centroid item 1 centroid
      set id gis:property-value x "LSOA11C"
      let pops map [y -> round ((gis:property-value x y) / scale-down-pop)] vars
      set popdata (list (reverse sublist pops 0 3) (reverse sublist pops 3 6) (reverse sublist pops 6 9) (reverse sublist pops 9 12))
      set ethnicity-counts count-ethnicities popdata
      set ses-counts count-sess popdata
      set totalpop count-totalpop popdata
      set maxpop round (totalpop / (1 - free_space))
    ]
  ]
  set original-ethn-distr map [ethn -> sort [percent-similar-ethnicity ethn] of turtles] ethnicities
  color-shape
  print-town-data
  reset-ticks
end

to shuffle-population
  ask turtles [
    set popdata matrix:to-row-list matrix:map round
      (matrix:from-row-list townpopdata matrix:* (totalpop / sum [totalpop] of turtles))
    set ethnicity-counts count-ethnicities popdata
    set ses-counts count-sess popdata
    set totalpop count-totalpop popdata
  ]
  color-shape
end

to equal-ethn-ses
  ask turtles [
    set popdata matrix:to-row-list matrix:make-constant length ethnicities length sess round (totalpop / 9)
    set ethnicity-counts count-ethnicities popdata
    set ses-counts count-sess popdata
    set totalpop count-totalpop popdata
    set maxpop round (totalpop / (1 - free_space))
  ]
  color-shape
end

;; GO PROCEDURES

to go
  repeat updates-per-person-per-tick * (sum [totalpop] of turtles) [ ask random-district [ move-one ] ]
  color-shape
  tick
end

to move-one ; for random districts
  ; select a "virtual" person in districtat random
  let ethnicity random-ethnicity
  let ses random-ses ethnicity
  let U_home utility ethnicity ses
  ; probabilisitc event: Does person wants to move? (based on absolute utility) can be switched off (that models that people always check one option)
  if random-float 1 > probability U_home or not decide-search-first [
    let option random-district
    let U_option [utility ethnicity ses] of option
    ; probabilistic event: Person decides to take the option or stay (based on relative utility)
    if ([maxpop - totalpop] of option > 0) and (random-float 1 < probability (U_option - U_home - 0 * distance option)) [
      ; in the line above we can check that distance of option negative in utility does not cause spatial correlation
      alter-popdata ethnicity ses -1
      ask option [ alter-popdata ethnicity ses 1 ]
    ]
  ]
end

to alter-popdata [ethnicity ses change] ; change should be 1 or -1
  let ethn-ind position ethnicity ethnicities
  let ses-ind position ses sess
  set popdata replace-item ethn-ind popdata (replace-item ses-ind (item ethn-ind popdata) (item ses-ind item ethn-ind popdata + change))
  set ethnicity-counts replace-item ethn-ind ethnicity-counts (item ethn-ind ethnicity-counts + change)
  set ses-counts replace-item ses-ind ses-counts (item ses-ind ses-counts + change)
  set totalpop totalpop + change
end

;; VISUALIZATION

to color-shape
  foreach gis:feature-list-of townshp [ x ->
    let turt one-of turtles with [id = (gis:property-value x "LSOA11C")]
    let val (ifelse-value
      (observe = "POP") [ [totalpop] of turt / max [totalpop] of turtles ]
      (observe = "ETHNIC-CONCENTRATION") [ [ethnic-concentration] of turt ]
      (observe = "SES-CONCENTRATION") [ [ses-concentration] of turt ]
      (observe = "AVGERAGE SES") [ [average-ses / 2] of turt ]
      (observe = "pop / maxpop") [ [totalpop / maxpop] of turt ]
      (substring observe 0 7 = "utility")
        [[ utility percent-similar-ethnicity (substring observe 8 (length observe)) ethnic-threshold] of turt]  ; high concentration, when beta = 0 random and not coupled, makes sense
      (substring observe 0 8 = "ethnfrac")
        [ [sum item (position (substring observe 9 (length observe)) ethnicities) popdata] of turt / [totalpop] of turt ]
      (substring observe 0 7 = "sesfrac")
        [ [sum map [y -> item (position (substring observe 8 (length observe)) sess) y] popdata] of turt / [totalpop] of turt ]
    )
    gis:set-drawing-color scale-color red val 1 0
    gis:fill x 0
    ask turt [ set size 0 set label precision val 2 set label-color blue  set hidden? hide-labels? ]
  ]
  gis:set-drawing-color black
  gis:draw townshp 1
  update-plots
end


to print-town-data
  let all1674 (map [x -> gis:property-value x "ALL1674"] gis:feature-list-of townshp)
  output-print (word town ": demographic data used")
  output-print (word "Pop 16-74 with regular SES: " (sum all1674))
  output-print (word "Districts (LSOA): " (length all1674))
  output-print (word "  Pop mean " round (sum all1674 / length all1674) ", min " (min all1674) ", max " (max all1674) )
  output-print ""
  output-print "Ethnicities (%)"
  output-print ethnicities
  output-print map [x -> precision ((100 / count-totalpop townpopdata) * x) 1] (count-ethnicities townpopdata)
  output-print ""
  output-print "SES = Socio-Economic Status (%)"
  output-print sess
  output-print map [x -> precision ((100 / count-totalpop townpopdata) * x) 1] (count-sess townpopdata)
  output-print ""
  output-print "All subgroups (rows Ethn, cols SES)"
  output-print matrix:pretty-print-text matrix:map [x -> precision x 1]
     matrix:times-scalar (matrix:from-row-list townpopdata) (100 / count-totalpop townpopdata)
end


;; REPORTERS
; For selection of "a random person"
to-report random-district report rnd:weighted-one-of turtles [totalpop] end

; For general computations on popdata-type lists of lists
to-report townpopdata report matrix:to-row-list reduce matrix:plus [matrix:from-row-list popdata] of turtles end
to-report count-sess [popd] report map [y -> sum map [x -> item y x] popd] range length sess end
to-report count-ethnicities [popd] report map sum popd end
to-report count-totalpop [popd] report sum map sum popd end

;; REPORTERS FOR TURTLES (districts)

; For selection of "a random person"
to-report random-ethnicity
  report first rnd:weighted-one-of-list (map list ethnicities ethnicity-counts) [ [p] -> last p ]
end
to-report random-ses [ethnicity]
  let ethn-ind item (position ethnicity ethnicities) popdata
  report first rnd:weighted-one-of-list (map list sess ethn-ind) [ [p] -> last p ]
end

; For decisions to move
to-report probability [U] ; models a random binomial choice probability for U + eps > 0 (eps ~ logistic distribution)
  report 1 / (1 + exp ( - beta * U))
end
to-report utility [ethnicity ses] ; the utility concept here is a linear in similarity fraction, shifted such that 0 divides favorable and non-favorable
  ; this utility is dichotomized with a certain degree of determinism (beta) by the function "probability"
  report ((percent-similar-ethnicity ethnicity) - ethnic-threshold) + ses-weight * ((percent-similar-ses ses) - ses-threshold)
end
to-report percent-similar-ethnicity [ethnicity] report item (position ethnicity ethnicities) ethnicity-counts / totalpop end
to-report percent-similar-ses [ses] report item (position ses sess) ses-counts / totalpop end

; For visualization
to-report ethnic-concentration report reduce + (map [x -> (x / totalpop) ^ 2] ethnicity-counts) end
to-report ses-concentration report reduce + (map [x -> (x / totalpop) ^ 2] ses-counts) end
to-report average-ses report (sum (map [[ x y ] -> x * y] ses-counts range length sess)) / totalpop end

;; BACKUP not used

to-report transpose-popdata
  report map [y -> map [x -> item y x] popdata] range length sess
end

to-report random-sesB
  report first rnd:weighted-one-of-list (map list sess (map [y -> map [x -> item y x] popdata] [0 1 2])) [ [p] -> last p ]
end

to-report random-logistic
  let x random-float 1
  report ln (x / (1 - x))
end

to-report random-gumbel
  let x random-float 1
  report (- ln (- ln x))
end
@#$#@#$#@
GRAPHICS-WINDOW
719
10
1354
646
-1
-1
19.0
1
10
1
1
1
0
0
0
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
29
89
103
123
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
29
420
171
454
NIL
color-shape
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
28
458
219
503
observe
observe
"ethnfrac ASIAN" "ethnfrac BLACK" "ethnfrac WHITEB" "ethnfrac OTHER" "sesfrac LOW" "sesfrac MID" "sesfrac HIGH" "POP" "ETHNIC-CONCENTRATION" "SES-CONCENTRATION" "AVGERAGE SES" "utility ASIAN" "utility BLACK" "utility WHITEB" "utility OTHER" "pop / maxpop"
2

BUTTON
30
175
94
209
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
62
544
240
589
avg. ethnic concentration
sum [ethnic-concentration * totalpop] of turtles / sum [totalpop] of turtles
3
1
11

SLIDER
29
214
204
247
ethnic-threshold
ethnic-threshold
0
1
0.35
0.01
1
NIL
HORIZONTAL

MONITOR
62
592
225
637
avg. ses concentration
sum [ses-concentration * totalpop] of turtles / sum [totalpop] of turtles
3
1
11

SLIDER
29
291
201
324
beta
beta
0
10
2.0
0.01
1
NIL
HORIZONTAL

SWITCH
95
505
220
538
hide-labels?
hide-labels?
1
1
-1000

BUTTON
29
129
167
163
NIL
shuffle-population
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
169
129
309
163
NIL
equal-ethn-ses
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
198
18
370
51
scale-down-pop
scale-down-pop
1
20
10.0
1
1
NIL
HORIZONTAL

PLOT
408
321
718
517
Ethnicity in districts
sorted districts
fraction
0.0
10.0
0.0
1.0
true
false
"" "clear-plot\nset-plot-x-range 0 count turtles"
PENS
"default" 1.0 0 -2674135 true "" "foreach range count turtles [x -> plotxy x item x sort [percent-similar-ethnicity (substring observe 9 (length observe))] of turtles]"
"pen-1" 1.0 0 -16777216 true "" "let distr item position (substring observe 9 (length observe)) ethnicities original-ethn-distr\nforeach range length distr [x -> plotxy x item x distr]"

PLOT
407
514
716
645
Districts
Fraction ethnicity
freq
0.0
1.0
0.0
50.0
true
false
"" "clear-plot\nset-plot-x-range 0 1"
PENS
"pen-1" 0.025 2 -16777216 true "" "let distr item position (substring observe 9 (length observe)) ethnicities original-ethn-distr\nhistogram distr"
"pen-2" 0.025 1 -2674135 true "" "histogram [percent-similar-ethnicity (substring observe 9 (length observe))] of turtles"

SWITCH
89
737
264
770
increaseTOthreshold?
increaseTOthreshold?
1
1
-1000

INPUTBOX
26
16
190
76
town
Bradford
1
0
String

OUTPUT
407
10
717
320
12

SLIDER
198
51
370
84
free_space
free_space
0
0.95
0.06
0.01
1
NIL
HORIZONTAL

SLIDER
206
214
378
247
ses-threshold
ses-threshold
0
1
0.35
0.01
1
NIL
HORIZONTAL

SLIDER
206
250
378
283
ses-weight
ses-weight
0
5
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
124
89
389
123
updates-per-person-per-tick
updates-per-person-per-tick
0
1
1.0
0.05
1
NIL
HORIZONTAL

SWITCH
127
179
321
213
decide-search-first
decide-search-first
0
1
-1000

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
