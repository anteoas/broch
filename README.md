# Broch
A library for handling units of measure. Features:
* Conversion between compatible units
* Comparison and arithmetic 
* Compound units 
* Data literals

Named after [Ole Jacob Broch](https://en.wikipedia.org/wiki/Ole_Jacob_Broch) 
for his contributions as director of the 
[International Bureau of Weights and Measures](https://en.wikipedia.org/wiki/International_Bureau_of_Weights_and_Measures). 

![](https://upload.wikimedia.org/wikipedia/commons/thumb/4/40/Ole_Jacob_Broch.png/375px-Ole_Jacob_Broch.png)

## Usage
The ergonomics for handling units is inspired by the excellent 
[tick.core](https://github.com/juxt/tick#tick).

### Basic units
```clojure
(ns myns 
  (:require [anteo.broch :as b]))

; making a unit
(b/meters 10) ;=> #broch/unit[10 "m"]

; data literals
#broch/unit[10 "m"] ;=> #broch/unit[10 "m"]

; the unit fns also convert units if compatible
(b/feet #broch/unit[10 "m"]) ;=> #broch/unit[32.80839895013123 "ft"]

; but conversion of incompatible units throw an error
(b/meters (b/seconds 3)) ;=> ExceptionInfo "Cannot convert from :time to :length"

; you can compare compatible units
(b/> #broch/unit[1 "km"] #broch/unit[999 "m"]) ;=> true

; and do arithmetic
(b/- #broch/unit[1 "km"] #broch/unit[1 "mi"]) ;=> #broch/unit[-0.6093440000000001 "km"]

; and, again, we get sensible errors if incompatible
(b/- #broch/unit[2 "km"] #broch/unit[1 "s"]) ;=> ExceptionInfo "Cannot add/subtract :length and :time"
```

### Derived units

```clojure
(ns myns
  (:require [anteo.broch :as b]
            [anteo.broch.protocols :as p]))

; derived units have a unit composition represented as a map of unit-fn to exponent
; as we all remember from school, a Watt is kg·m²/s³
(p/->units (b/watts 4)) ;=> {#broch/unit[nil "kg"] 1, #broch/unit[nil "m"] 2, #broch/unit[nil "s"] -3}

; this allows more complicated arithmetic (* and /) to derive the correct unit if defined
(b/* #broch/unit[3 "m/s²"] #broch/unit[3 "s"]) ;=> #broch/unit[9 "m/s"]
(b/* #broch/unit[12 "kW"] #broch/unit[5 "h"]) ;=> #broch/unit[60 "kWh"]
(b// #broch/unit[12 "J"] #broch/unit[1 "km"]) ;=> #broch/unit[0.12 "N"]

; If all units are cancelled out, a number is returned
(/ #broch/unit[1 "m"] #broch/unit[2 "m"]) ;=> 1/2

; If no unit of that composition is defined, error
(/ #broch/unit[1 "m"] #broch/unit[2 "s"]) ;=> #broch/unit[0.5 "m/s"]
(/ #broch/unit[2 "s"] #broch/unit[1 "m"]) ;=> ExceptionInfo "No derived unit is registered for {#broch/unit[nil "s"] 1, #broch/unit[nil "m"] -1}"
```

### Defining new units
Broch comes with a bunch of units pre-defined in `anteo.broch` (more will likely be added in time).
But defining your own units is a peace-of-cake. 

```clojure
(ns myns
  (:require [anteo.broch :as b]))
  
; all units have a measure and a symbol 
(b/measure #broch/unit[1 "m"]) ;=> :speed 
(b/symbol #broch/unit[1 "m"]) ;=> "m" (same as in the tag literal)

; broch uses the measure to know how to convert and derive units
; both must be given when defining the unit alogn with it's scale from the "base" unit of that measure
(b/defunit meters :length "m" 1) ;=> #'myns/meters
(b/defunit seconds :time "s" 1) ;=> #'myns/seconds

; The built-in units rely on the SI-system for measures and their base units. 
; So the meter is the base measure of length, and other units of :length must specify their scale relative to it. 
(b/defunit feet :length "ft" 1250/4101) ;=> #'myns/feet  
; Note that 1250/4101 gives the exact ratio, a floating-point number would result in rounding errors on conversion.


; derived units are similar, but also takes a units-map giving their composition
(b/defderived meters-per-second :speed {meters 1 seconds -1} "m/s" 1) ;=> #'myns/meters-per-second
; Also note that running the `defunit` forms above would print warnings like "WARN: a unit with symbol m already exists! Overriding..." You probably don't want to override taken symbols.

; Measures and symbols are just names though, so they could be anything. For example:
(b/defunit you :coolness "you" 1) ;=> #'myns/you
(b/defunit rich-hickey :coolness "DJ Rich" 1000) ;=> #'myns/rich-hickey
(= #broch/unit[1 "DJ Rich"] #broch/unit[1000 "you"]) ;=> true
```


## Deploy
Update dependencies in pom.xml by running
```shell
$ clj -Spom
```
and deploy to clojars with
```shell
$ env CLOJARS_USERNAME=<username> CLOJARS_PASSWORD=<clojars-token> clj -X:deploy
```