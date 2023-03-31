[![Clojars Project](https://img.shields.io/clojars/v/no.anteo/broch.svg)](https://clojars.org/no.anteo/broch)

# Broch
A library for handling numbers with units. 

Features:
* Conversion between compatible units
* Comparison and arithmetic
* Data literals

Named after [Ole Jacob Broch](https://en.wikipedia.org/wiki/Ole_Jacob_Broch) 
for his contributions as director of the 
[International Bureau of Weights and Measures](https://en.wikipedia.org/wiki/International_Bureau_of_Weights_and_Measures). 

![](https://upload.wikimedia.org/wikipedia/commons/thumb/4/40/Ole_Jacob_Broch.png/375px-Ole_Jacob_Broch.png)

## Names
There is some disagreement on what to name things in this space, but I've settled on these definitions:
- `measure` = the thing that is measured. (i.e `:length` or `:time`)
- `unit` = `measure`, `scaling` and `symbol`. (the scaling is based in SI units) 
- `quantity` = `unit` and `number`. 

## Usage
The ergonomics for handling units is inspired by the excellent 
[tick.core](https://github.com/juxt/tick#tick).

The following code assumes this ns definition. (You probably wont need `broch.protocols` though.)
```clojure 
(ns my-ns
  (:require [broch.core :as b]
            [broch.protocols :as p]))
```

### Basic units
```clojure
; to turn a number into a quantity, use a unit fn
; there are many built-in ones, like b/meters
(b/meters 10) ;=> #broch/quantity[10 "m"]

; data literals
#broch/quantity[10 "m"] ;=> #broch/quantity[10 "m"]

; the unit fns also convert quantities, if compatible
(b/feet (b/meters 10)) ;=> #broch/quantity[32.8083989501 "ft"]

; but conversion of incompatible units throws an error
(b/meters (b/seconds 3)) ;=> ExceptionInfo "Cannot convert :time into :length"

; you can compare compatible units
(b/> #broch/quantity[1 "km"] #broch/quantity[999 "m"]) ;=> true

; and do arithmetic
(b/- #broch/quantity[1 "km"] #broch/quantity[1 "mi"]) ;=> #broch/quantity[-0.609344 "km"]

; and, again, we get sensible errors if incompatible
(b/- #broch/quantity[2 "km"] #broch/quantity[1 "s"]) ;=> ExceptionInfo "Cannot add/subtract :length and :time"
```

### Derived units

```clojure
; units have a an internal composition-map of measure to exponent + scaling

; simple units have a only their own measure
(p/composition (b/meters 2))
; => {:length 1, :broch/scaled 1}

; compound units have a more complicated composition map of the measures they're composed of
; as we all remember from school, W = J/s = N·m/s = kg·m²/s³
(p/composition (b/watts 4)) 
;=> {:mass 1, :length 2, :time -3, :broch/scaled 1}

; a kilowatt is the same, but scaled by 1000
(p/composition (b/kilowatts 4))
;=> {:mass 1, :length 2, :time -3, :broch/scaled 1000}

; this allows more complicated arithmetic (* and /) to derive the correct unit and convert the quantity, if it's defined
(b/* #broch/quantity[3 "m/s²"] #broch/quantity[3 "s"]) ;=> #broch/quantity[9 "m/s"]
(b/* #broch/quantity[12 "kW"] #broch/quantity[5 "h"]) ;=> #broch/quantity[60 "kWh"]
(b// #broch/quantity[12 "J"] #broch/quantity[1 "km"]) ;=> #broch/quantity[0.12 "N"]

; If all units are cancelled out, a number is returned
(/ #broch/quantity[1 "m"] #broch/quantity[2 "m"]) ;=> 1/2

; If no unit with a derived composition is defined, an error is thrown
(/ #broch/quantity[1 "m"] #broch/quantity[2 "s"]) ;=> #broch/quantity[0.5 "m/s"]
(/ #broch/quantity[2 "s"] #broch/quantity[1 "m"]) 
;=> ExceptionInfo "No derived unit is registered for {#broch/quantity[nil "s"] 1, #broch/quantity[nil "m"] -1}"
```

### Defining new units
Broch comes with a bunch of units pre-defined in `broch.core` (more will likely be added in time, requests and PRs are welcome).

But defining your own units is a peace-of-cake. 

```clojure
; all units have a measure and a symbol 
(b/measure #broch/quantity[1 "m"]) ;=> :speed 
(b/symbol #broch/quantity[1 "m"]) ;=> "m" (same as in the tag literal)

; broch uses the measure to know how to convert and derive units
; both must be given when defining the unit along with its scale from the "base" unit of that measure
(b/defunit meters :length "m" 1) ;=> #'my-ns/meters
(b/defunit seconds :time "s" 1) ;=> #'my-ns/seconds

; The built-in units rely on the SI-system for measures and their base units. 
; So the meter is the base unit of :length, and other units of :length must specify their scale relative to it. 
(b/defunit feet :length "ft" 0.3048) ;=> #'my-ns/feet  

; derived units are similar, but also takes a unit-map giving their composition
(b/defunit meters-per-second :speed "m/s" {meters 1 seconds -1} 1) ;=> #'my-ns/meters-per-second
; Also note that since these units are already defined, running the `defunit` forms above would print warnings like 
; "WARN: a unit with symbol m already exists! Overriding..." 
; You probably don't want to override taken symbols, make up a new one instead.

; If you provide a composition, the given scaling is relative to the composing units
; so you could say for example:
; a yard is 0.9144 meters
(defunit yards :length "yd" 0.9144) :=> #'my-ns/yards 
; and a foot is a third of a yard
(defunit feet :length "ft" 1/3 {yards 1}) :=> #'my-ns/feet
; and there's 12 inches in a foot
(defunit inches :length "in" 1/12 {feet 1}) :=> #'my-ns/inches
; and it knows that an inch is scaled 0.0254 of a meter
(b/meters (b/inches 1)) ;=> #broch/quantity[0.0254 "m"]

; Measures and symbols are just names though, so they could be anything. For example:
(b/defunit me :coolness "me" 1) ;=> #'my-ns/me
(b/defunit rich-hickey :coolness "DJ Rich" 1000) ;=> #'my-ns/rich-hickey
(= #broch/quantity[1 "DJ Rich"] #broch/quantity[1000 "me"]) ;=> true
```

### Tradeoffs
This library is not written for high performance. 
It rather tries to be as accurate as possible and avoid precision loss with floating-point numbers. 
This means that it sometimes "upcasts" numbers to ratios, if it cannot keep it as a double losing precision.
Ratios can be harder to read for humans, so where that is a concern I recommend converting to double with `b/boxed`
```clojure 
(b/boxed double (b/meters 355/113)) ;=> #broch/quantity[3.141592920353982 "m"]
```

### Not yet implemented
- Clojurescript compatibility

## Deploy
Build jar and deploy to clojars with
```shell
$ clj -T:build clean
$ clj -T:build jar
$ env CLOJARS_USERNAME=<username> CLOJARS_PASSWORD=<clojars-token> clj -X:deploy
```
