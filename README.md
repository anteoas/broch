[![Clojars Project](https://img.shields.io/clojars/v/no.anteo/broch.svg)](https://clojars.org/no.anteo/broch)

# Broch
A library for handling quantities. Features:
* Conversion between compatible units
* Comparison and arithmetic
* Data literals

Named after [Ole Jacob Broch](https://en.wikipedia.org/wiki/Ole_Jacob_Broch) 
for his contributions as director of the 
[International Bureau of Weights and Measures](https://en.wikipedia.org/wiki/International_Bureau_of_Weights_and_Measures). 

![](https://upload.wikimedia.org/wikipedia/commons/thumb/4/40/Ole_Jacob_Broch.png/375px-Ole_Jacob_Broch.png)

## Names
There is not full agreement on what to name things in this space, but I've settled on these definitions.
- `measure` = the thing that is measured. (i.e `:length` or `:time`)
- `unit` = `measure` + scaling, denoted by a unit symbol. (scaling based in SI units) 
- `quantity` = `unit` + number. 

The actual type is called Quantity. 
But I sometimes call it a unit (and internally the number can be `nil` to represent this) 
if we're talking about just the unit and not any specific amount.

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
; making a quantity
(b/meters 10) ;=> #broch/quantity[10 "m"]

; data literals
#broch/quantity[10 "m"] ;=> #broch/quantity[10 "m"]

; the unit fns also convert units if compatible
(b/feet #broch/quantity[10 "m"]) ;=> #broch/quantity[32.8083989501 "ft"]

; but conversion of incompatible units throw an error
(b/meters (b/seconds 3)) ;=> ExceptionInfo "Cannot convert :time into :length"

; you can compare compatible units
(b/> #broch/quantity[1 "km"] #broch/quantity[999 "m"]) ;=> true

; and do arithmetic
(b/- #broch/quantity[1 "km"] #broch/quantity[1 "mi"]) ;=> #broch/quantity[-0.6093440000000001 "km"]

; and, again, we get sensible errors if incompatible
(b/- #broch/quantity[2 "km"] #broch/quantity[1 "s"]) ;=> ExceptionInfo "Cannot add/subtract :length and :time"
```

### Derived units

```clojure
; units have a composition represented as a map of unit to exponent

; simple units have a self-referring composition
(p/composition (b/meters 2))
; => {#broch/quantity[nil "m"] 1, :broch/scaled 1}

; as we all remember from school, a Watt is kg·m²/s³
(p/composition (b/watts 4)) 
;=> {#broch/quantity[nil "kg"] 1, #broch/quantity[nil "m"] 2, #broch/quantity[nil "s"] -3, , :broch/scaled 1}

; a kilowatt is the same, but scaled by 1000
(p/composition (b/kilowatts 4))
;=> {#broch/quantity[nil "kg"] 1, #broch/quantity[nil "m"] 2, #broch/quantity[nil "s"] -3, :broch/scaled 1000}

; this allows more complicated arithmetic (* and /) to derive the correct unit and convert the quantity, if it's defined
(b/* #broch/quantity[3 "m/s²"] #broch/quantity[3 "s"]) ;=> #broch/quantity[9 "m/s"]
(b/* #broch/quantity[12 "kW"] #broch/quantity[5 "h"]) ;=> #broch/quantity[60 "kWh"]
(b// #broch/quantity[12 "J"] #broch/quantity[1 "km"]) ;=> #broch/quantity[0.12 "N"]

; If all units are cancelled out, a number is returned
(/ #broch/quantity[1 "m"] #broch/quantity[2 "m"]) ;=> 1/2

; If no unit of that composition is defined, error
(/ #broch/quantity[1 "m"] #broch/quantity[2 "s"]) ;=> #broch/quantity[0.5 "m/s"]
(/ #broch/quantity[2 "s"] #broch/quantity[1 "m"]) 
;=> ExceptionInfo "No derived unit is registered for {#broch/quantity[nil "s"] 1, #broch/quantity[nil "m"] -1}"
```

### Defining new units
Broch comes with a bunch of units pre-defined in `broch.core` (more will likely be added in time).
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
(b/defunit feet :length "ft" 1250/4101) ;=> #'my-ns/feet  
; Note that 1250/4101 gives the exact ratio
; a double would not be completely accurate resulting in rounding errors on conversion.

; derived units are similar, but also takes a unit-map giving their composition
(b/defunit meters-per-second :speed "m/s" {meters 1 seconds -1} 1) ;=> #'my-ns/meters-per-second
; Also note that since these units are alrady defined, running the `defunit` forms above would print warnings like 
; "WARN: a unit with symbol m already exists! Overriding..." 
; You probably don't want to override taken symbols, make up a new one instead.

; Measures and symbols are just names though, so they could be anything. For example:
(b/defunit me :coolness "me" 1) ;=> #'my-ns/me
(b/defunit rich-hickey :coolness "DJ Rich" 1000) ;=> #'my-ns/rich-hickey
(= #broch/quantity[1 "DJ Rich"] #broch/quantity[1000 "me"]) ;=> true
```

### Tradeoffs
This library is not written for high performance. 
It rather tries to be as accurate as possible and avoid precision loss with floating-point numbers. 
This means that it sometimes returns ratios instead of doubles, if it cannot downcast without losing precision.
Ratios are harder to read for humans, so where that is a concern I recommend converting to double with `b/boxed`
```clojure 
(b/boxed double (b/meters 1/3)) ;=> #broch/quantity[0.3333333333333333 "m"]

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