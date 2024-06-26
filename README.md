[![Clojars Project](https://img.shields.io/clojars/v/no.anteo/broch.svg)](https://clojars.org/no.anteo/broch)

# Broch
A library for handling numbers with units. 

Main Features:
* Conversion, comparison and arithmetic.
* Data literals
* Clojurescript compatible
* No dependencies

Named after [Ole Jacob Broch](https://en.wikipedia.org/wiki/Ole_Jacob_Broch) 
for his contributions as director of the 
[International Bureau of Weights and Measures](https://en.wikipedia.org/wiki/International_Bureau_of_Weights_and_Measures). 

![](https://upload.wikimedia.org/wikipedia/commons/thumb/4/40/Ole_Jacob_Broch.png/375px-Ole_Jacob_Broch.png)

## Names
There is some disagreement on what to name things in this space, but I've settled on these definitions:
- `measure` = the thing that is measured. (i.e `:length` or `:time`)
- `unit` = a `measure` with a set `scaling` and a `symbol`. (the scaling is based in SI units) 
- `quantity` = a `number` with a `unit`. 

## Usage
The ergonomics for handling units is inspired by the excellent 
[tick.core](https://github.com/juxt/tick#tick).

The following code assumes this ns definition.
```clojure 
(ns my-ns
  (:require [broch.core :as b]))
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

; if you need it, you can find all equivalent quantities (with the same measure)
(sort-by b/num (b/equivalent-quantities (b/seconds 1)))
;=> (#broch/quantity[1/3600 "h"]
;    #broch/quantity[1/60 "min"]
;    #broch/quantity[1 "s"]
;    #broch/quantity[1000 "ms"]
;    #broch/quantity[1000000 "μs"]
;    #broch/quantity[1000000000 "ns"])
```

### Derived units

```clojure
; units have an internal composition-map of measure to exponent + scaling

; simple units just have their own measure
(b/composition (b/meters 2))
; => {:length 1, :broch/scaled 1}

; compound units have a more complicated composition map of the measures they're composed of
; as we all remember from school: W = J/s = N·m/s = kg·m²/s³
(b/composition (b/watts 4)) 
;=> {:mass 1, :length 2, :time -3, :broch/scaled 1}

; a kilowatt is the same, but scaled by 1000
(b/composition (b/kilowatts 4))
;=> {:mass 1, :length 2, :time -3, :broch/scaled 1000}

; this allows more complicated arithmetic (* and /) to derive the correct unit and convert the quantity, if it's defined
(b/* #broch/quantity[3 "m/s²"] #broch/quantity[3 "s"]) ;=> #broch/quantity[9 "m/s"]
(b/* #broch/quantity[12 "kW"] #broch/quantity[5 "h"]) ;=> #broch/quantity[60 "kWh"]
(b// #broch/quantity[12 "J"] #broch/quantity[1 "km"]) ;=> #broch/quantity[0.12 "N"]

; If all units are cancelled out, a number is returned
(b// #broch/quantity[1 "m"] #broch/quantity[2 "m"]) ;=> 1/2

; If no unit with a derived composition is defined, an error is thrown
(b// #broch/quantity[1 "m"] #broch/quantity[2 "s"]) ;=> #broch/quantity[0.5 "m/s"]
(b// #broch/quantity[2 "s"] #broch/quantity[1 "m"]) 
;=> ExceptionInfo "No derived unit is registered for {#broch/quantity[nil "s"] 1, #broch/quantity[nil "m"] -1}"
```

### Defining new units
Broch comes with a bunch of units pre-defined in `broch.core` (more might be added in time).

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

; derived units are similar, but also take a unit-map giving their composition
(b/defunit meters-per-second :speed "m/s" {meters 1 seconds -1} 1) ;=> #'my-ns/meters-per-second
; Also note that since these units are already defined, running the `defunit` forms above would print warnings like 
; "WARN: a unit with symbol m already exists! Overriding..." 
; You probably don't want to override taken symbols, make up a new one instead.

; If you provide a composition, the given scaling is relative to the composing units
; so you could say for example:
; a yard is 0.9144 meters
(defunit yards :length "yd" 0.9144) ;=> #'my-ns/yards 
; and a foot is a third of a yard
(defunit feet :length "ft" 1/3 {yards 1}) ;=> #'my-ns/feet
; and there's 12 inches in a foot
(defunit inches :length "in" 1/12 {feet 1}) ;=> #'my-ns/inches
; and it knows that an inch is scaled 0.0254 of a meter
(b/meters (b/inches 1)) ;=> #broch/quantity[0.0254 "m"]

; Measures and symbols are just names though, so they could be anything. For example:
(b/defunit me :coolness "me" 1) ;=> #'my-ns/me
(b/defunit rich-hickey :coolness "DJ Rich" 1000) ;=> #'my-ns/rich-hickey
(= #broch/quantity[1 "DJ Rich"] #broch/quantity[1000 "me"]) ;=> true
```

### Formatting
```clojure
; The standard print is a tagged literal with number and unit symbol
#broch/quantity[1000 "m"]
; which is useful for developers, but not so much for everyone else

; str gives a more readable string
(str #broch/quantity[1000 "m"]) ;=> "1000 m" 

; and you can use the provided `nicest` fn to get a "nicer" unit, given some compatible options
(str (b/nicest #broch/quantity[1000 "m"] [b/meters b/kilometers])) ;=> "1 km"
; "niceness" is defined to be the unit with the shortest number, 
; and it prefers non-ratios when possible as they can be hard to read

; Localization is out of scope for broch.
; A lot of languages use latin unit symbols for SI-units, so you might not need to translate those,
; but you can use `b/num` and `b/symbol` to format and translate however you like, f.ex.:
(str (format-number language (b/num q)) " " 
     (translate-symbol language (b/symbol q)))

```

## Tradeoffs
This library is not written for high performance. 
It rather tries to be as accurate as possible and avoid precision loss with floating-point numbers. 
This means that it sometimes "upcasts" numbers to ratios, if it cannot keep it as a double without losing precision.
Ratios can be harder to read for humans, so where that is a concern you can cast the number type with `b/boxed`
```clojure 
(b/boxed double (b/meters 355/113)) ;=> #broch/quantity[3.141592920353982 "m"]
```

## FAQ
<details>
    <summary>Where's Celsius and Fahrenheit?</summary>

TLDR: Intentionally left out.

Both these common ways of denoting temperature has intentionally been *left out* of this library. 
This is because neither °C nor °F are actually *just* units of measure in the true sense, because their zero-points are not zero.
They are *units on a scale*, which is why we prefix them with a °.

Zero grams is no mass, and zero miles per hour is no speed, but zero °C is *not* no temperature. 
It's quite a lot of temperature actually, exactly 273.15 K of temperature.
Zero kelvin *is* no temperature, and that's why it is included in this library, 
and why it's (probably) the only unit for temperature you'll ever see used in any real computations involving temperature. 

We could have added support for translation (shifting the zero-point),
but that would have complicated conversion and raised some difficult questions on how to handle equality and arithmetic with 
these non-zero-based units. 

For example:
```clojure 
; remember that 32°F = 0°C
(b/+ (b/fahrenheit 32) (b/celcius 0)) ;=> ?
(b/+ (b/celcius 0) (b/fahrenheit 32)) ;=> ?
```
Is it 0 °C or 64 °F? Both answers are plausible depending on which unit you choose to convert to before adding them together.
And picking one interpretation, say always converting to the first argument's unit, would mean that `b/+` and `b/*` are no longer 
[commutative](https://en.wikipedia.org/wiki/Commutative_property) for temperatures, which is no good. 

In conclusion, if you need to do stuff with temperatures and show the result in °C or °F,
do whatever you need to do in kelvins and then [scale the result yourself](https://en.wikipedia.org/wiki/Conversion_of_scales_of_temperature) like this:
```clojure 
(def k #broch/quantity[345 "K"])

; to fahrenheit
(double
 (- (* (b/num k) 9/5)
    (rationalize 459.67))) 
;=> 161.33

; to celcius
(double
 (- (b/num k)
    (rationalize 273.15))) 
;=> 71.85
```

</details>

<details>
<summary>What happens when units have the same composition?</summary>

Some units are defined in terms of the same SI-units. 
For example, you have both Hertz and Becquerel defined as 1/s, but their measure is different. 
Hertz measures frequency and Becquerel measures radioactive decay, these measures differ in that one is periodic and the other isn't.

The problem is how to know which unit to use if a calculation returns a composition used by more than one unit, like 1/s.
```clojure
(b// (b/meters-per-second 1) (b/meters 1)) ;=> hertz or becquerel?
```
Being correct in questions like this is outside the scope of this library. 
So in cases where the unit compositions are equal broch will just return the unit that was defined first as a default. 
In this case Hertz:
```clojure
(b// (b/meters-per-second 1) (b/meters 1)) ;=> #broch/quantity[1 "Hz"]
```

And if you need an alternative unit you can explicitly convert it, as these units are compatible, despite having different measure
, due to having equivalent compositions.
```clojure
(b/becquerels (b/hertz 1)) ;=> #broch/quantity[1 "Bq"]
```

</details>

## Deploy
To build jar and deploy to clojars:
1. Have username and clojars-token in `.deploy-opts.edn` and run
2. Run `bin/deploy.bb`
```shell 
 bin/deploy.bb
```
