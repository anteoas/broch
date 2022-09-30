# units
A lib for handling units of measure.
* Conversion between compatible units
* Comparison and arithmetic 
* Compound units 
* Data literals


## Usage
The ergonomics for handling units is inspired by [tick.core](https://github.com/juxt/tick#tick).

### Basic units
```clojure 
(ns myns 
  (:require [units.core :as un]))

; making a unit
(un/meters 10) ;=> #unit/u[10 "m"]

; data literals
#unit/u[10 "m"] ;=> #unit/u[10 "m"]

; the unit fns also convert units if compatible
(un/feet #unit/u[10 "m"]) ;=> #unit/u[32.80839895013123 "ft"]

; but conversion of incompatible units throws an error
(un/meters (un/seconds 3)) ;=> ExceptionInfo "Cannot convert from :time to :length"

; you can compare compatible units
(un/> #unit/u[1 "km"] #unit/u[999 "m"]) ;=> true

; and do arithmetic
(un/- #unit/u[1 "km"] #unit/u[1 "mi"]) ;=> #unit/u[-0.6093440000000001 "km"]

; and, again, we get sensible errors if incompatible
(un/- #unit/u[2 "km"] #unit/u[1 "s"]) ;=> ExceptionInfo "Cannot add/subtract :length and :time"

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