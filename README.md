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
(un/meters 10) ;=> #length/m"10"

; data literals
#length/m"10" ;=> #length/m"10"

; the unit fns also convert units if compatible
(un/feet #length/m"10") ;=> #length/ft"32.80839895013123"

; but conversion of incompatible units throws an error
(un/meters (un/seconds 3)) ;=> ExceptionInfo "Cannot convert from :time to :length"

; you can compare compatible units
(un/> #length/km"1" #length/m"999") ;=> true

; and do arithmetic
(un/- #length/km"2" #length/mi"1") ;=> #length/km"0.39065599999999995"

; and, again, we get sensible errors if incompatible
(un/- #length/km"2" #time/s"1") ;=> ExceptionInfo "Cannot convert from :time to :length"

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