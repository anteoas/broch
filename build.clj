(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'no.anteo/broch)
(def version (format "0.1.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file "target/deploy.jar")

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (b/write-pom {:class-dir class-dir
                :lib       lib
                :version   version
                :basis     basis
                :src-dirs  ["src"]
                :scm       {:url                 "https://github.com/anteoas/broch"
                            :connection          "scm:git:git://github.com/anteoas/broch.git"
                            :developerConnection "scm:git:ssh://git@github.com/anteoas/broch.git"
                            :tag                 (str "v" version)}})
  (b/copy-file {:src    (str class-dir "/META-INF/maven/no.anteo/broch/pom.xml")
                :target "./pom.xml"})
  (b/copy-dir {:src-dirs   ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file  jar-file}))