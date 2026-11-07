;; Lint config for `mallet`: mistakes, plus limits on complex code.
(:mallet-config
 (:extends :strict)
 (:enable :line-length :max 100)
 (:enable :function-length :max 40 :definers ("DEFN" "DEFCOMMAND"))
 (:enable :cyclomatic-complexity :max 10 :definers ("DEFN" "DEFCOMMAND"))
 (:enable :missing-struct-docstring)
 (:enable :missing-package-docstring)
 (:for-paths ("src" "skills")
             (:enable :missing-variable-docstring)))
