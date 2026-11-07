;; Lint config for `mallet`: mistakes, plus limits on complex code.
(:mallet-config
 (:extends :strict)
 (:enable :line-length :max 100)
 (:enable :function-length :max 40)
 (:enable :cyclomatic-complexity :max 10))
