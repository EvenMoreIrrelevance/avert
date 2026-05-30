(ns emi.avert)

(declare -unknown-to-kondo)

(defn -swallow
  {:clj-kondo/macroexpand-hook true}
  [& _vals]
  nil)

(defmacro -if-kondo
  {:clj-kondo/macroexpand-hook true}
  [kondof runtimef]
  (if-not (resolve 'emi.avert/-unknown-to-kondo)
    kondof
    runtimef))

(defmacro -when-kondo
  {:clj-kondo/macroexpand-hook true}
  [& forms]
  `(-if-kondo
     ~@(if (< (count forms) 2)
         forms
         `(do ~@forms))
     nil))

(defn -parse-pairs-and-tail
  {:clj-kondo/macroexpand-hook true}
  [syntax]
  (if (< (bounded-count 2 syntax) 2)
    [nil (first syntax)]
    (let [[pairs [[tail]]] (partition-by count (partition-all 2 syntax))]
      [pairs tail])))

(comment
  (-parse-pairs-and-tail '[1 2 3 4 5])
  (-parse-pairs-and-tail '[1 2 3 4])
  (-parse-pairs-and-tail '[1])
  *e)

(defn throw-errform
  "Either throws the given error or an ex-info with the given message and error map"
  {:clj-kondo/macroexpand-hook true}
  [err-or-map message]
  (throw
    (if (map? err-or-map)
      (ex-info
        (or (::message err-or-map) message)
        (dissoc err-or-map ::message ::cause)
        (::cause err-or-map))
      err-or-map)))

(defmacro mavert
  "Throws an error (implicitly coerced with `throw-errform`) if any of the conditions are truthy;
   When building the error, `not-averted` is let-bound to `[condition form-representation]`"
  {:clj-kondo/macroexpand-hook true}
  [not-averted & averted-err-pairs-and-ok]
  (let [[averted->err ok]
        (-parse-pairs-and-tail averted-err-pairs-and-ok)]
    `(do
       ~@(for [[avertion errform] averted->err]
           `(when-let [not-averted# ~avertion]
              (let [~not-averted [not-averted# (quote ~avertion)]]
                (-when-kondo ~not-averted)
                (throw-errform ~errform ~(pr-str avertion)))))
       ~ok)))

(defmacro $avert
  "Shorthand for mavert with `not-averted` = `[$ $form]`"
  {:clj-kondo/macroexpand-hook true}
  [& averted-err-pairs-and-ok]
  `(mavert ~'[$ $form] ~@averted-err-pairs-and-ok))

(defn enriched-error
  "Builds an ex-info having the same message as the original error 
   (unless ::message is bound in the enrichment-map), `(merge (ex-data err) enrichment-map)` as the data
   and `err` as the cause."
  {:clj-kondo/macroexpand-hook true}
  [err enrichment-map]
  (ex-info
    (or (::message enrichment-map) (ex-message err))
    (merge (ex-data err) (dissoc enrichment-map ::message ::cause))
    (or (::cause enrichment-map) err)))

(defmacro with-cleanup
  "try-finally with the finally clause at the top."
  {:clj-kondo/macroexpand-hook true}
  [cleanup & body]
  `(try (do ~@body)
     (finally ~cleanup)))

(defn -coerce-catching-clauses
  {:clj-kondo/macroexpand-hook true}
  [clauses]
  (cond
    (seq? clauses) `[~clauses]
    (simple-symbol? (first clauses)) `[(java.lang.Exception ~@clauses)]
    :else clauses))

(defmacro catching
  "try-catch with the catch bodies at the top, expressed either as a vector of a simple-symbol
   and a body (which will catch `Exception`), a single `catch` clause without the `catch` marker,
   or a vector thereof."
  {:clj-kondo/macroexpand-hook true}
  [catches & body]
  body
  `(try (do ~@body)
     ~@(doall
         (let [coerced-catches (-coerce-catching-clauses catches)]
           ($avert
             (not (vector? coerced-catches)) {::message "failed coercing catches" :value catches}
             (seq (remove seq? coerced-catches)) {::message "found non-sequence catch clauses" :value $}
             (for [clause coerced-catches]
               (do (-swallow clause) `(catch ~@clause))))))))

(defn -coerce-rethrowing-clauses
  {:clj-kondo/macroexpand-hook true}
  [clauses]
  (if (map? clauses)
    `[(java.lang.Exception e# (enriched-error e# ~clauses))]
    (-coerce-catching-clauses clauses)))

(defmacro rethrowing
  "Like catching, but throws the evaluation of catch clauses.
   If `rethrows` is a map, `Exception` is caught and `(enriched-error e# ~clauses)` is thrown."
  {:clj-kondo/macroexpand-hook true}
  [rethrows & body]
  `(try (do ~@body)
     ~@(doall
         (let [coerced-rethrows (-coerce-rethrowing-clauses rethrows)]
           ($avert
             (not (vector? coerced-rethrows)) {::message "failed coercing rethrows" :value rethrows}
             (seq (remove seq? coerced-rethrows)) {::message "found non-sequence rethrow clauses" :value $}
             (for [clause coerced-rethrows]
               (let [[ety evar & bod] clause]
                 (-when-kondo (-swallow ety evar bod))
                 `(catch ~ety ~evar
                    ~@(butlast bod)
                    (throw-errform ~(last bod) "")))))))))

(comment 
  (catching (Throwable _ nil)
    (assert (= true false))) 
  
  (catching [(clojure.lang.ExceptionInfo e {:cause (ex-cause e)})
             (Exception _ "should be dead code")]
    (rethrowing (Throwable e {::message "didn't catch Error" ::cause e})
      (catching [_ nil]
        (assert (= true false)))))

  *e)
