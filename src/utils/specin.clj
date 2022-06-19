(ns utils.specin
  "Facilitates defining function specs inline and automatic instrumentation.

  This is mostly taken from the following library with some minor changes:
  https://github.com/Provisdom/defn-spec"
  (:refer-clojure :exclude [defn])
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))


(declare qualify-symbol
         specin-form)


(defmacro fdef
  "Exact same parameters as `s/fdef`. Automatically enables instrumentation for
  `fn-sym` when `s/*compile-asserts*` is true."
  [fn-sym & specs]
  `(let [r# (s/fdef ~fn-sym ~@specs)]
     ~@(when s/*compile-asserts*
         [`(stest/instrument '~(qualify-symbol fn-sym))])
     r#))

(s/def ::defn-args
  (s/cat :name simple-symbol?
         :docstring (s/? string?)
         :meta (s/? map?)
         :bs (s/* any?)))

(s/fdef defn
  :args ::defn-args
  :ret any?)

(defmacro defn
  "Exact same parameters as `clojure.core/defn`. You may optionally include
  `::s/args` and/or `::s/ret` in your function's attr-map to have the args
  and/or return value of your function checked with `s/assert`.
  Setting `s/*compile-asserts*` to `false` will result in a regular function
  definition."
  {:arglists '([name doc-string? attr-map? [params*] prepost-map? body]
               [name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?])}
  [& args]
  (if s/*compile-asserts*
    (specin-form args)
    `(clojure.core/defn ~@args)))


(defn- qualify-symbol
  [sym-name]
  (symbol (str *ns*) (str sym-name)))

(defn- specin-form
  [args]
  (let [{:keys [name meta]} (s/conform ::defn-args args)
        args-spec (:args meta)
        ret-spec (:ret meta)
        fn-spec (:fn meta)
        fdef-sym (if (false? (::instrument? meta)) `s/fdef `fdef)]
    `(do
       (clojure.core/defn ~@args)
       (~fdef-sym ~name
         ~@(when-let [s args-spec] [:args s])
         ~@(when-let [s ret-spec] [:ret s])
         ~@(when-let [s fn-spec] [:fn s])))))

