;; java -cp cljs.jar:src clojure.main repl.clj

(require 'cljs.repl)
(require 'cljs.build.api)
(require 'cljs.closure)
(require 'cljs.repl.browser)
(require 'cljs.analyzer)

(cljs.build.api/build "src"
  {:main 'n01se.hexdefense
   :output-to "out/main.js"
   :verbose true})


(let [build cljs.closure/build
      repl-env (cljs.repl.browser/repl-env :port 9000)]
  (with-redefs [cljs.closure/build (fn [source opts compiler-env]
                                     (binding [cljs.analyzer/*cljs-ns* 'cljs.user]
                                       (let [rtn (build source opts compiler-env)]
                                         (cljs.repl/evaluate-form
                                          repl-env @compiler-env "auto-reload"
                                          `(~'ns cljs.user (:require ~(with-meta '[n01se.hexdefense]
                                                                        {:reload :reload}))))
                                         rtn)))]
    (cljs.repl/repl repl-env
                    :watch "src"
                    :output-dir "out")))

