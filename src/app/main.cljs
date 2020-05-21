
(ns app.main
  (:require [app.control :as control :refer [init-interface refresh-interface]]))

;; called when app first loads (as specified in shadow-cljs.edn)
(defn main! []
  (init-interface)
  )

;; hot reload; called by shadow-cljs when code is saved
;;   object state is preserved
(defn reload! []
  (refresh-interface))
