(ns ejc-sql.graph
  (:import (com.github.mdr.ascii.java GraphLayouter
                                      GraphBuilder
                                      ScalaJavaHelper)
           (com.github.mdr.ascii.layout.prefs LayoutPrefsImpl)))

(defn layout [& {:keys [remove-kinks
                        compactify
                        elevate-edges
                        vertical
                        unicode
                        double-vertices
                        rounded
                        explicit-ascii-bends]
                 :or {remove-kinks true
                      compactify true
                      elevate-edges true
                      vertical true
                      unicode true
                      double-vertices false
                      rounded false
                      explicit-ascii-bends false}}]
  (LayoutPrefsImpl/custom remove-kinks
                          compactify
                          elevate-edges
                          vertical
                          unicode
                          double-vertices
                          rounded
                          explicit-ascii-bends))

(defn graph [vertexes edges layout]
  (ScalaJavaHelper/renderGraph
   (let [b (GraphBuilder.)]
     (mapv #(.addVertex b %) vertexes)
     (mapv #(.addEdge b (first %) (second %)) edges)
     (.build b))
   layout))

(comment
  (println
   (.toString
    (graph ["a" "b" "c"]
           [["a" "c"] ["a" "b"]]
           (layout :rounded true :unicode true))))
 )




