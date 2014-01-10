(ns ejc-sql.jpa
  (:require [cemerick.pomegranate :as pom])
  (:use [clojure.java.io]
        [ejc-sql.lib]
        [ejc-sql.output])
  (:import [javax.persistence Persistence
                              EntityManager
                              TypedQuery]))

(def em)

(defn connect [{:keys [connection-name
                       persistent-xml-url
                       domain-objects-url
                       jdbc-driver-url]}]
  (pom/add-classpath persistent-xml-url)
  (pom/add-classpath domain-objects-url)
  (pom/add-classpath jdbc-driver-url)
  (def em (-> (Persistence/createEntityManagerFactory connection-name)
              (.createEntityManager))))

(defn connect-plain [connection-name
                     persistent-xml-url
                     domain-objects-url
                     jdbc-driver-url]
  (connect
   {:connection-name    connection-name
    :persistent-xml-url persistent-xml-url
    :domain-objects-url domain-objects-url
    :jdbc-driver-url    jdbc-driver-url}))

(defn eval-jpql [jpql]
  (-> em (.createQuery jpql) (.getResultList)))

(defn row-to-str [row]
  (if (or (instance? Object[] %) (seq? row))
    (map str row)
    (str row)))

(defn eval-jpql-print [jpql]
  (print (map row-to-str (eval-jpql jpql))))
