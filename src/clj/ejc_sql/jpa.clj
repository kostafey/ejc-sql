(ns ejc-sql.jpa
  (:use [clojure.java.io]
        [cemerick.pomegranate :as pom]
        [ejc-sql.lib]
        [ejc-sql.output])
  (:import [javax.persistence Persistence
                              EntityManager
                              TypedQuery]))

(def em)

(defn jpa-connect [{:keys [connection-name
                           persistent-xml-url
                           domain-objects-url
                           jdbc-driver-url]}]
  (pom/add-classpath persistent-xml-url)
  (pom/add-classpath domain-objects-url)
  (pom/add-classpath jdbc-driver-url)
  (def em (-> (Persistence/createEntityManagerFactory connection-name)
              (.createEntityManager))))

(defn execute-jpql [jpql]
  (-> em (.createQuery jpql) (.getResultList)))

