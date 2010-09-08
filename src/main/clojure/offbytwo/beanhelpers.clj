(ns offbytwo.beanhelpers)

(defmulti to-java (fn [destination-type value] [destination-type (class value)]))
(defmulti from-java class)

;(derive Boolean :primitive)
;(derive Integer :primitive)
;(derive String :primitive)


(defn- get-property-descriptors [clazz]
  (.getPropertyDescriptors (java.beans.Introspector/getBeanInfo clazz)))

(defn- is-getter [method]
  (and method (= 0 (alength (. method (getParameterTypes))))))

(defn- is-setter [method]
  (and method (= 1 (alength (. method (getParameterTypes))))))

(defn- get-setter-type [method]
  (get (.getParameterTypes method) 0))

(defn- make-getter-fn [method]
  (fn [instance]
    (.invoke method instance nil)))

(defn- make-setter-fn [method]
    (fn [instance value]
      (.invoke method instance (into-array [(to-java value (get-setter-type method))]))))

(defn- add-getter-fn [the-map prop-descriptor]
  (let [name (.getName prop-descriptor)
        method (.getReadMethod prop-descriptor)]
    (if (is-getter method)
      (assoc the-map (keyword name) (make-getter-fn method)))))


(defn- add-setter-fn [the-map prop-descriptor]
  (let [name (.getName prop-descriptor)
        method (.getWriteMethod prop-descriptor)]
    (if (is-setter method)
      (assoc the-map (keyword name) (make-setter-fn method))
      the-map)))


(defmethod to-java [Enum String] [enum value]
  (.invoke (.getDeclaredMethod enum "valueOf" (into-array [String])) nil (into-array [value])))

(defmethod to-java [Object clojure.lang.APersistentMap] [clazz props]
  (let [instance (.newInstance clazz)
        setter-map (reduce add-setter-fn {} (get-property-descriptors clazz))]
    (doseq [[key value] props]
      (let [setter (get setter-map (keyword key))]
        (if (nil? setter)
          (println "WARNING: Cannot set value for " key " because there is no setter.")
          (apply setter [instance value]))))
    instance))

(defmethod to-java :default [_ value] value)


;(defmethod from-java )

(defmethod from-java Object [instance]
     (let [clazz (.getClass instance)
           getter-map (reduce add-getter-fn {} (get-property-descriptors clazz))]
       (reduce add-getter-fn {} (get-property-descriptors clazz))))