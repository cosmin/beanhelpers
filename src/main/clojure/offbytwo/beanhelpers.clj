(ns offbytwo.beanhelpers)

(defmulti to-java (fn [destination-type value] [destination-type (class value)]))
(defmulti from-java class)

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
    (from-java (.invoke method instance nil))))

(defn- make-setter-fn [method]
    (fn [instance value]
      (.invoke method instance (into-array [(to-java (get-setter-type method) value)]))))

(defn- add-getter-fn [the-map prop-descriptor]
  (let [name (.getName prop-descriptor)
        method (.getReadMethod prop-descriptor)]
    (if (and (is-getter method) (not (= "class" name)))
      (assoc the-map (keyword name) (make-getter-fn method))
      the-map)))

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

(doseq [clazz [String Character Byte Short Integer Long Float Double Boolean]]
  (derive clazz ::reference-type))

(defmethod from-java ::reference-type [value] value)
(defmethod from-java Iterable [instance] (for [each (seq instance)] (from-java each)))
(defmethod from-java java.util.Map [instance] (into {} instance))
(defmethod from-java nil [_] nil)
(defmethod from-java Enum [enum] (str enum))

(defmethod from-java Object [instance]
  (try
    (let [clazz (.getClass instance)
          getter-map (reduce add-getter-fn {} (get-property-descriptors clazz))]
      (into {} (for [[key getter-fn] (seq getter-map)] [key (getter-fn instance)])))
    (catch Exception e (println "Error trying to convert " instance e))))

(defmethod from-java javax.xml.datatype.XMLGregorianCalendar [obj]
           (let [date {:year (.getYear obj)
                       :month (.getMonth obj)
                       :day (.getDay obj)}
                 time {:hour (.getHour obj)
                       :minute (.getMinute obj)
                       :second (.getSecond obj)}
                 tz {:timezone (.getTimezone obj)}
                 is-undefined? #(= javax.xml.datatype.DatatypeConstants/FIELD_UNDEFINED %1)]
             (conj {}
                   (if-not (is-undefined? (:year date))
                     date)
                   (if-not (is-undefined? (:hour time))
                     time)
                   (if-not (is-undefined? (:timezone tz))
                     tz))))

(prefer-method from-java java.util.Map Iterable)
(prefer-method from-java Iterable Object)
(prefer-method from-java ::reference-type Object)
