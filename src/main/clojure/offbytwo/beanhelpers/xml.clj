(ns offbytwo.beanhelpers.xml
  (:use offbytwo.beanhelpers))

(defmethod to-java [javax.xml.datatype.XMLGregorianCalendar clojure.lang.APersistentMap] [clazz props]
  "Create an XMLGregorianCalendar object given the following keys :year :month :day :hour :minute :second :timezone"
  (let [instance (.newInstance clazz)
        undefined javax.xml.datatype.DatatypeConstants/FIELD_UNDEFINED
        getu #(get %1 %2 undefined)]
    (doto instance
      (.setYear (getu props :year))
      (.setMonth (getu props :month))
      (.setDay (getu props :day))
      (.setHour (getu props :hour))
      (.setMinute (getu props :minute))
      (.setTimezone (getu props :timezone)))))

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
