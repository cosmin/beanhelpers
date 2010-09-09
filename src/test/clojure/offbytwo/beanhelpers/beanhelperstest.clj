(ns offbytwo.beanhelpers.beanhelperstest
  (:use offbytwo.beanhelpers)
  (:use clojure.test)
  (:import (offbytwo.beanhelpers.test Person Address State)))

(deftest clojure-to-java
  (let [person (to-java Person {:name "Bob" 
                                :age 30 
                                :address {:line1 "123 Main St" 
                                          :city "Dallas" 
                                          :state "TX" 
                                          :zip "75432"}})]
    (is (= "Bob" (.getName person)))
    (is (= 30 (.getAge person)))
    (is (= "123 Main St" (.. person getAddress getLine1)))
    (is (= "Dallas" (.. person getAddress getCity)))
    (is (= State/TX (.. person getAddress getState)))
    (is (= "75432" (.. person getAddress getZip)))))

(deftest java-to-clojure
  (let [address (new Address "123 Main St" "Dallas" State/TX "75432")
        person (Person. "Bob" 30 address)
        person-info (from-java person)]
    (is (= "Bob" (:name person)))
    (is (= 30 (:age person)))
    (is (= "123 Main St" (:line1 (:address person))))
    (is (= "TX" (:state (:address person))))))
