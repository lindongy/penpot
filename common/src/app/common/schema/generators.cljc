;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.schema.generators
  (:refer-clojure :exclude [set])
  (:require
   [clojure.test.check.generators :as tgen]
   [app.common.schema.registry :as sr]
   [app.common.uuid :as uuid]
   [malli.core :as m]
   [malli.generator :as mg]
   [malli.util :as mu]))

(defn sample
  ([g]
   (mg/sample g {:registry sr/default-registry}))
  ([g opts]
   (mg/sample g (assoc opts :registry sr/default-registry))))

(defn generate
  ([g]
   (mg/generate g {:registry sr/default-registry}))
  ([g opts]
   (mg/generate g (assoc opts :registry sr/default-registry))))

(defn generator
  ([s]
   (mg/generator s {:registry sr/default-registry}))
  ([s opts]
   (mg/generator s (assoc opts :registry sr/default-registry))))

(defn small-double
  [& {:keys [min max] :or {min -100 max 100}}]
  (tgen/double* {:min min, :max max, :infinite? false, :NaN? false}))

(defn small-int
  [& {:keys [min max] :or {min -100 max 100}}]
  (tgen/large-integer* {:min min, :max max}))

;; FIXME: revisit
(defn uuid
  []
  (->> tgen/small-integer
       (tgen/fmap (fn [_] (uuid/next)))))

;; FIXME: revisit
(defn set-of
  [choices]
  (->> tgen/nat
       (tgen/fmap (fn [i]
                    (into #{}
                          (map (fn [_] (rand-nth choices)))
                          (range i))))))
(defn set
  [g]
  (tgen/set g))

(defn elements
  [s]
  (tgen/elements s))

(defn one-of
  [& gens]
  (tgen/one-of (into [] gens)))

(defn fmap
  [f g]
  (tgen/fmap f g))

(defn tuple
  [& opts]
  (apply tgen/tuple opts))

