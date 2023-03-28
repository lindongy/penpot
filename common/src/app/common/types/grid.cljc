;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.types.grid
  (:require
   [app.common.schema :as sm]
   [app.common.spec :as us]
   [app.common.types.color :as-alias ctc]
   [app.common.types.grid.color :as-alias grid-color]
   [clojure.spec.alpha :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SPECS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Board grids


(s/def ::grid-color/color string?)
(s/def ::grid-color/opacity ::us/safe-number)

(s/def ::size (s/nilable ::us/safe-integer))
(s/def ::item-length (s/nilable ::us/safe-number))

(s/def ::color (s/keys :req-un [::grid-color/color
                                ::grid-color/opacity]))
(s/def ::type #{:stretch :left :center :right})
(s/def ::gutter (s/nilable ::us/safe-integer))
(s/def ::margin (s/nilable ::us/safe-integer))

(s/def ::square
  (s/keys :req-un [::size
                   ::color]))

(s/def ::column
  (s/keys :req-un [::color]
          :opt-un [::size
                   ::type
                   ::item-length
                   ::margin
                   ::gutter]))

(s/def ::row ::column)

(s/def ::saved-grids
  (s/keys :opt-un [::square
                   ::row
                   ::column]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCHEMA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sm/def! ::grid-color
  [:map {:title "PageGridColor"}
   [:color ::ctc/rgb-color]
   [:opacity ::sm/safe-number]])

(sm/def! ::column-params
  [:map
   [:color ::grid-color]
   [:type [::sm/one-of #{:stretch :left :center :right}]]
   [:size {:optional true} ::sm/safe-number]
   [:margin {:optional true} [:maybe ::sm/safe-number]]
   [:item-length {:optional true} [:maybe ::sm/safe-number]]
   [:gutter {:optional true} [:maybe ::sm/safe-number]]])

(sm/def! ::square-params
  [:map
   [:size ::sm/safe-number]
   [:color ::grid-color]])

(sm/def! ::grid
  [:multi {:dispatch :type}
   [:column
    [:map
     [:type [:= :column]]
     [:display :boolean]
     [:params ::column-params]]]

   [:row
    [:map
     [:type [:= :row]]
     [:display :boolean]
     [:params ::column-params]]]

   [:square
    [:map
     [:type [:= :square]]
     [:display :boolean]
     [:params ::square-params]]]])

(sm/def! ::saved-grids
  [:map {:title "PageGrid"}
   [:square {:optional true} ::square-params]
   [:row {:optional true} ::column-params]
   [:column {:optional true} ::column-params]])
