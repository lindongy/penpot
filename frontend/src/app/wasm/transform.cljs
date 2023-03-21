(ns app.wasm.transform
  (:require
   [app.common.data.macros :as dm]
   [app.util.wasm :as wasm]
   [app.util.wasm.types :as types]
   [promesa.core :as p]))

(defonce instance (atom nil))
(defonce memory (atom nil))
(defonce transform-input (atom nil))
(defonce transform-output (atom nil))

(defn- init-proxies
  [asm]
  (js/console.log "init transform proxies")
  (reset! transform-input (types/from asm "transformInput" "TransformInput"))
  (reset! transform-output (types/from asm "transformOutput" "TransformOutput")))

(defn set-transform-matrix
  [matrix]
  (let [transform-input @transform-input]
    (set! (.. ^js transform-input -matrix -a) (:a matrix))
    (set! (.. ^js transform-input -matrix -b) (:b matrix))
    (set! (.. ^js transform-input -matrix -c) (:c matrix))
    (set! (.. ^js transform-input -matrix -d) (:d matrix))
    (set! (.. ^js transform-input -matrix -e) (:e matrix))
    (set! (.. ^js transform-input -matrix -f) (:f matrix))))

(defn set-transform-transform
  [tf]
  (let [transform-input @transform-input]
    (set! (.. ^js transform-input -transform -a) (:a tf))
    (set! (.. ^js transform-input -transform -b) (:b tf))
    (set! (.. ^js transform-input -transform -c) (:c tf))
    (set! (.. ^js transform-input -transform -d) (:d tf))
    (set! (.. ^js transform-input -transform -e) (:e tf))
    (set! (.. ^js transform-input -transform -f) (:f tf))))

(defn set-transform-transform-inverse
  [tfi]
  (let [transform-input @transform-input]
    (set! (.. ^js transform-input -transformInverse -a) (:a tfi))
    (set! (.. ^js transform-input -transformInverse -b) (:b tfi))
    (set! (.. ^js transform-input -transformInverse -c) (:c tfi))
    (set! (.. ^js transform-input -transformInverse -d) (:d tfi))
    (set! (.. ^js transform-input -transformInverse -e) (:e tfi))
    (set! (.. ^js transform-input -transformInverse -f) (:f tfi))))

(defn set-transform-vector
  [vector]
  (let [transform-input @transform-input]
    (set! (.. ^js transform-input -vector -x) (:x vector))
    (set! (.. ^js transform-input -vector -y) (:y vector))))

(defn set-transform-center
  [center]
  (let [transform-input @transform-input]
    (set! (.. ^js transform-input -center -x) (:x center))
    (set! (.. ^js transform-input -center -y) (:y center))))

(defn set-transform-origin
  [origin]
  (let [transform-input @transform-input]
    (set! (.. ^js transform-input -origin -x) (:x origin))
    (set! (.. ^js transform-input -origin -y) (:y origin))))

(defn set-transform-should-transform
  [should-transform]
  (let [transform-input @transform-input]
    (set! (.. ^js transform-input -shouldTransform) should-transform)))

(defn set-transform-rotation
  [rotation]
  (let [transform-input @transform-input]
    (set! (.. ^js transform-input -rotation) rotation)))

(defn transform-move
  "Transforms a matrix by the move modifier"
  [matrix modifier]
  (let [vector (dm/get-prop modifier :vector)]
    (set-transform-matrix matrix)
    (set-transform-vector vector)
    (. @instance move)
    (let [transform-output @transform-output]
      (. ^js transform-output -matrix))))

(defn transform-resize
  "Transforms a matrix by the resize modifier"
  [matrix modifier]
  (let [tf     (dm/get-prop modifier :transform)
        tfi    (dm/get-prop modifier :transform-inverse)
        vector (dm/get-prop modifier :vector)
        origin (dm/get-prop modifier :origin)]
    (set-transform-matrix matrix)
    (set-transform-origin origin)
    (set-transform-vector vector)
    (cond (some? tf) (set-transform-transform tf))
    (cond (some? tfi) (set-transform-transform-inverse tfi))
    (set-transform-should-transform (if (or (some? tf) (some? tfi)) 1 0))
    (. @instance resize)
    (let [transform-output @transform-output]
      (. ^js transform-output -matrix))))

(defn transform-rotate
  "Transforms a matrix by the rotation modifier"
  [matrix modifier]
  (let [center   (dm/get-prop modifier :center)
        rotation (dm/get-prop modifier :rotation)]
    (set-transform-matrix matrix)
    (set-transform-center center)
    (set-transform-rotation rotation)
    (. @instance rotate)
    (let [transform-output @transform-output]
      (. ^js transform-output -matrix))))

(defn transform
  [matrix modifier]
  (let [type (dm/get-prop modifier :type)]
    (case type
      :move (transform-move matrix modifier)
      :resize (transform-resize matrix modifier)
      :rotation (transform-rotate matrix modifier))))

(defn add-move-modifier
  [modifier]
  (let [vector (dm/get-prop modifier :vector)]
    (set-transform-vector vector)
    (. ^js @instance addMove (dm/get-prop modifier :order))))

(defn add-resize-modifier
  [modifier]
  (let [tf     (dm/get-prop modifier :transform)
        tfi    (dm/get-prop modifier :transform-inverse)
        vector (dm/get-prop modifier :vector)
        origin (dm/get-prop modifier :origin)]
    (set-transform-origin origin)
    (set-transform-vector vector)
    (cond (some? tf) (set-transform-transform tf))
    (cond (some? tfi) (set-transform-transform-inverse tfi))
    (set-transform-should-transform (if (or (some? tf) (some? tfi)) 1 0))
    (. ^js @instance addResize (dm/get-prop modifier :order))))

(defn add-rotation-modifier
  [modifier]
  (let [center   (dm/get-prop modifier :center)
        rotation (dm/get-prop modifier :rotation)]
    (set-transform-center center)
    (set-transform-rotation rotation)
    (. ^js @instance addRotation (dm/get-prop modifier :order))))

(defn add-modifier
  [modifier]
  (let [type (dm/get-prop modifier :type)]
    (case type
      :move (add-move-modifier modifier)
      :resize (add-resize-modifier modifier)
      :rotation (add-rotation-modifier modifier))))

(defn modifiers->transform
  [modifiers]
  (. ^js @instance reset)
  (doseq [modifier modifiers]
    (add-modifier modifier))
  (js/performance.mark "transform:start")
  (. ^js @instance compute)
  (js/performance.mark "transform:stop")
  (js/performance.measure "transform" "transform:start" "transform:stop")
  (. ^js @transform-output -matrix))

(defn init!
  "Loads WebAssembly module"
  []
  (p/then
   (wasm/load "wasm/transform.release.wasm")
   (fn [asm]
     (js/console.log asm)
     (reset! instance asm)
     (reset! memory asm.memory)
     (init-proxies asm))))
