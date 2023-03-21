(ns app.wasm.resize
  (:require 
   [app.util.wasm :as wasm]
   [app.util.wasm.types :as types]
   [promesa.core :as p]))

(defonce instance (atom nil))
(defonce memory (atom nil))
(defonce resize-input (atom nil))
(defonce resize-output (atom nil))

(defn- init-proxies
  [asm]
  (js/console.log "init resize proxies")
  (reset! resize-input (types/from asm "resizeInput" "ResizeInput"))
  (reset! resize-output (types/from asm "resizeOutput" "ResizeOutput")))

(defn- resize-get-handler
  [handler]
  (case handler
    :right 0
    :bottom-right 1
    :bottom 2
    :bottom-left 3
    :left 4
    :top-left 5
    :top 6
    :top-right 7))

(defn resize-start
  [handler initial]
  (let [resize-input @resize-input]
    (set! (.. ^js resize-input -start -x) (:x initial))
    (set! (.. ^js resize-input -start -y) (:y initial))
    (set! (.. ^js resize-input -handler) (resize-get-handler handler))))

(defn resize-update-transforms
  [shape]
  (let [resize-input @resize-input]
    (set! (.. ^js resize-input -transform -a) (:a (:transform shape)))
    (set! (.. ^js resize-input -transform -b) (:b (:transform shape)))
    (set! (.. ^js resize-input -transform -c) (:c (:transform shape)))
    (set! (.. ^js resize-input -transform -d) (:d (:transform shape)))
    (set! (.. ^js resize-input -transform -e) (:e (:transform shape)))
    (set! (.. ^js resize-input -transform -f) (:f (:transform shape)))
    (set! (.. ^js resize-input -transformInverse -a) (:a (:transform-inverse shape)))
    (set! (.. ^js resize-input -transformInverse -b) (:b (:transform-inverse shape)))
    (set! (.. ^js resize-input -transformInverse -c) (:c (:transform-inverse shape)))
    (set! (.. ^js resize-input -transformInverse -d) (:d (:transform-inverse shape)))
    (set! (.. ^js resize-input -transformInverse -e) (:e (:transform-inverse shape)))
    (set! (.. ^js resize-input -transformInverse -f) (:f (:transform-inverse shape)))))

(defn resize-update
  [shape point point-snap lock? center?]
  (let [{:keys [x y width height]} (:selrect shape)
        {:keys [rotation]} shape
        resize-input @resize-input]
    (set! (.. ^js resize-input -rotation) (or rotation 0))
    (set! (.. ^js resize-input -shouldLock) (if lock? 1 0))
    (set! (.. ^js resize-input -shouldCenter) (if center? 1 0))
    (set! (.. ^js resize-input -selRect -position -x) x)
    (set! (.. ^js resize-input -selRect -position -y) y)
    (set! (.. ^js resize-input -selRect -size -x) width)
    (set! (.. ^js resize-input -selRect -size -y) height)
    (set! (.. ^js resize-input -current -x) (:x point))
    (set! (.. ^js resize-input -current -y) (:y point))
    (set! (.. ^js resize-input -snap -x) (:x point-snap))
    (set! (.. ^js resize-input -snap -y) (:y point-snap))
    (set! (.. ^js resize-input -shouldTransform) (if (some? (:transform shape)) 1 0))
    (when (some? (:transform shape))
      (resize-update-transforms shape))
    (. @instance resize)))

(defn init!
  "Loads WebAssembly module"
  []
  (p/then
   (wasm/load "wasm/resize.release.wasm")
   (fn [asm]
     (js/console.log asm)
     (reset! instance asm)
     (reset! memory asm.memory)
     (init-proxies asm))))
