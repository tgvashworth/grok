(ns grok.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer []]
            [clojure.string :as string]))

(enable-console-print!)

(defn elem [t text]
  (let [e (.createElement js/document t)]
    (set! (. e -textContent) text)
    e))

(defn slice [s start len] (->> s (drop start) (take (or len (count s)))))

(def stringer #(apply str %))

(defn make-selector [source s]
  (let [e (elem "button" (apply str (apply slice source s)))]
    (.appendChild js/document.body e)))

(def app-state
  (atom {:source "Hello, world. This is some arbitrary text pertaining to the grokking of said text, and such like and so forth."
         :selections [[0, 13] [14, 10]]
         :active-selection 1}))

(defn editable [state owner {:keys [target className] :as  opts}]
  (reify
    om/IInitState
    (init-state [_]
                {:text (target state)})
    om/IRenderState
    (render-state [_ {:keys [text]}]
            (dom/span #js {:contentEditable "true"
                         :className className
                          :onInput
                            (fn [e]
                              (println e)
                              (om/transact! state target
                                            (fn [_] (.. e -target -textContent))))}
                     text))))

(defn selector-view [state owner]
  (reify
    om/IRender
    (render [_]
            (let [selections (:selections state)
                  asi (:active-selection state)
                  source (:source state)]
              (apply dom/div #js {:className "selector-view"}
                     (map-indexed (fn [i selection]
                                    (dom/div #js {:onClick (fn [e]
                                                                (om/transact! state :active-selection (fn [_] i)))}
                                                (stringer (apply slice source selection))))
                                  selections))))))

(defn source-view [state owner]
  (reify
    om/IInitState
    (init-state [_]
      (let [selections (:selections state)
            asi (:active-selection state)
            source (:source state)
            selection (if asi (selections asi) [0, (count source)])
            [start length] selection
            before (stringer (slice source 0 start))
            highlighted (stringer (slice source start length))
            after (stringer (slice source (+ start length) (count source)))]
        {:before before
         :highlighted highlighted
         :after after}))
    om/IRenderState
    (render-state [_ {:keys [before highlighted after] :as local-state}]
      (let []
        (dom/div #js {:className "source-view"}
                 (om/build editable local-state {:opts {:target :before}})
                 (om/build editable local-state {:opts {:target :highlighted :className "source-selection"}})
                 (om/build editable local-state {:opts {:target :after}}))))))

(defn app-view [app-state owner]
  (reify
    om/IRender
    (render [_]
            (dom/div nil
                     (om/build source-view app-state)
                     (om/build selector-view app-state)))))

(om/root app-view app-state {:target (. js/document (getElementById "app"))})
