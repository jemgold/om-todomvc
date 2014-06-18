(ns om-todo.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as string]))

(enable-console-print!)

(def ENTER_KEY 13)

(def app-state
  (atom
    {:title "Omygod"
     :todos [{:text "Pay Sweepstakes entry" :completed true}
             {:text "Cook dinner"}
             {:text "Watch England lose *again*"}]
     }))

(defn main [app owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js {:id "todoapp"}
               (om/build header app)
               (dom/section #js {:id "main"}
                 (om/build toggle-all (:todos app))
                 (om/build todo-list (:todos app)) )
               (om/build footer (:todos app))
                 ))))

(defn todo-list [todos _]
  (reify
    om/IRender
    (render [_]
      (apply dom/ul #js {:id "todo-list"}
             (om/build-all todo-view todos)) ) ))

(defn todo-view [todo owner]
  (reify
    om/IRender
    (render [this]
      (let [class (cond-> ""
                    (:completed todo) (str "completed"))]
        (dom/li #js {:className class}
                (dom/input
                  #js {:className "toggle" :type "checkbox"
                       :checked (:completed todo)
                       :onClick (fn [e] (om/transact! todo :completed #(not %)))})
                (dom/label nil (:text todo) )
                (dom/button #js {:className "destroy"} nil)
                )))))

(defn toggle-all [todos _]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
               (dom/input
                 #js {:id "toggle-all" :type "checkbox"})
               (dom/label
                 #js {:htmlFor "toggle-all"}
                 "Mark all as completed") ))))

(defn header [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/section #js {:id "header"}
                   (dom/h1 nil (:title app))
                   (om/build new-todo-view app)))))

(defn new-todo-view [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/input #js {:id "new-todo"
                      :placeholder "What needs to be done?"
                      :autoFocus true
                      :onKeyPress #(add-todo % app owner)  }
                      nil ) )))

(defn footer [todos _]
  (reify
    om/IRender
    (render [_]
      (dom/footer #js {:id "footer"}
                  (om/build todos-count todos) ))))

(defn todos-count [todos]
  (dom/span #js {:id "todo-count"}
            (dom/strong nil (remaining todos))
            " items left" ) )

(defn remaining [todos]
  (->> todos
       (filter (fn [todo] (not (:completed todo) )))
       count))

(defn add-todo [event app owner]
  (let [new-todo (.-value (om/get-node owner) )]
    (when
      (and (= (.-keyCode event) ENTER_KEY)
           (not (string/blank? new-todo)))
      (om/transact! app :todos #(conj % {:text new-todo}))
      )))

(om/root
  main
  app-state
  {:target (. js/document (getElementById "app"))})
