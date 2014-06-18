(ns om-todo.core
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]
            [clojure.string :as string]))

(enable-console-print!)

(def ENTER_KEY 13)

(def app-state
  (atom
    {:title "Omygod"
     :todos [{:text "Pay Sweepstakes entry" :completed true}
             {:text "Cook dinner"}
             {:text "Watch England lose *again*"}]}))

(defcomponent main [app owner]
  (render [_]
          (dom/div
            {:id "todoapp"}
            (om/build header app)
            (dom/section
              {:id "main"}
              (om/build toggle-all (:todos app))
              (om/build todo-list (:todos app)))
            (om/build footer (:todos app)))))

(defcomponent todo-list [todos owner]
  (render [_]
          (dom/ul {:id "todo-list"}
                  (om/build-all todo-view todos))))

(defcomponent todo-view [todo owner]
  (render [_]
          (let [class (cond-> ""
                        (:completed todo) (str "completed"))]
            (dom/li
              {:class class}
              (dom/input
                {:class "toggle" :type "checkbox"
                 :checked (:completed todo)
                 :on-click (fn [e] (om/transact! todo :completed #(not %)))})
              (dom/label (:text todo))
              (dom/button {:class "destroy"})))))

(defcomponent toggle-all [todos owner]
  (render [_]
          (dom/div
            (dom/input
              {:id "toggle-all" :type "checkbox"})
            (dom/label
              {:for "toggle-all"}
              "Mark all as completed") )))

(defcomponent header [app owner]
  (render [_]
          (dom/section
            {:id "header"}
            (dom/h1 (:title app))
            (om/build new-todo-view app))))

(defcomponent new-todo-view [app owner]
  (render [_]
          (dom/input
            {:id "new-todo"
             :placeholder "What needs to be done?"
             :auto-focus true
             :on-key-press #(add-todo % app owner)})))

(defcomponent footer [todos owner]
  (render [_]
          (dom/footer
            {:id "footer"}
            (om/build todos-count todos) )))

(defn todos-count [todos]
  (dom/span
    {:id "todo-count"}
    (dom/strong (remaining todos))
    " items left" ))

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
