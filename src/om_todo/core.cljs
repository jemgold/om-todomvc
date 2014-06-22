(ns om-todo.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
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
  (init-state [_]
              {:chans {:delete (chan)}})
  (will-mount [_]
              (let [delete (om/get-state owner [:chans :delete])]
                (go-loop []
                      (let [todo (<! delete)]
                        (om/transact! app :todos
                                      (fn [xs] (vec (remove #(= todo %) xs)))))
                      (recur))))
  (render-state [_ {:keys [chans]}]
                (dom/div
                  {:id "todoapp"}
                  (om/build header app)
                  (dom/section
                    {:id "main"}
                    (om/build toggle-all-button (:todos app))
                    (om/build todo-list (:todos app) {:init-state chans}))
                  (om/build footer (:todos app)))))

(defcomponent todo-list [todos owner]
  (render-state [_ chans]
          (dom/ul {:id "todo-list"}
                  (om/build-all todo-view todos {:init-state chans}))))

(defcomponent todo-view [todo owner]
  (render-state [_ {:keys [delete]}]
          (let [class (cond-> ""
                        (:completed todo) (str "completed"))]
            (dom/li
              {:class class}
              (dom/input
                {:class "toggle" :type "checkbox"
                 :checked (:completed todo)
                 :on-click #(toggle-completed todo)})
              (dom/label (:text todo))
              (dom/button
                {:class "destroy"
                 :on-click (fn [e] (put! delete @todo))})))))

(defcomponent toggle-all-button [todos owner]
  (render [_]
          (dom/div
            (dom/input
              {:id "toggle-all" :type "checkbox"
               :on-click #(toggle-all todos)
               :checked (= (remaining todos) 0)})
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
             :on-key-press #(add-todo app owner %)})))

(defcomponent footer [todos owner]
  (render [_]
          (dom/footer
            {:id "footer"}
            (om/build todos-count todos) )))

;; Events

(defn add-todo [app owner event]
  (let [new-todo (.-value (om/get-node owner) )]
    (when
      (and (= (.-keyCode event) ENTER_KEY)
           (not (string/blank? new-todo)))
      (om/transact! app :todos #(conj % {:text new-todo}))
      )))

(defn toggle-completed [todo]
  (om/transact! todo :completed #(not %)))

(defn toggle-all [todos]
  (om/transact! todos
                (fn [todos] (vec (map #(assoc % :completed (not (:completed %))) todos)))))

;; Helpers

(defn remaining [todos]
  (->> todos
       (filter (fn [todo] (not (:completed todo) )))
       count))

(defn inflect [word count]
  (if (= count 1)
    word
    (str word "s")))

(defn todos-count [todos]
  (dom/span
    {:id "todo-count"}
    (dom/strong (remaining todos))
    " "
    (inflect "item" (remaining todos))
    " left"))

;; Let's go!

(om/root
  main
  app-state
  {:target (. js/document (getElementById "app"))})
