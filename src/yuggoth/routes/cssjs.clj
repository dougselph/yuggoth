(ns yuggoth.routes.cssjs
  (:use compojure.core                 
        noir.util.route
        hiccup.core
        hiccup.form 
        hiccup.element 
        hiccup.util 
        yuggoth.config)
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [markdown.core :as markdown] 
            [yuggoth.views.layout :as layout]
            [yuggoth.util :as util]
            [noir.session :as session]
            [noir.response :as resp]   
            [noir.util.cache :as cache]
            [yuggoth.models.db :as db]
            [yuggoth.routes.blog :as br]
            [yuggoth.routes.comments :as comments]))

(defn- asset-delete-form
  [assetid]
  (form-to [:post "/admin/cssjs/delete"]
           (hidden-field "assetid" assetid)
           (submit-button {:class "btn btn-mini"} (s/capitalize (text :delete))))
  )

(defn admin-list-assets
  "Tagslisting in table with links to delete, edit"
  []
  (let [assets (db/cssjs-assets)
        asset_map {:css (vec (sort-by :ins_order (filter #(= (:asset_type %) "css") assets)))
                   :js (vec (sort-by :ins_order (filter #(= (:asset_type %) "js") assets)))}
        atypes [:css :js]]
    (prn (str "asset_map is: " asset_map))
    (prn (str "atypes is: " atypes))
    (prn (str "assets from db query: " assets))
    (layout/admin (text :asset-header)
                  (for [atype atypes]
                    [:span
                     [:row
                      [:h4 (text (keyword (str (name atype) "-asset-header")))]
                      [:div {:align "right"} (link-to (str "/admin/cssjs/new/" (name atype))
                                                      (text (keyword (str "new-" (name atype)
                                                                          "-asset"))))]]
                     [:table {:class "table table-striped"}
                      [:thead
                       [:tr
                        [:th (s/capitalize (text :id-header))]
                        [:th (s/capitalize (text :name))]
                        [:th (s/capitalize (text :path))]
                        [:th (s/capitalize (text :order-header))]
                        [:th {:colspan 2} (s/capitalize (text :actions-header))]]]
                      (for [asset (atype asset_map)]
                        [:tr 
                         [:td (:id asset)]
                         [:td (:name asset)]
                         [:td (:path asset)]
                         [:td (:ins_order asset)]
                         [:td {:width "40px"}
                          (link-to {:class "btn btn-mini"}
                                   (str "/admin/asset/edit/" (:id asset))
                                   (s/capitalize (text :edit)))]
                         [:td (asset-delete-form (:id asset))]])]
                     [:row "&nbsp"]])
     )))

(defn admin-edit-asset
  "cssjs-asset edit form, used for creating and editing cssjs_assets for insertion into page."
  [assetid assettype error]
  (let [new? (if (= assetid :new) true false)
        {:keys [name path asset_type ins_order]} (if new?
                              {:name "" :path "" :asset_type assettype :ins_order "0"}
                              (into {} (db/cssjs-asset (Integer/parseInt assetid))))
        page-title (if new?
                     (text (keyword (str "new-" assettype "-asset")))
                     (text (keyword (str "edit-" assettype "-asset"))))]
    (prn (str "assettype is: " assettype))
    (layout/admin
      page-title
      (when error [:div.error error])
      [:div {:class "row"}
       [:div {:class "span12"}
        [:div {:class "row"}
         [:div {:class "span8"}
          (form-to {:class "form-horizontal"}
                   [:post "/admin/asset/save"]
                   [:div {:class "control-group"}
                    (label {:class "control-label"} "name" (s/capitalize (text :name)))
                    [:div {:class "controls"}
                     (text-field {:tabindex 1 :class "input-xxlarge"} "name" name)]]
                   [:div {:class "control-group"}
                    (label {:class "control-label"} "path" (s/capitalize (text :path)))
                    [:div {:class "controls"}
                     (text-field {:tabindex 2 :class "input-xxlarge" :rows 5}
                                "path" path)]]
                   [:div {:class "control-group" :style "clear:both"}
                    [:div {:class "controls"}
                     (submit-button {:class "btn"} (text :submit))]]
                   (hidden-field "asset_type" asset_type)
                   (hidden-field "assetid" assetid))]]]])))

(defn admin-save-asset
  [assetid name path asset_type ins_order]
  (prn (str "AssetID is: " assetid))
  (if (= assetid "new")
    (let [db_resp (first (db/add-cssjs-asset name path asset_type ins_order))]
      (prn (str "In TRUE branch with assetid of: " assetid))
      (prn (str "db_resp was: " db_resp)))
    (db/update-cssjs-asset assetid name path asset_type ins_order))
  (resp/redirect "/admin/cssjs"))

(defn admin-delete-asset
  [assetid]
  (db/delete-tag assetid)
  (resp/redirect "/admin/cssjs"))

(defroutes cssjs-routes
  (GET "/admin/cssjs" [] (restricted (admin-list-assets)))
  (GET "/admin/cssjs/new/:assettype" [assettype]
       (restricted (admin-edit-asset :new assettype false)))
  (GET "/admin/cssjs/edit/:assetid" [assetid] (restricted (admin-edit-asset assetid false)))
  (POST "/admin/asset/save" [assetid name path asset_type ins_order]
        (restricted (admin-save-asset assetid name path asset_type ins_order)))
  (POST "/admin/cssjs/delete" [assetid] (restricted (admin-delete-asset assetid))))
