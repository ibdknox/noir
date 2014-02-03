(ns noir.content.directory
  "Functions dealing with displaying directory contents."
  (:use [noir.core])
  (:require [noir.statuses :as statuses]
            [hiccup.core :as hcore]
            [hiccup.element :as helement])
  (:import java.io.File))


(defn- vetted-path [root path]
  "If the file exists and is either in the root path or is the root path,
   return true.  Otherwise, return false."
  (let [canonical-root (-> root File. .getCanonicalFile .getPath)
        file (-> path File. .getCanonicalFile)]
    (if (and (.exists file)
             (or (= (.getParent file) canonical-root) (= (.getCanonicalPath file) canonical-root)))
      file
      nil)))

(defn- render-file-listing [dir]
  "Show a list of all files in the given directory."
  (let [paths (remove #(.isDirectory %1) (.listFiles dir))
        filenames (map #(.getName %1) paths)]
    (hcore/html [:ul
           (map #(vector :li (helement/link-to (str "./" %1) %1))
                filenames)])))

(defn render-static-dir
  "Render a directory at rootdir named fname."
  [rootdir fname]
  (let [path (str rootdir File/separatorChar fname)
        file (vetted-path rootdir path)]
    (cond
     (nil? file) (statuses/get-page 404)
     (.isDirectory file) (render-file-listing file)
     :else (slurp (.getCanonicalPath file)))))

(defmacro defstaticdir
  "Create a static directory viewer.  First argument is the path to the directory on the
   local filesystem.  The second argument is the URL with no trailing slash.  For instance:

   (defstaticdir \"/some/path/to/templates\" \"/myfiles\")

   Viewing the path with no file will show a listing of all files."
  [rootdir baseurl]
  (let [farg [:get [(str baseurl "/:fname") :fname #".*"]] ]
    `(defpage ~farg {:keys [~'fname]}
       (render-static-dir ~rootdir ~'fname))))
