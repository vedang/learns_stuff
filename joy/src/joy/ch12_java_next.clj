(ns joy.ch12-java-next
  (:import [com.sun.net.httpserver HttpHandler HttpServer]
           java.net.InetSocketAddress))

(def OK java.net.HttpURLConnection/HTTP_OK)

(defn respond
  ([exchange body]
   (respond identity exchange body))
  ([around exchange body]
   (.sendResponseHeaders exchange OK 0)
   (with-open [resp (around (.getResponseBody exchange))]
     (.write resp (.getBytes body)))))

(defn new-server
  [port path handler]
  (doto (HttpServer/create (InetSocketAddress. port) 0)
    (.createContext path handler)
    (.setExecutor nil)
    (.start)))


(defn default-handler
  [txt]
  (proxy [HttpHandler]
      []
    (handle [exchange]
      (respond exchange txt))))

(comment
  (def p (default-handler "Hello Cleveland"))
  (def server (new-server 8123 "/joy/hello" p))
  (update-proxy p
                {"handle" (fn [this exchange]
                            (respond exchange (str "This is : " this)))}))

(defn echo-handler
  )
