(ns yuggoth.locales)

(def dict  
  {:en 
   {:about-title "About"
    :archives-title "Archives"
    :config-title "Initial Configuration"
    :home-title "Home"
    :login-title "Login"
    :profile-title "Profile"
    :welcome-title "Welcome to your new blog"
    :latest-comments-title "Latest comments"
    :upload-title "Upload file"
    :blog-title "Blog title"
    :preview-title "Preview (click to redraw)"
    :recent-posts-title "Recent posts"
    
    :pass-mismatch "entered passwords do not match"
    :admin-required "administrator name is required"
    :blog-title-required "blog title is required"
    
    :host "host"
    :port "port"
    :schema "schema"
    :ssl-port "ssl port"
    :ssl? "enable SSL"
    :initialize "initialize"
    
    :about "about"
    :available-files "available files"
    :file-to-upload "File to upload"
    :file-uploaded "file uploaded successfully"
    :error-uploading "An error has occured while uploading the file: "
    :upload "upload"
    :hide "hide"
    :show "show"
    :name "name"
    :more "more..."
    :css-url "custom css url"
    :password "password"
    :new-password "new password"   
    :confirm-password "confirm password"
    :wrong-password "wrong password"
    :email "email"
    :create "create"
    :user "user"
    :login "login"
    :logout "logout"
    :public "public"
    :edit "edit"
    :delete "delete"
    :next "next"
    :previous "previous"
    :tags "Tags"
    :empty-page "Empty page"
    :nothing-here "Nothing here yet..."
    :other "other"
    :new-post "Create Post"
    :edit-post "Edit Post"
    :content "Body"    
    :post "post"
    :title "Title"
    :title-required "post title is required"
    :comment "comment"
    :comments "comments"
    :commenting-as "Commenting as "
    :anonymous "anonymous"
    :author "author"
    :preview "preview"
    :profile "profile"
    :powered-by " - Powered by: "
    :help "help"
    :captcha "captcha"
    :submit "Submit"
    :update-profile "update profile"
    :profile-updated "profile updated successfully"
    :admin-pass-required "administrator password is required"
    
    :delete-tags "select tags to delete "
    :update-tags "update tags"
    
    :italics-help "*italics*"
    :italics-example "italics"   
    :bold-help "**bold**"
    :bold-example "bold"
    :strikethrough-help "~~foo~~"
    :strikethrough-example "strikethrough"
    :link-help "[link](http://http://example.net/)"
    :link-example "link"
    :superscript-help "super^script"
    :super-example "super"
    :script-example "script"
    :quote-help ">quoted text"
    :quote-example "quoted text"
    :code-help "4 spaces indented code"

    :id-header "ID"
    :time-header "Time"
    :public-header "Published?"
    :actions-header "Actions"
    :slug-header "Slug"
    :tease "Tease"
    :publish-date "Publish Date"
    :markdown-help "Markdown Help"
    :new-page "Create Page"
    :edit-page "Edit Page"
    :new-tag "Create Tag"
    :edit-tag "Edit Tag"
    :blog-settings "Blog Settings"
    :admin-user-name "Admin User Name"

    :asset-header "CSS/js Assets"
    :css-asset-header "CSS Assets"
    :js-asset-header "js Assets"
    :new-css-asset "Create CSS Asset"
    :new-js-asset "Create js Asset"
    :edit-css-asset "Edit CSS Asset"
    :edit-js-asset "Edit js Asset"
    :new-menu-item "Create Menu Link"
    :edit-menu-item "Edit Menu Link"
    :order-header "Insertion Order"
    :path "Path"
    }
   :es 
   {:about-title "Acerca de"
    :archives-title "Archivos"
    :config-title "Configuración Inicial"
    :home-title "Inicio"
    :Login-title "Ingresar"
    :profile-title "Perfil"
    :welcome-title "Bienvenido a nuestro blog"
    :latest-comments-title "Ultimos comentarios"
    :upload-title "Subir Archivo"
    :blog-title "Título del Blog"
    :preview-title "Vista previa (click para actualizar)"
    :recent-posts-title "Entradas recientes"
    
    :pass-mismatch "las contraseñas ingresadas no son iguales"
    :admin-required "se requiere el nombre del administrador"
    :blog-title-required "se requiere el título del blog"
    
    :host "servidor"
    :port "puerto"
    :schema "schema"
    :ssl-port "puerto SSL"
    :ssl? "activar SSL"
    :initialize "inicializar"
    
    :about "acerca de"
    :available-files "archivos disponibles"
    :file-to-upload "Archivo a subir"
    :file-uploaded "Archivo fue subido satisfactoriamente"
    :error-uploading "Ha ocurrido un error mientras se subía el archivo: "
    :upload "subir"
    :hide "ocultar"
    :show "mostrar"
    :name "nombre"
    :more "más..."
    :css-url "url del archivo css personalizado"
    :password "contraseña"
    :new-password "nueva contraseña"
    :confirm-password "confirmar contraseña"
    :wrong-password "contraseña incorrecta"
    :email "email"
    :create "crear"
    :user "usuario"
    :login "ingresar"
    :logout "salir"
    :public "público"
    :edit "editar"
    :delete "borrar"
    :next "siguiente"
    :previous "anterior"
    :tags "tags"
    :empty-page "Página sin contenido"
    :nothing-here "Nada que ver aquí todavía..."
    :other "otro"
    :new-post "Nueva entrada"
    :edit-post "Editar entrada"
    :content "contenido"
    :post "entrada"
    :title "Título"
    :title-required "se requiere el título de la entrada"
    :comment "comentario"
    :comments "comentarios"
    :commenting-as "Comentando como "
    :anonymous "anónimo"
    :author "autor"
    :preview "vista previa"
    :profile "perfil"
    :powered-by " - Creado con: "
    :help "ayuda"
    :captcha "captcha"
    :submit "enviar"
    :update-profile "actualizar perfil"
    :profile-updated "perfil actualizado satisfactoriamente"
    :admin-pass-required "se requiere la contraseña del administrador"
    
    :delete-tags "seleciona tags para borrar "
    :update-tags "actualizar tags"
    
    :italics-help "*italicas*"
    :italics-example "italicas"   
    :bold-help "**negritas**"
    :bold-example "negritas"
    :strikethrough-help "~~foo~~"
    :strikethrough-example "tachado"
    :link-help "[enlace](http://http://example.net/)"
    :link-example "enlace"
    :superscript-help "super^script"
    :super-example "super"
    :script-example "script"
    :quote-help ">texto citado"
    :quote-example "texto citado"
    :code-help "código identado a 4 espacios"

    :id-header "ID"
    :time-header "Fecha"
    :public-header "Publicado?"
    :actions-header "Acciones"
    :slug-header "Slug"
    :tease "Introducción"
    :publish-date "Fecha de publicación"
    :markdown-help "Ayuda de Markdown"
    :new-page "Crear Página"
    :edit-page "Editar Página"
    :new-tag "Crear Tag"
    :edit-tag "Editar Tag"
    :blog-settings "Configuración del Blog"
    :admin-user-name "Nombre del Usuario Administrador"

    :asset-header "CSS/js Assets"
    :css-asset-header "CSS Assets"
    :js-asset-header "js Assets"
    :new-css-asset "Create CSS Asset"
    :new-js-asset "Create js Asset"
    :edit-css-asset "Edit CSS Asset"
    :edit-js-asset "Edit js Asset"
    :new-menu-item "Create Menu Link"
    :edit-menu-item "Edit Menu Link"
    :order-header "Insertion Order"
    :path "Path"
    }})

