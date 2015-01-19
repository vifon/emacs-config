(define-skeleton cmake-root-skeleton
  "" ""
  (let* ((path (split-string default-directory "/"))
         (curdir (nth (- (length path) 2) path))
         (name (if (string= curdir "src")
                   (nth (- (length path) 3) path)
                   curdir)))
    (concat
     "cmake_minimum_required(VERSION 2.8)\n"
     "project("name")\n"
     "add_subdirectory(src)\n"
     )) _ )

(define-skeleton cmake-skeleton
  "" ""
  (let* ((path (split-string default-directory "/"))
         (curdir (nth (- (length path) 2) path))
         (name (if (string= curdir "src")
                   (nth (- (length path) 3) path)
                   curdir)))
    (concat
     "set("name"_SOURCES\n  "(mapconcat 'identity (merge 'list (file-expand-wildcards "*.c") (file-expand-wildcards "*.cpp") 'string-lessp) "\n  ")"\n  )\n\n"
     "set("name"_HEADERS\n  "(mapconcat 'identity (merge 'list (file-expand-wildcards "*.h") (file-expand-wildcards "*.hpp") 'string-lessp) "\n  ")"\n  )\n\n"

     "add_executable("name" ${"name"_SOURCES})\n"
     "#target_link_libraries("name")\n"
     )) _ )

(define-skeleton cmake-glob-skeleton
  "" ""
  (let* ((path (split-string default-directory "/"))
         (curdir (nth (- (length path) 2) path))
         (name (if (string= curdir "src")
                   (nth (- (length path) 3) path)
                   curdir)))
    (concat
     "file(GLOB "name"_SOURCES *.cpp *.c)\n"
     "file(GLOB "name"_HEADERS *.hpp *.h)\n"
     )))

(define-skeleton cmake-files-skeleton
  "" ""
  (let* ((path (split-string default-directory "/"))
         (curdir (nth (- (length path) 2) path))
         (name (if (string= curdir "src")
                   (nth (- (length path) 3) path)
                   curdir)))
    (concat
     "set("name"_SOURCES\n  "(mapconcat 'identity (merge 'list (file-expand-wildcards "*.c") (file-expand-wildcards "*.cpp") 'string-lessp)"\n  ")"\n  )\n\n"
     "set("name"_HEADERS\n  "(mapconcat 'identity (merge 'list (file-expand-wildcards "*.h") (file-expand-wildcards "*.hpp") 'string-lessp)"\n  ")"\n  )\n\n"
     )))

(define-skeleton cmake-qt-skeleton
  "" ""
  (let* ((path (split-string default-directory "/"))
         (curdir (nth (- (length path) 2) path))
         (name (if (string= curdir "src")
                   (nth (- (length path) 3) path)
                   curdir)))
    (concat
     "find_package(Qt4 REQUIRED)\n"
     "set("name"_SOURCES\n  "(mapconcat 'identity (merge 'list (file-expand-wildcards "*.c") (file-expand-wildcards "*.cpp") 'string-lessp)"\n  ")"\n  )\n\n"
     "set("name"_HEADERS\n  "(mapconcat 'identity (merge 'list (file-expand-wildcards "*.h") (file-expand-wildcards "*.hpp") 'string-lessp)"\n  ")"\n  )\n\n"
     "Qt4_wrap_cpp("name"_HEADERS_MOC ${"name"_HEADERS})\n"
     "include(${QT_USE_FILE})\n"
     "add_definitions(${QT_DEFINITIONS})\n\n"

     "add_executable("name" ${"name"_SOURCES}\n  ${"name"_HEADERS_MOC})\n"
     "target_link_libraries("name" ${QT_LIBRARIES})\n"
     )) _ )

(define-skeleton cmake-qt5-skeleton
  "" ""
  (let* ((path (split-string default-directory "/"))
         (curdir (nth (- (length path) 2) path))
         (name (if (string= curdir "src")
                   (nth (- (length path) 3) path)
                   curdir)))
    (concat
     "set(CMAKE_AUTOMOC ON)\n"
     "\n"
     "find_package(Qt5Core REQUIRED)\n"
     "find_package(Qt5Gui REQUIRED)\n"
     "find_package(Qt5Widgets REQUIRED)\n"
     "\n"
     "set("name"_SOURCES\n  "(mapconcat 'identity (merge 'list (file-expand-wildcards "*.c") (file-expand-wildcards "*.cpp") 'string-lessp)"\n  ")"\n  )\n\n"
     "set("name"_HEADERS\n  "(mapconcat 'identity (merge 'list (file-expand-wildcards "*.h") (file-expand-wildcards "*.hpp") 'string-lessp)"\n  ")"\n  )\n"
     "\n"
     "add_executable("name" ${"name"_SOURCES})\n"
     "#target_link_libraries("name")\n"
     "qt5_use_modules("name" Core Guid Widgets)\n"
     )) _ )

(define-auto-insert "/CMakeLists\\.txt$" 'cmake-skeleton)
