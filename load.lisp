(pushnew (uiop:getcwd) ql:*local-project-directories*)
(ql:quickload :flow)
(asdf:load-system :flow)
