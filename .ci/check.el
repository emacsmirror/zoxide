(setq ci-dir (getenv "CI_PROJECT_DIR")
      ci-script-dir (expand-file-name ".ci" ci-dir))

(load (expand-file-name "deps" ci-script-dir))

(package-install 'package-lint)

(message "* Package Lint: ")

(with-current-buffer (window-buffer (package-lint-current-buffer))
  (message "%s" (buffer-string)))

(message "* Checkdoc: ")

(checkdoc-current-buffer t)

(with-current-buffer (get-buffer checkdoc-diagnostic-buffer)
  (message "%s" (buffer-string)))
