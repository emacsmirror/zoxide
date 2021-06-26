(setq ci-dir (getenv "CI_PROJECT_DIR")
      gitlab-dir (expand-file-name ".gitlab" ci-dir))

(load (expand-file-name "deps" gitlab-dir))

(package-install 'package-lint)

(message "* Package Lint: ")

(with-current-buffer (window-buffer (package-lint-current-buffer))
  (message "%s" (buffer-string)))

(message "* Checkdoc: ")

(checkdoc-current-buffer t)

(with-current-buffer (get-buffer checkdoc-diagnostic-buffer)
  (message "%s" (buffer-string)))
