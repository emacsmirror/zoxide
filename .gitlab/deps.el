(require 'xdg)
(setq ci-dir (getenv "CI_PROJECT_DIR")
      build-dir (expand-file-name ".build" ci-dir)
      cache-dir (xdg-cache-home))

(message (concat "ci dir: " ci-dir))
(message (concat "build dir: " build-dir))
(message (concat "cache dir: " cache-dir))

(setq-default package-user-dir (expand-file-name "emacs/packages" cache-dir))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)
