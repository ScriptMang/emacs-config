;;; crafted-completion-config.el --- Crafted Completion Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Setup completion packages. Completion in this sense is more like
;; narrowing, allowing the user to find matches based on minimal
;; inputs and "complete" the commands, variables, etc from the
;; narrowed list of possible choices.

;;; Code:

;;; Vertico
(use-package vertico
  :ensure t
  :init (vertico-mode))
  (elpaca-wait)
  
  ;;submodule to vertico
  (use-package vertico-directory
    :ensure (:host github :repo "minad/vertico")
    :after crafted-deafults-config
    :config  
    (fido-mode -1)
    (fido-vertical-mode -1)
    (icomplete-mode -1)
    (icomplete-vertical-mode -1)
    (customize-set-variable 'vertico-cycle t))

  ;;(require 'vertico-directory)
  ;; Cycle back to top/bottom result when the edge is reached
 

  ;; Turn off the built-in fido-vertical-mode and icomplete-vertical-mode, if
  ;; they have been turned on by crafted-defaults-config, because they interfere
  ;; with this module.
  ;;(with-eval-after-load 'crafted-defaults-config
   (elpaca-wait)


;;; Marginalia
(use-package marginalia
  :ensure t
  :init (marginalia-mode 1) 
  :config
   ;; Configure Marginalia
  (customize-set-variable 'marginalia-annotators
                          '(marginalia-annotators-heavy
                            marginalia-annotators-light
                            nil))
  )
(elpaca-wait)


;;; Consult
;; Since Consult doesn't need to be required, we assume the user wants these
;; setting if it is installed (regardless of the installation method).
(when (locate-library "consult")
  ;; Set some consult bindings
  (keymap-global-set "C-s" 'consult-line)
  (keymap-set minibuffer-local-map "C-r" 'consult-history)

  (setq completion-in-region-function #'consult-completion-in-region))


;;; Orderless
(use-package orderless
  :ensure t
  :config
  ;; Set up Orderless for better fuzzy matching
  (customize-set-variable 'completion-styles '(orderless basic))
  (customize-set-variable 'completion-category-overrides
                          '((file (styles . (partial-completion)))))
 )



;;; Embark
(use-package embark
  :ensure t
  :config
  (keymap-global-set "<remap> <describe-bindings>" #'embark-bindings)
  (keymap-global-set "C-." 'embark-act)
  ;; Use Embark to show bindings in a key prefix with `C-h`
  (setq prefix-help-command #'embark-prefix-help-command))
(elpaca-wait)

  (use-package embark-consult 
    :ensure (:host github :repo "oantolin/embark")	       
    :hook (embark-collect-mode . consult-preview-at-point-mode))


;;; Corfu
(use-package corfu
   :ensure (:wait t)
   :init (global-corfu-mode 1)
   :config
 ;; Setup corfu for popup like completion
  (customize-set-variable 'corfu-cycle t)        ; Allows cycling through candidates
  (customize-set-variable 'corfu-auto t)         ; Enable auto completion
  (customize-set-variable 'corfu-auto-prefix 2)  ; Complete with less prefix keys
 )
(elpaca-wait)

  (unless (display-graphic-p)
    (use-package corfu-terminal
       :ensure (:host github :repo "minad/corfu")
       :init (corfu-terminal-mode +1))) 

     (use-package corfu-popupinfo
       :ensure (:host github :repo "minad/corfu")
       :init (corfu-popupinfo-mode 1)
       :config
       (eldoc-add-command #'corfu-insert)
       (keymap-set corfu-map "M-p" #'corfu-popupinfo-scroll-down)
       (keymap-set corfu-map "M-n" #'corfu-popupinfo-scroll-up)
       (keymap-set corfu-map "M-d" #'corfu-popupinfo-toggle))
(elpaca-wait)
   
;;; Cape

(use-package cape
  :ensure t
  :config
  ;; Setup Cape for better completion-at-point support and more

  ;; Add useful defaults completion sources from cape
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)

  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

  ;; No auto-completion or completion-on-quit in eshell
  (defun crafted-completion-corfu-eshell ()
    "Special settings for when using corfu with eshell."
    (setq-local corfu-quit-at-boundary t
                corfu-quit-no-match t
                corfu-auto nil)
    (corfu-mode))
  (add-hook 'eshell-mode-hook #'crafted-completion-corfu-eshell)

 )

(provide 'crafted-completion-config)
;;; crafted-completion.el ends here
