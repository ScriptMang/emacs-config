(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'aggressive t)
 '(bookmark-save-flag 1)
 '(completion-category-overrides '((file (styles partial-completion))))
 '(completion-cycle-threshold 3)
 '(completion-styles '(orderless basic))
 '(completions-detailed t)
 '(corfu-auto t)
 '(corfu-auto-prefix 2)
 '(corfu-cycle t)
 '(custom-safe-themes
   '("da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "dc2e1b0abb9a5d2033f6d881618932dcdb9af7633d8fa44336f9c9a3484379bd" "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434" "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd" "91c008faf603a28d026957120a5a924a3c8fff0e12331abf5e04c0e9dd310c65" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7" "e8ceeba381ba723b59a9abc4961f41583112fc7dc0e886d9fc36fa1dc37b4079" "6e33d3dd48bc8ed38fd501e84067d3c74dfabbfc6d345a92e24f39473096da3f" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" "6f1f6a1a3cff62cc860ad6e787151b9b8599f4471d40ed746ea2819fcd184e1a" "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641" "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b" "88cb0f9c0c11dbb4c26a628d35eb9239d1cf580cfd28e332e654e7f58b4e721b" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb" "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e" "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851" "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "242f33ba517c05f45e075d8ed3d13c0a7b7d1392e0c95d66830029e561607085" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0" "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738" "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "a6920ee8b55c441ada9a19a44e9048be3bfb1338d06fc41bce3819ac22e4b5a1" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "a3010c151dc4f42d56dec26a85ae5640afc227bece71d058e394667718b66a49" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "e871f44a640f98523876f77dccdbf0e20747ca7e111f9f147fe23c9d5f4937c1" "2fcd2b44646836f0f4acbd42a13fa85123dac744628f0105a5e9f0f7dbbc936a" "0018c218377a0f234066cd01eb9b636d3739b0b614c7b2c0b8e37a306b7bf8ef" "714394050e703db8a773ed350ca6f9cb6636d4bf2e348514804a48929aafc762" "45e409674661674c12070af5f8ef71741599eeb9fccd84557f1b822509f3b100" "e6b0ec96166bb3bb2843d83e56c0292308aab10ee5b79fb921d16ad2dbea5d5f" "2459d6e7e96aefaed9cebaf7fde590f64e76c96f48632d8310cfea5d10ec2bb1" "e1990eeea39781f009b7f4634ca52a770d05bb7ce423a8fbbcd8a4f327efb626" "b6c43bb2aea78890cf6bd4a970e6e0277d2daf0075272817ea8bb53f9c6a7f0a" "51f3fb81f9233280cb28ee3023e43e82c9307d59d158626881ca14f964d2abeb" "000ae191922c662e7f89eae84932415f9d7f5c3045b167b3375c2ad9b62a0c78" "2cccba1da519e6764ba3e2c5b443d66f951f31a56aa1494fca6f93fe34d27e51" "9312a0692efc799e797c6689b3216c45d80e460184f666fffdb6dab65d8d4947" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "611ef0918b8b413badb8055089b5499c1d4ac20f1861efba8f3bfcb36ad0a448" "69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8" default))
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eshell-scroll-to-bottom-on-input 'this t)
 '(fancy-splash-image "~/crafted-emacs/system-crafters-logo.png")
 '(fast-but-imprecise-scrolling t)
 '(global-auto-revert-non-file-buffers t)
 '(ibuffer-movement-cycle nil)
 '(ibuffer-old-time 24)
 '(kill-do-not-save-duplicates t)
 '(load-prefer-newer t t)
 '(marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil) t)
 '(mouse-wheel-progressive-speed nil)
 '(package-archive-priorities
   '(("gnu" . 99)
     ("nongnu" . 80)
     ("stable" . 70)
     ("melpa" . 0)))
 '(package-selected-packages
   '(drag-stuff crux casual activities xah-fly-keys yasnippet-snippets emmet-mode web-mode lsp-ui project-treemacs consult-project-extra nerd-icons-corfu consult-lsp magit-todos go-tag solaire-mode forge htmltagwrap dumb-jump lsp-origami origami diminish dashboard nerd-icons nerd-icons-ibuffer treemacs-nerd-icons nerd-icons-dired treemacs-magit treemacs-icons-dired nerd-icons-completion doom-modeline doom-themes yasnippet helpful dap-mode go-dlv flycheck-golangci-lint flycheck-indicator autothemer ample-theme jazz-theme nimbus-theme modus-themes spacemacs-theme flycheck go-mode which-key eglot lsp-mode f ace-window keycast devdocs ef-themes vertico orderless marginalia embark-consult embark corfu-terminal corfu consult cape magit))
 '(scroll-conservatively 101)
 '(scroll-margin 0)
 '(scroll-preserve-screen-position t)
 '(switch-to-buffer-in-dedicated-window 'pop)
 '(switch-to-buffer-obey-display-actions t)
 '(tab-always-indent 'complete)
 '(vertico-cycle t)
 '(xref-show-definitions-function 'xref-show-definitions-completing-read))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
