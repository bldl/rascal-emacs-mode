byte-compile:
	emacs -Q -L . -batch -f batch-byte-compile rascal-mode-cc.el rascal-mode-prelude.el

-include local.mk

readme :
	org-ruby README.org > README.html

dev-install :
	gem install org-ruby
