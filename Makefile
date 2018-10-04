version:
	emacs --version

test : version
	cask exec emacs --script test/ejc-tests.el

elpa:
ifeq ($(wildcard $(HOME)/.cask/bin/),)
	rm -rf ~/.cask
	curl -fsSkL https://raw.github.com/cask/cask/master/go | python
endif
	cask install
	cask update
