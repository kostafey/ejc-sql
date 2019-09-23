version:
	emacs --version

test : version
	cask exec emacs --script test/ejc-tests.el

elpa:
ifeq ($(OS),Windows_NT)
	if exist $(USERPROFILE)\.cask rmdir $(USERPROFILE)\.cask /s /q
	curl -fsSkL https://raw.github.com/cask/cask/master/go | python
else ifeq ($(wildcard $(HOME)/.cask/bin/),)
	rm -rf ~/.cask
	curl -fsSkL https://raw.github.com/cask/cask/master/go | python
endif
	cask install
	cask update
