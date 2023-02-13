.PHONY: hpack
hpack:
	hpack .

.PHONY: build
build: hpack
	cabal build

.PHONY: build
test: hpack
	cabal test --test-show-details=direct --test-option=--format=checks
