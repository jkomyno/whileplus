install:
	stack install

build:
	stack build

run:
	stack run whileplus

ghci:
	stack ghci

.PHONY: install build run ghci
