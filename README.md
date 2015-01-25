An Implementation of a Language with Pattern and Copattern Matching
===================================================================

## Usage

	cabal sandbox init
	cabal install --jobs --enable-tests
	cabal test
	cabal run -- samples/fib.uro -- "fib().head()"

## Documentation

	cabal haddock
	open dist/doc/html/uroboro/index.html

## Testing

	cabal exec -- ghci test/Spec.hs
	:main
	:reload

	cabal exec -- ghci -Wall test/Uroboro/ParserSpec.hs
	hspec spec
