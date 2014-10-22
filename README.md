An Implementation of a Language with Pattern and Copattern Matching
===================================================================

## Usage

	cabal sandbox init
	cabal install --jobs --enable-tests
	cabal test
	cabal run samples/prelude.uro

## Documentation

	cabal haddock
	open dist/doc/html/uroboro/index.html

## Testing

	ghci test/Spec.hs
	:main
	:reload
