An Implementation of a Language with Pattern and Copattern Matching
===================================================================

## Usage

	cabal sandbox init
	cabal install --jobs --enable-tests
	cabal test
	cabal run samples/prelude.uro

## Testing

	ghci test/Spec.hs
	:main
	:reload
