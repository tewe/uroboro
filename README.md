An Implementation of a Language with Pattern and Copattern Matching
===================================================================

## Usage

	cabal sandbox init
	cabal install --jobs --enable-tests
	cabal test
	cabal run "f(x)"

## Testing

	ghci test/Spec.hs
	:main
	:reload
