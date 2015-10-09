run tests with:

    cd {package root}
    
    cabal sandbox init
    cabal configure --enable-tests
    cabal install --only-dependencies --enable-tests

    cabal test --test-option=--color
      or
    cabal test --test-option=--color --show-details=always

