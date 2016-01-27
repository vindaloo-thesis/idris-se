Idris to Serpent back end
-------------------------
Proof-of-concept compiler back end for [Ethereum](https://ethereum.org/) smart contracts written in [Idris](http://www.idris-lang.org/). Compiles down to Serpent. Far from ready for production - output code exceeds gas limit even for very trivial programs. 


Examples
--------
https://github.com/vindaloo-thesis/examples


Build and install
-----------------
Tested with [Idris](https://github.com/idris-lang/Idris-dev/) version 0.10. Idris needs to be built with FFI support.

    cabal install
    cd lib && idris --build ethereum.ipkg -i lib --interface; cd ..
